namespace Fun.Result

[<RequireQualifiedAccess>]
module Async =
    let inline map f xA = async {
        let! x = xA
        return f x
    }

    let inline mapOption f xA =
        xA
        |> map (
            function
            | Some x' -> x' |> f |> Some
            | None -> None
        )

    let inline retn x = async.Return x

    let inline apply fA xA = async {
        let! fChild = Async.StartChild fA
        let! x = xA
        let! f = fChild
        return f x
    }

    let inline bind f xA = async.Bind(xA, f)

    let inline bindOption f xA =
        xA
        |> bind (
            function
            | Some x -> x |> f
            | None -> None |> retn
        )

    let inline pass f =
        map (fun x ->
            f
            x
        )


    let sequence<'T> (asyncs: Async<'T> seq) =
        let state: 'T seq = [] |> Seq.ofList
        asyncs
        |> Seq.fold
            (fun s x -> async {
                let! s' = s
                let! x' = x
                return Seq.append [ x' ] s'
            })
            (async { return state })
        |> map Seq.rev


[<AutoOpen>]
module AsyncOptionComputationExpression =

    type AsyncOptionBuilder() =
        member inline __.Return(x) = x |> Some |> Async.retn
        member inline __.ReturnFrom(x) = x
        member inline __.Bind(x, f) = async {
            match! x with
            | Some x -> return! f x
            | None -> return None
        }
        member inline this.Zero() = this.Return()
        member inline __.Delay(f) = f
        member inline __.Run(f) = f ()

        member this.While(guard, body) =
            if not (guard ()) then
                this.Zero()
            else
                this.Bind(body (), (fun () -> this.While(guard, body)))

        member inline this.TryWith(body, handler) = async {
            try
                return! this.ReturnFrom(body ())
            with e ->
                return! handler e
        }

        member inline this.TryFinally(body, compensation) = async {
            try
                return! this.ReturnFrom(body ())
            finally
                compensation ()
        }

        member inline this.Using(disposable: #System.IDisposable, body) =
            let body' = fun () -> body disposable
            this.TryFinally(
                body',
                fun () ->
                    match disposable with
                    | null -> ()
                    | disp -> disp.Dispose()
            )

        member inline this.For(sequence: seq<_>, body) =
            this.Using(sequence.GetEnumerator(), (fun enum -> this.While(enum.MoveNext, this.Delay(fun () -> body enum.Current))))
        member inline this.Combine(a, b) = this.Bind(a, (fun () -> b ()))

    let asyncOption = AsyncOptionBuilder()
