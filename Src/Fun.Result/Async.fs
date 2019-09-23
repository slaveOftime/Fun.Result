namespace Fun.Result

[<RequireQualifiedAccess>]
module Async =
    let map f xA = async { let! x = xA
                           return f x }

    let mapOption f xA =
        xA
        |> map (function
               | Some x' -> x' |> f |> Some
               | None -> None)

    let retn x = async.Return x

    let apply fA xA =
        async {
            let! fChild = Async.StartChild fA
            let! x = xA
            let! f = fChild
            return f x }

    let bind f xA = async.Bind(xA, f)

    let bindOption f xA =
        xA
        |> bind (function
               | Some x -> x |> f
               | None -> None |> retn)

    let pass f = map (fun x -> f; x)


    let sequence<'T> (asyncs: Async<'T> seq) =
        let state: 'T seq = [] |> Seq.ofList
        asyncs
        |> Seq.fold
            (fun s x -> 
                async {
                    let! s' = s
                    let! x' = x
                    return Seq.append [x'] s'
                })
            (async { return state })
        |> map Seq.rev


[<AutoOpen>]
module AsyncOptionComputationExpression = 

    type AsyncOptionBuilder() = 
        member __.Return(x) = x |> Some |> Async.retn
        member __.ReturnFrom(x) = x
        member __.Bind(x, f) =
            async {
                match! x with
                | Some x -> return! f x
                | None -> return None
            }
        member this.Zero() = this.Return()
        member __.Delay(f) = f
        member __.Run(f) = f()

        member this.While(guard, body) =
            if not (guard()) then this.Zero()
            else this.Bind(body(), fun () -> this.While(guard, body))

        member this.TryWith(body, handler) =
            try
                this.ReturnFrom(body())
            with e -> handler e

        member this.TryFinally(body, compensation) =
            try
                this.ReturnFrom(body())
            finally
                compensation()

        member this.Using(disposable : #System.IDisposable, body) =
            let body' = fun () -> body disposable
            this.TryFinally(body',
                            fun () ->
                                match disposable with
                                | null -> ()
                                | disp -> disp.Dispose())

        member this.For(sequence : seq<_>, body) =
            this.Using
                (sequence.GetEnumerator(),
                 fun enum ->
                     this.While
                         (enum.MoveNext, this.Delay(fun () -> body enum.Current)))
        member this.Combine(a, b) = this.Bind(a, fun () -> b())

    let asyncOption = AsyncOptionBuilder()
