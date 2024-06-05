namespace Fun.Result

#if !FABLE_COMPILER

open System.Threading.Tasks


type TaskResult<'Success, 'Failure> = Task<Result<'Success, 'Failure>>


[<RequireQualifiedAccess>]
module TaskResult =
    let inline map f (x: TaskResult<_, _>) : TaskResult<_, _> = Task.map (Result.map f) x

    let inline mapError f (x: TaskResult<_, _>) : TaskResult<_, _> = Task.map (Result.mapError f) x

    let inline retn x : TaskResult<_, _> = Ok x |> Task.FromResult


    let inline bind (f: _ -> TaskResult<_, _>) (x: TaskResult<_, _>) : TaskResult<_, _> = task {
        let! result = x
        match result with
        | Ok x -> return! f x
        | Error x -> return (Error x)
    }

    let inline bindError (f: _ -> Task<_>) (x: TaskResult<_, _>) : TaskResult<_, _> = task {
        let! result = x
        match result with
        | Ok x -> return Ok x
        | Error x ->
            let! newErr = f x
            return (Error newErr)
    }

    let inline ofSuccess x : TaskResult<_, _> = Ok x |> Task.FromResult

    let inline ofError x : TaskResult<_, _> = Error x |> Task.FromResult

    let inline ofAsync (x: Async<_>) : TaskResult<_, _> = task {
        let! result = x
        return Ok result
    }

    let inline ofTask (x: Task<_>) : TaskResult<_, _> = x |> Task.map Ok

    let inline ofEmptyTask (x: Task) : TaskResult<_, _> = task {
        do! x
        return Ok()
    }

    let inline bindTask f x = x |> ofTask |> bind f

    let inline mapTask f x = x |> ofTask |> map f


[<AutoOpen>]
module TaskResultComputationExpression =
    type TaskResultBuilder() =
        member inline __.Return(x) = TaskResult.retn x

        member inline __.Bind(x: TaskResult<_, _>, [<InlineIfLambda>] f) = TaskResult.bind f x
        member inline __.Bind(x: Result<_, _>, [<InlineIfLambda>] f) = TaskResult.bind f (Task.retn x)

        member inline __.ReturnFrom(x) = x
        member inline __.Delay(f) = f
        member inline __.Run([<InlineIfLambda>] f) = f ()

        member inline this.Zero() = this.Return()

        member this.While(guard, body: unit -> TaskResult<_, _>) =
            if not (guard ()) then
                this.Zero()
            else
                this.Bind(body (), (fun () -> this.While(guard, body)))

        member inline this.TryWith(body, handler) = task {
            try
                return! this.ReturnFrom(body ())
            with e ->
                return! handler e
        }

        member inline this.TryFinally(body, compensation) = task {
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
        member inline this.Combine(a: TaskResult<_, _>, b) = this.Bind(a, (fun () -> b ()))

    let taskResult = TaskResultBuilder()

#endif
