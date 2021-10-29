namespace Fun.Result

#if !FABLE_COMPILER

open System.Threading.Tasks


type TaskResult<'Success, 'Failure> = Task<Result<'Success, 'Failure>>


[<RequireQualifiedAccess>]
module TaskResult =
    let map f (x: TaskResult<_, _>): TaskResult<_, _> =
        Task.map (Result.map f) x

    let mapError f (x: TaskResult<_, _>): TaskResult<_, _> =
        Task.map (Result.mapError f) x

    let retn x: TaskResult<_, _> = Ok x |> Task.FromResult


    let bind (f: _ -> TaskResult<_, _>) (x: TaskResult<_, _>): TaskResult<_, _> =
        task {
            let! result = x
            match result with
            | Ok x -> return! f x
            | Error x -> return (Error x)
        }

    let bindError (f: _ -> Task<_>) (x: TaskResult<_, _>): TaskResult<_, _> =
        task {
            let! result = x
            match result with
            | Ok x -> return Ok x
            | Error x -> 
                let! newErr = f x
                return (Error newErr)
        }

    let ofSuccess x: TaskResult<_, _> = Ok x |> Task.FromResult

    let ofError x: TaskResult<_, _> = Error x |> Task.FromResult

    let ofAsync (x: Async<_>): TaskResult<_, _> =
        task {
            let! result = x
            return Ok result
        }

    let ofTask (x: Task<_>): TaskResult<_, _> = x |> Task.map Ok

    let ofEmptyTask (x: Task): TaskResult<_, _> =
        task {
            do! x
            return Ok()
        }
    
    let bindTask f x = x |> ofTask |> bind f

    let mapTask f x = x |> ofTask |> map f


[<AutoOpen>]
module TaskResultComputationExpression =
    type TaskResultBuilder() =
        member __.Return(x) = TaskResult.retn x
        member __.Bind(x, f) = TaskResult.bind f x
        member __.ReturnFrom(x) = x
        member __.Delay(f) = f
        member __.Run(f) = f()

        member this.Zero() = this.Return()

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

    let taskResult = TaskResultBuilder()

#endif