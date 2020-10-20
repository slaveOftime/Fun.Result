namespace Fun.Result


type AsyncResult<'Success, 'Failure> = Async<Result<'Success, 'Failure>>

[<RequireQualifiedAccess>]
module AsyncResult =
    let map f (x : AsyncResult<_, _>) : AsyncResult<_, _> =
        Async.map (Result.map f) x
    let mapError f (x : AsyncResult<_, _>) : AsyncResult<_, _> =
        Async.map (Result.mapError f) x
    let ignore x = x |> map ignore

    let retn x : AsyncResult<_, _> =
        x
        |> Result.Ok
        |> Async.retn

    /// Handles asynchronous exceptions and maps them into Failure cases using the provided function
    let catch f (x : AsyncResult<_, _>) : AsyncResult<_, _> =
        x
        |> Async.Catch
        |> Async.map (function
               | Choice1Of2(Ok v) -> Ok v
               | Choice1Of2(Error err) -> Error err
               | Choice2Of2 ex -> Error(f ex))

    /// Apply an AsyncResult function to an AsyncResult value, monadically
    let applyM (fAsyncResult : AsyncResult<_, _>)
        (xAsyncResult : AsyncResult<_, _>) : AsyncResult<_, _> =
        fAsyncResult
        |> Async.bind
               (fun fResult ->
               xAsyncResult
               |> Async.map (fun xResult -> Result.apply fResult xResult))

    /// Apply an AsyncResult function to an AsyncResult value, applicatively
    let applyA (fAsyncResult : AsyncResult<_, _>)
        (xAsyncResult : AsyncResult<_, _>) : AsyncResult<_, _> =
        fAsyncResult
        |> Async.bind
               (fun fResult ->
               xAsyncResult
               |> Async.map (fun xResult -> Validation.apply fResult xResult))

    /// Apply a monadic function to an AsyncResult value
    let bind (f : 'a -> AsyncResult<'b, 'c>) (xAsyncResult : AsyncResult<_, _>) : AsyncResult<_, _> =
        async {
            let! xResult = xAsyncResult
            match xResult with
            | Ok x -> return! f x
            | Error err -> return (Error err)
        }

    let bindError (f : _ -> Async<_>) (xAsyncResult : AsyncResult<_, _>) : AsyncResult<_, _> =
        async {
            let! xResult = xAsyncResult
            match xResult with
            | Ok x -> return Ok x
            | Error e -> 
                let! err = f e
                return (Error err)
        }

    /// Convert a list of AsyncResult into a AsyncResult<list> using monadic style.
    /// Only the first error is returned. The error type need not be a list.
    let sequenceM resultList =
        let (<*>) = applyM
        let (<!>) = map
        let cons head tail = head :: tail
        let consR headR tailR = cons <!> headR <*> tailR
        let initialValue = retn []
        List.foldBack consR resultList initialValue

    /// Convert a list of AsyncResult into a AsyncResult<list> using applicative style.
    /// All the errors are returned. The error type must be a list.
    let sequenceA resultList =
        let (<*>) = applyA
        let (<!>) = map
        let cons head tail = head :: tail
        let consR headR tailR = cons <!> headR <*> tailR
        let initialValue = retn [] // empty list inside Result
        // loop through the list, prepending each element
        // to the initial value
        List.foldBack consR resultList initialValue

    /// Lift a value into an Ok inside a AsyncResult
    let ofSuccess x : AsyncResult<_, _> =
        x
        |> Result.Ok
        |> Async.retn

    /// Lift a value into an Error inside a AsyncResult
    let ofError x : AsyncResult<_, _> =
        x
        |> Result.Error
        |> Async.retn

    /// Lift a Result into an AsyncResult
    let ofResult x : AsyncResult<_, _> = x |> Async.retn

    /// Lift a Async into an AsyncResult
    let ofAsync x : AsyncResult<_, _> = x |> Async.map Result.Ok

    #if !FABLE_COMPILER
    let ofTask x : AsyncResult<_, _> =
        x
        |> Async.AwaitTask
        |> ofAsync
    #endif

    let sleep (ms: int) = Async.Sleep ms |> ofAsync

    let bindAsync f x =
        x
        |> ofAsync
        |> bind f

    let mapAsync f x =
        x
        |> ofAsync
        |> map f

    let pass f = map (fun x -> f; x)

[<AutoOpen>]
module AsyncResultComputationExpression =
    type AsyncResultBuilder() =
        member __.Return(x) = AsyncResult.retn x
        member __.Bind(x, f) = AsyncResult.bind f x
        member __.ReturnFrom(x) = x
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

    let asyncResult = AsyncResultBuilder()

[<AutoOpen>]
module AsyncResultOptionComputationExpression =
    type AsyncResultOptionBuilder() =

        member __.Return(x) =
            x
            |> Some
            |> AsyncResult.retn

        member __.ReturnFrom(x) = x

        member __.Bind(x, f) =
            asyncResult {
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

    let asyncResultOption = AsyncResultOptionBuilder()
