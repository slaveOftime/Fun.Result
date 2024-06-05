namespace Fun.Result

open Microsoft.FSharp.Control

type AsyncResult<'Success, 'Failure> = Async<Result<'Success, 'Failure>>

[<RequireQualifiedAccess>]
module AsyncResult =
    let inline map f (x: AsyncResult<_, _>) : AsyncResult<_, _> = Async.map (Result.map f) x
    let inline mapError f (x: AsyncResult<_, _>) : AsyncResult<_, _> = Async.map (Result.mapError f) x
    let inline ignore x = x |> map ignore

    let inline retn x : AsyncResult<_, _> = x |> Result.Ok |> Async.retn

    /// Handles asynchronous exceptions and maps them into Failure cases using the provided function
    let inline catch f (x: AsyncResult<_, _>) : AsyncResult<_, _> =
        x
        |> Async.Catch
        |> Async.map (
            function
            | Choice1Of2(Ok v) -> Ok v
            | Choice1Of2(Error err) -> Error err
            | Choice2Of2 ex -> Error(f ex)
        )

    /// Apply an AsyncResult function to an AsyncResult value, monadically
    let inline applyM (fAsyncResult: AsyncResult<_, _>) (xAsyncResult: AsyncResult<_, _>) : AsyncResult<_, _> =
        fAsyncResult |> Async.bind (fun fResult -> xAsyncResult |> Async.map (fun xResult -> Result.apply fResult xResult))

    /// Apply an AsyncResult function to an AsyncResult value, applicatively
    let inline applyA (fAsyncResult: AsyncResult<_, _>) (xAsyncResult: AsyncResult<_, _>) : AsyncResult<_, _> =
        fAsyncResult |> Async.bind (fun fResult -> xAsyncResult |> Async.map (fun xResult -> Validation.apply fResult xResult))

    /// Apply a monadic function to an AsyncResult value
    let inline bind (f: 'a -> AsyncResult<'b, 'c>) (xAsyncResult: AsyncResult<_, _>) : AsyncResult<_, _> = async {
        let! xResult = xAsyncResult
        match xResult with
        | Ok x -> return! f x
        | Error err -> return (Error err)
    }

    let inline bindError (f: _ -> Async<_>) (xAsyncResult: AsyncResult<_, _>) : AsyncResult<_, _> = async {
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
    let inline ofSuccess x : AsyncResult<_, _> = x |> Result.Ok |> Async.retn

    /// Lift a value into an Error inside a AsyncResult
    let inline ofError x : AsyncResult<_, _> = x |> Result.Error |> Async.retn

    /// Lift a Result into an AsyncResult
    let inline ofResult x : AsyncResult<_, _> = x |> Async.retn

    /// Lift a Async into an AsyncResult
    let inline ofAsync x : AsyncResult<_, _> = x |> Async.map Result.Ok

#if !FABLE_COMPILER
    let inline ofTask x : AsyncResult<_, _> = x |> Async.AwaitTask |> ofAsync
#endif

    let inline sleep (ms: int) = Async.Sleep ms |> ofAsync

    let inline bindAsync f x = x |> ofAsync |> bind f

    let inline mapAsync f x = x |> ofAsync |> map f

    let inline pass f =
        map (fun x ->
            f
            x
        )

[<AutoOpen>]
module AsyncResultComputationExpression =
    type AsyncResultBuilder() =
        member inline __.Return(x) = AsyncResult.retn x
        member inline __.Bind(x, f) = AsyncResult.bind f x
        member inline __.ReturnFrom(x) = x
        member inline this.Zero() = this.Return()
        member inline __.Delay(f) = f
        member inline __.Run(f) = f ()

        member this.While(guard, body: unit -> AsyncResult<unit, _>) =
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

        member inline this.TryFinally(body, compensation) =
            this.ReturnFrom(body ())
            |> Async.Catch
            |> Async.map (
                function
                | Choice1Of2 x ->
                    compensation ()
                    x
                | Choice2Of2 ex ->
                    compensation ()
                    raise ex
            )

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

    let asyncResult = AsyncResultBuilder()

[<AutoOpen>]
module AsyncResultOptionComputationExpression =
    type AsyncResultOptionBuilder() =

        member inline __.Return(x) = x |> Some |> AsyncResult.retn

        member inline __.ReturnFrom(x) = x

        member inline __.Bind(x, f) = asyncResult {
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

    let asyncResultOption = AsyncResultOptionBuilder()
