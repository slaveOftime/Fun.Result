namespace FSharpUtils


[<RequireQualifiedAccess>]
module Result =
    let bimap onSuccess onError xR =
        match xR with
        | Ok x -> onSuccess x
        | Error err -> onError err

    let map = Result.map
    let mapError = Result.mapError

    let bind = Result.bind

    let iter (f : _ -> unit) result = map f result |> ignore

    /// Apply a Result<fn> to a Result<x> monadically
    let apply fR xR =
        match fR, xR with
        | Ok f, Ok x -> Ok(f x)
        | Error err1, Ok _ -> Error err1
        | Ok _, Error err2 -> Error err2
        | Error err1, Error _ -> Error err1

    // combine a list of results, monadically
    let sequence aListOfResults =
        let (<*>) = apply
        let (<!>) = map
        let cons head tail = head :: tail
        let consR headR tailR = cons <!> headR <*> tailR
        let initialValue = Ok []
        List.foldBack consR aListOfResults initialValue


    /// Lift a two parameter function to use Result parameters
    let lift2 f x1 x2 =
        let (<!>) = map
        let (<*>) = apply
        f <!> x1 <*> x2

    /// Lift a three parameter function to use Result parameters
    let lift3 f x1 x2 x3 =
        let (<!>) = map
        let (<*>) = apply
        f <!> x1 <*> x2 <*> x3

    /// Lift a four parameter function to use Result parameters
    let lift4 f x1 x2 x3 x4 =
        let (<!>) = map
        let (<*>) = apply
        f <!> x1 <*> x2 <*> x3 <*> x4

    /// Apply a monadic function with two parameters
    let bind2 f x1 x2 = lift2 f x1 x2 |> bind id

    /// Apply a monadic function with three parameters
    let bind3 f x1 x2 x3 = lift3 f x1 x2 x3 |> bind id

    /// Predicate that returns true on success
    let isOk =
        function
        | Ok _ -> true
        | Error _ -> false

    /// Predicate that returns true on failure
    let isError xR =
        xR
        |> isOk
        |> not

    /// Lift a given predicate into a predicate that works on Results
    let filter pred =
        function
        | Ok x -> pred x
        | Error _ -> true

    /// On success, return the value. On error, return a default value
    let ifError defaultVal =
        function
        | Ok x -> x
        | Error _ -> defaultVal

    /// Apply a monadic function to an Result<x option>
    let bindOption f xR =
        match xR with
        | Some x -> f x |> map Some
        | None -> Ok None

    /// Convert an Option into a Result. If none, use the passed-in errorValue
    let ofOption errorValue opt =
        match opt with
        | Some v -> Ok v
        | None -> Error errorValue

    /// Convert a Result into an Option
    let toOption xR =
        match xR with
        | Ok v -> Some v
        | Error _ -> None

    /// Convert the Error case into an Option
    /// (useful with List.choose to find all errors in a list of Results)
    let toErrorOption =
        function
        | Ok _ -> None
        | Error err -> Some err

    /// Run a unit in the map pipeline without input
    let pass f = map (fun x -> f; x)

    let catch errorMap f x =
        try f x
        with ex -> errorMap ex |> Error


[<AutoOpen>]
module ResultComputationExpression =
    type ResultBuilder() =
        member __.Return(x) = Ok x
        member __.Bind(x, f) = Result.bind f x
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

    let result = new ResultBuilder()
