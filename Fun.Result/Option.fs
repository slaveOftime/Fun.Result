namespace Fun.Result


module Option =
    let inline ofTrue x =
        match x with
        | true -> Some()
        | false -> None

    let inline ofFalse x =
        match x with
        | false -> Some()
        | true -> None

    let inline defaultWithOption fn x =
        match x with
        | None -> fn ()
        | x -> x


[<AutoOpen>]
module OptionComputationExpression =
    type OptionBuilder() =
        member inline _.Return(x) = Some x
        member inline _.ReturnFrom(x) = x

        member inline _.Bind(x, [<InlineIfLambda>] f) = Option.bind f x

        member inline _.Bind(x: Result<_, _>, [<InlineIfLambda>] f) =
            Option.bind
                f
                (match x with
                 | Ok x -> Some x
                 | _ -> None)

        member inline _.Delay([<InlineIfLambda>] f) = f ()
        member inline _.Zero() = Some()

    let option = OptionBuilder()
