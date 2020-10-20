namespace Fun.Result


module Option =
    let ofTrue = function true -> Some() | false -> None
    let ofFalse = function false -> Some() | true -> None


[<AutoOpen>]
module OptionComputationExpression =
    type OptionBuilder() =
        member __.Return(x) = Some x
        member __.ReturnFrom(x) = x
        member __.Bind(x, f) = Option.bind f x
        member __.Delay(f) = f()
        member __.Zero() = Some ()

    let option = OptionBuilder()
