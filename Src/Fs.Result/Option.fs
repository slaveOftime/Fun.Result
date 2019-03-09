namespace global


[<AutoOpen>]
module OptionComputationExpression =
    type OptionBuilder() =
        member __.Return(x) = Some x
        member __.ReturnFrom(x) = x
        member __.Bind(x, f) = Option.bind f x
        member __.Delay(f) = f()

    let option = OptionBuilder()
