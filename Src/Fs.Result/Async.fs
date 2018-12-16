namespace global

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


[<AutoOpen>]
module AsyncOptionComputationExpression = 

    type AsyncOptionBuilder() = 
        member __.Return(x) = x |> Some |> Async.retn

        member __.Bind(x, f) =
            async {
                match! x with
                | Some x -> return! f x
                | None -> return None
            }

        member __.ReturnFrom(x) = x

    let asyncOption = AsyncOptionBuilder()
