namespace FSharpUtils

/// The `Validation` type is the same as the `Result` type but with a *list* for failures
/// rather than a single value. This allows `Validation` types to be combined by combining their errors
type Validation<'Success, 'Failure> = Result<'Success, 'Failure list>

[<RequireQualifiedAccess>]
module Validation =
    /// Apply a Validation<fn> to a Validation<x> applicatively
    let apply (fV : Validation<_, _>) (xV : Validation<_, _>) : Validation<_, _> =
        match fV, xV with
        | Ok f, Ok x -> Ok(f x)
        | Error errs1, Ok _ -> Error errs1
        | Ok _, Error errs2 -> Error errs2
        | Error errs1, Error errs2 -> Error(errs1 @ errs2)

    /// combine a list of Validation, applicatively
    let sequence (aListOfValidations : Validation<_, _> list) =
        let (<*>) = apply
        let (<!>) = Result.map
        let cons head tail = head :: tail
        let consR headR tailR = cons <!> headR <*> tailR
        let initialValue = Ok []
        List.foldBack consR aListOfValidations initialValue

    let ofResult xR : Validation<_, _> = xR |> Result.mapError List.singleton
    let toResult (xV : Validation<_, _>) : Result<_, _> = xV
