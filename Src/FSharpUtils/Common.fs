namespace global

open System


[<AutoOpen>]
module Utils =
    
    let (|NullOrEmptyString|SafeString|) str =
        if String.IsNullOrEmpty str
        then NullOrEmptyString
        else SafeString str
