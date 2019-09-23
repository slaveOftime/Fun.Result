namespace Fun.Result

open System


[<AutoOpen>]
module SafeStringDsl =
    
    let (|NullOrEmptyString|SafeString|) str =
        if String.IsNullOrEmpty str
        then NullOrEmptyString
        else SafeString str

    let (|SafeStringLower|_|) = function
        | SafeString x -> Some (x.ToLower())
        | NullOrEmptyString -> None

    let (|SafeStringUpper|_|) = function
        | SafeString x -> Some (x.ToUpper())
        | NullOrEmptyString -> None
    
    let (|SafeStringExtension|_|) = function
        | SafeString x ->
            let index = x.LastIndexOf '.'
            if index >= 0 then Some (x.Substring(0, index), x.Substring(index).ToLower())
            else Some (x, "")
        | NullOrEmptyString ->
            None


[<RequireQualifiedAccess>]
module SafeString =
    let toOption = function
        | SafeString x      -> Some x
        | NullOrEmptyString -> None

