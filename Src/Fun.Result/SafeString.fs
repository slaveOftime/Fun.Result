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

    let (|SafeStringHead|_|) tail = function
        | SafeString x ->
            if String.IsNullOrEmpty tail |> not && x.EndsWith(tail, StringComparison.OrdinalIgnoreCase)
            then Some (x.Substring(0, x.Length - tail.Length))
            else None
        | NullOrEmptyString ->
            None

    let (|SafeStringEndWith|_|) (ends: string) = function
        | SafeString x ->
            if x.EndsWith(ends) then Some()
            else None
        | NullOrEmptyString ->
            None

    let (|SafeStringEndWithCi|_|) (ends: string) = function
        | SafeString x ->
            if x.EndsWith(ends, StringComparison.OrdinalIgnoreCase) then Some()
            else None
        | NullOrEmptyString ->
            None

    let (|SafeStringStartWith|_|) (start: string) = function
        | SafeString x ->
            if x.StartsWith(start) then Some()
            else None
        | NullOrEmptyString ->
            None

    let (|SafeStringStartWithCi|_|) (start: string) = function
        | SafeString x ->
            if x.StartsWith(start, StringComparison.OrdinalIgnoreCase) then Some()
            else None
        | NullOrEmptyString ->
            None

    let(|INT32|_|) (str: string) =
        match Int32.TryParse str with
        | true, x -> Some x
        | _ -> None

    let(|INT64|_|) (str: string) =
        match Int64.TryParse str with
        | true, x -> Some x
        | _ -> None

    let(|FLOAT|_|) (str: string) =
        try
            Convert.ToDouble(str) |> Some
        with _ ->
            None

    let(|DECIMAL|_|) (str: string) =
        try
            Convert.ToDecimal(str) |> Some
        with _ ->
            None


[<RequireQualifiedAccess>]
module SafeString =
    let toOption = function
        | SafeString x      -> Some x
        | NullOrEmptyString -> None

