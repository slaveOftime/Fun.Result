namespace Fun.Result

open System


[<AutoOpen>]
module SafeStringDsl =

    let inline (|NullOrEmptyString|SafeString|) str = if String.IsNullOrEmpty str then NullOrEmptyString else SafeString str

    let inline (|SafeStringLower|_|) x =
        match x with
        | SafeString x -> Some(x.ToLower())
        | NullOrEmptyString -> None

    let inline (|SafeStringUpper|_|) x =
        match x with
        | SafeString x -> Some(x.ToUpper())
        | NullOrEmptyString -> None

    let inline (|SafeStringExtension|_|) x =
        match x with
        | SafeString x ->
            let index = x.LastIndexOf '.'
            if index >= 0 && index < x.Length - 1 then
                Some(x.Substring(0, index), x.Substring(index).ToLower())
            else
                None
        | NullOrEmptyString -> None

    /// Given the tail and extract the head
    let inline (|SafeStringHead|_|) tail =
        function
        | SafeString x ->
            if String.IsNullOrEmpty tail |> not && x.EndsWith(tail, StringComparison.OrdinalIgnoreCase) then
                Some(x.Substring(0, x.Length - tail.Length))
            else
                None
        | NullOrEmptyString -> None

    /// Given the head and extract the tail
    let inline (|SafeStringTail|_|) head =
        function
        | SafeString x ->
            if String.IsNullOrEmpty head |> not && x.StartsWith(head, StringComparison.OrdinalIgnoreCase) then
                Some(x.Substring(head.Length))
            else
                None
        | NullOrEmptyString -> None

    let inline (|SafeStringEndWith|_|) (ends: string) =
        function
        | SafeString x -> if x.EndsWith(ends) then Some() else None
        | NullOrEmptyString -> None

    let inline (|SafeStringEndWithCi|_|) (ends: string) =
        function
        | SafeString x -> if x.EndsWith(ends, StringComparison.OrdinalIgnoreCase) then Some() else None
        | NullOrEmptyString -> None

    let inline (|SafeStringStartWith|_|) (start: string) =
        function
        | SafeString x -> if x.StartsWith(start) then Some() else None
        | NullOrEmptyString -> None

    let inline (|SafeStringStartWithCi|_|) (start: string) =
        function
        | SafeString x -> if x.StartsWith(start, StringComparison.OrdinalIgnoreCase) then Some() else None
        | NullOrEmptyString -> None

    let inline (|INT32|_|) (str: string) =
        match Int32.TryParse str with
        | true, x -> Some x
        | _ -> None

    let inline (|INT64|_|) (str: string) =
        match Int64.TryParse str with
        | true, x -> Some x
        | _ -> None

    let inline (|FLOAT|_|) (str: string) =
        match Double.TryParse(str) with
        | true, x -> Some x
        | _ -> None

    let inline (|DECIMAL|_|) (str: string) =
        match Decimal.TryParse(str) with
        | true, x -> Some x
        | _ -> None

    let inline (|DATETIME|_|) (str: string) =
        match DateTime.TryParse(str) with
        | true, x -> Some x
        | _ -> None

    let inline (|GUID|_|) (str: string) =
        match Guid.TryParse(str) with
        | true, x -> Some x
        | _ -> None


[<RequireQualifiedAccess>]
module SafeString =
    let inline toOption x =
        match x with
        | SafeString x -> Some x
        | NullOrEmptyString -> None
