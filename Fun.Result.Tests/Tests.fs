module Tests

open Xunit
open Fun.Result


[<Fact>]
let ``SafeStringEndWith test`` () =
    match "123ends" with
    | SafeStringEndWith "ends" -> ()
    | _ -> failwith "SafeStringEndWith failed"

    match "123ends" with
    | SafeStringEndWith "Ends" -> failwith "Case ignore is not correctly"
    | _ -> ()


[<Fact>]
let ``SafeStringEndWithCi test`` () =
    match "123ends" with
    | SafeStringEndWithCi "ends" -> ()
    | _ -> failwith "SafeStringEndWithCi failed"

    match "123ends" with
    | SafeStringEndWithCi "EnDs" -> ()
    | _ -> failwith "SafeStringEndWithCi failed"


[<Fact>]
let ``SafeStringStartWith test`` () =
    match "start123" with
    | SafeStringStartWith "start" -> ()
    | _ -> failwith "SafeStringStartWith failed"

    match "123ends" with
    | SafeStringStartWith "Ends" -> failwith "Case ignore is not correctly"
    | _ -> ()


[<Fact>]
let ``SafeStringStartWithCi test`` () =
    match "start123" with
    | SafeStringStartWithCi "start" -> ()
    | _ -> failwith "SafeStringStartWithCi failed"

    match "start123" with
    | SafeStringStartWithCi "sTart" -> ()
    | _ -> failwith "SafeStringStartWithCi failed"


[<Fact>]
let ``SafeStringHead test`` () =
    match "123tail" with
    | SafeStringHead "tail" x -> Assert.Equal("123", x)
    | _ -> failwith "SafeStringHead failed"


[<Fact>]
let ``SafeStringTail test`` () =
    match "123tail" with
    | SafeStringTail "123" x -> Assert.Equal("tail", x)
    | _ -> failwith "SafeStringTail failed"

    match "123" with
    | SafeStringTail "123" x -> Assert.Equal("", x)
    | _ -> failwith "SafeStringTail failed"


[<Fact>]
let ``SafeStringTail with INT32`` () =
    match "+123" with
    | SafeStringTail "+" (INT32 x) -> Assert.Equal(123, x)
    | _ -> failwith "SafeStringHead failed"


[<Fact>]
let ``SafeStringExtension should work`` () =
    match "demo" with
    | SafeStringExtension(_, _) -> failwith "SafeStringExtension failed"
    | _ -> ()

    match "demo." with
    | SafeStringExtension(_, _) -> failwith "SafeStringExtension failed"
    | _ -> ()

    match "demo.p" with
    | SafeStringExtension(_, ".p") -> ()
    | _ -> failwith "SafeStringExtension failed"

    match "demo.P" with
    | SafeStringExtension(_, ".p") -> ()
    | _ -> failwith "SafeStringExtension failed"


[<Fact>]
let ``Option tests`` () =
    option { do! Some() }
    |> function
        | Some x -> Assert.Equal((), x)
        | None -> failwith "Zero function is not correct"

    option { do! None }
    |> function
        | None -> ()
        | Some _ -> failwith "Zero function is not correct"


    let result = None |> Option.defaultWithOption (fun _ -> Some 1)
    Assert.Equal(Some 1, result)

    let result = Some 0 |> Option.defaultWithOption (fun _ -> Some 1)
    Assert.Equal(Some 0, result)

    let result = None |> Option.defaultWithOption (fun _ -> None)
    Assert.Equal(None, result)


    let result = option {
        let! x = Some 1
        let! y = Some 1
        let! z = Some 1
        return x + y + z
    }
    Assert.Equal(Some 3, result)
