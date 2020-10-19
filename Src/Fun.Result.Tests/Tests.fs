module Tests

open System.Net.Http
open System.Diagnostics
open FSharp.Control.Tasks
open Xunit
open Fun.Result


[<Fact>]
let ``Task map`` () =
    Task.retn 12
    |> Task.map ((*) 2)
    |> Task.runSynchronously
    |> fun x ->
        Assert.Equal(24, x)


[<Fact>]
let ``TaskResult basic`` () =
    let sw = Stopwatch.StartNew()
    taskResult {
        let x = 1
        return x + 1
    }
    |> Task.sleep 1000
    |> Task.runSynchronously
    |> fun x ->
        Assert.True(sw.ElapsedMilliseconds > 900L)
        Assert.Equal(Ok 2, x)


[<Fact>]
let ``HttpClient`` () =
    task {
        use httpClient = new HttpClient()
        let! r = httpClient.GetAsync("https://www.slaveoftime.fun")
        if int r.StatusCode < 400 then 
            return! r.Content.ReadAsStringAsync() |> Task.map Ok
        else
            return r.StatusCode |> int |> Error
    }
    |> TaskResult.map (fun x -> x.Length)
    |> Task.runSynchronously
    |> function
        | Ok x -> Assert.True(x > 0)
        | Error x -> Assert.True(x > 400)


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
