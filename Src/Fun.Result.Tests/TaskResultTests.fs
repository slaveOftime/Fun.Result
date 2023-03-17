module TaskResultTests

open System
open System.Net.Http
open System.Diagnostics
open System.Threading.Tasks
open Xunit
open FsUnit.Xunit.Typed
open Fun.Result


[<Fact>]
let ``Task map`` () = Task.retn 12 |> Task.map ((*) 2) |> Task.runSynchronously |> fun x -> Assert.Equal(24, x)


[<Fact>]
let ``task for using HttpClient`` () =
    taskResult {
        use httpClient = new HttpClient()
        let! r = httpClient.GetAsync("https://www.bing.com") |> TaskResult.ofTask
        if int r.StatusCode < 400 then
            return! r.Content.ReadAsStringAsync() |> Task.map Ok
        else
            return! r.StatusCode |> int |> TaskResult.ofError
    }
    |> TaskResult.map (fun x -> x.Length)
    |> Task.runSynchronously
    |> function
        | Ok x -> Assert.True(x > 0)
        | Error x -> Assert.True(x > 400)


[<Fact>]
let ``TaskResult basic`` () =
    task {
        let sw = Stopwatch.StartNew()

        let! result =
            taskResult {
                let! _ = TaskResult.ofSuccess 1
                return 1 + 1
            }
            |> Task.sleep 1000

        sw.ElapsedMilliseconds |> should greaterOrEqualThan 1000L
        result |> should equal (Ok 2)
    }


[<Fact>]
let ``TaskResult try with`` () = task {
    let! result = taskResult {
        try
            failwith "fail"
            return 1
        with _ ->
            return 2
    }
    result |> should equal (Ok 2)
}


[<Fact>]
let ``TaskResult try finally`` () =
    Assert.ThrowsAnyAsync<exn>(fun () -> taskResult {
        try
            failwith "fail"
        finally
            ()
    })



[<Fact>]
let ``TaskResult should handle use correctly`` () = task {
    let mutable calls = Collections.Generic.List()
    let disposeObj = { new IDisposable with member _.Dispose() = calls.Add(1) }

    let! _ = taskResult {
        use _ = disposeObj
        do! Task.Delay 100 |> TaskResult.ofEmptyTask
        calls.Add(2)
    }

    calls |> Seq.toList |> should equal [ 2; 1 ]
}
