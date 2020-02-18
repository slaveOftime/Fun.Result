module Tests

open System.Net.Http
open System.Diagnostics
open System.Threading.Tasks
open FSharp.Control.Tasks
open Xunit
open Fun.Result


[<Fact>]
let ``Task map`` () =
    Task.FromResult 12
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
