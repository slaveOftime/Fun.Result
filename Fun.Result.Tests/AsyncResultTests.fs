module AsyncResultTests

open System
open Xunit
open FsUnit.Xunit.Typed
open Fun.Result


[<Fact>]
let ``AsyncResult try with`` () =
    asyncResult {
        try
            failwith "fail"
            return 1
        with _ ->
            return 2
    }
    |> Async.RunSynchronously
    |> should equal (Ok 2)


[<Fact>]
let ``AsyncResult try finally`` () =
    Assert.ThrowsAny<exn>(fun () ->
        asyncResult {
            try
                failwith "fail"
            finally
                ()
        }
        |> Async.RunSynchronously
        |> ignore
    )


[<Fact>]
let ``AsyncResult should handle use correctly`` () =
    let mutable calls = Collections.Generic.List()
    let disposeObj =
        { new IDisposable with
            member _.Dispose() = calls.Add(1)
        }

    asyncResult {
        use _ = disposeObj
        do! Async.Sleep 100 |> Async.map Ok
        calls.Add(2)
    }
    |> Async.map ignore
    |> Async.RunSynchronously

    calls |> Seq.toList |> should equal [ 2; 1 ]


[<Fact>]
let ``AsyncResultOption should handle use correctly`` () =
    let mutable calls = Collections.Generic.List()
    let disposeObj =
        { new IDisposable with
            member _.Dispose() = calls.Add(1)
        }

    asyncResultOption {
        use _ = disposeObj
        do! Async.Sleep 100 |> Async.map (Some >> Ok)
        calls.Add(2)
    }
    |> Async.map ignore
    |> Async.RunSynchronously

    calls |> Seq.toList |> should equal [ 2; 1 ]


[<Fact>]
let ``AsyncOption should handle use correctly`` () =
    let mutable calls = Collections.Generic.List()
    let disposeObj =
        { new IDisposable with
            member _.Dispose() = calls.Add(1)
        }

    asyncOption {
        use _ = disposeObj
        do! Async.Sleep 100 |> Async.map Some
        calls.Add(2)
    }
    |> Async.map ignore
    |> Async.RunSynchronously

    calls |> Seq.toList |> should equal [ 2; 1 ]
