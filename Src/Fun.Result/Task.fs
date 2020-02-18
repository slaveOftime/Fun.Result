namespace Fun.Result

open System.Threading.Tasks
open FSharp.Control.Tasks


[<RequireQualifiedAccess>]
module Task =
    let map f (t: Task<_>) =
        task {
            let! x = t
            return f x
        }

    let bind f (t: Task<_>) =
        task {
            let! x = t
            return! f x
        }

    let sleep (ms: int) (t: Task<_>) = 
        task {
            do! Task.Delay ms
            return! t
        }

    let runSynchronously (t: Task<_>) = t.Wait(); t.Result
