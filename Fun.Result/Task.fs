namespace Fun.Result

#if !FABLE_COMPILER

open System.Threading.Tasks


[<RequireQualifiedAccess>]
module Task =
    let inline map ([<InlineIfLambda>] f) (t: Task<_>) = task {
        let! x = t
        return f x
    }

    let inline bind ([<InlineIfLambda>] f) (t: Task<_>) = task {
        let! x = t
        return! f x
    }

    let inline sleep (ms: int) (t: Task<_>) = task {
        do! Task.Delay ms
        return! t
    }

    let inline toUnitTask (t: Task) = task { do! t }

    let inline runSynchronously (t: Task<_>) =
        t.Wait()
        t.Result

    let inline retn x = Task.FromResult x

#endif
