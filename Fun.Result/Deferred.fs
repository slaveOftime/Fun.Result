namespace Fun.Result


[<RequireQualifiedAccess>]
type DeferredState<'T, 'Error> =
    | NotStartYet
    | Loading
    | Loaded of 'T
    | LoadFailed of 'Error
    | Reloading of 'T
    | ReloadFailed of 'T * 'Error

    member inline this.Value =
        match this with
        | DeferredState.Loaded x
        | DeferredState.Reloading x
        | DeferredState.ReloadFailed(x, _) -> Some x
        | _ -> None

    member inline this.IsLoadingNow =
        match this with
        | Loading
        | Reloading _ -> true
        | _ -> false

    member inline this.Error =
        match this with
        | LoadFailed e
        | ReloadFailed(_, e) -> Some e
        | _ -> None

    member inline this.StartLoad() =
        match this with
        | Loaded x
        | ReloadFailed(x, _) -> Reloading x
        | _ -> Loading

    member inline this.WithError e =
        match this with
        | Reloading x -> ReloadFailed(x, e)
        | _ -> LoadFailed e


[<RequireQualifiedAccess>]
module DeferredState =

    let map fn state =
        match state with
        | DeferredState.NotStartYet -> DeferredState.NotStartYet
        | DeferredState.Loading -> DeferredState.Loading
        | DeferredState.Loaded x -> DeferredState.Loaded(fn x)
        | DeferredState.LoadFailed e -> DeferredState.LoadFailed e
        | DeferredState.Reloading x -> DeferredState.Reloading(fn x)
        | DeferredState.ReloadFailed(x, e) -> DeferredState.ReloadFailed(fn x, e)

    let inline ofOption data =
        match data with
        | Some x -> DeferredState.Loaded x
        | None -> DeferredState.NotStartYet

    let inline toOption (data: DeferredState<_, _>) = data.Value

    let inline ofResult data =
        match data with
        | Ok x -> DeferredState.Loaded x
        | Error e -> DeferredState.LoadFailed e


[<RequireQualifiedAccess>]
type DeferredOperation<'T, 'Error> =
    | Start
    | Finished of 'T
    | Failed of 'Error


[<RequireQualifiedAccess>]
module DeferredOperation =

    let inline map ([<InlineIfLambda>] fn) operation =
        match operation with
        | DeferredOperation.Start -> DeferredOperation.Start
        | DeferredOperation.Finished x -> DeferredOperation.Finished(fn x)
        | DeferredOperation.Failed e -> DeferredOperation.Failed e

    let inline ofOption data =
        match data with
        | Some x -> DeferredOperation.Finished x
        | None -> DeferredOperation.Start

    let inline toOption data =
        match data with
        | DeferredOperation.Finished x -> Some x
        | _ -> None

    let inline ofResult data =
        match data with
        | Ok x -> DeferredOperation.Finished x
        | Error e -> DeferredOperation.Failed e


[<RequireQualifiedAccess>]
type LoadingState<'T> =
    | NotStartYet
    | Loading
    | Loaded of 'T
    | Reloading of 'T

    member inline this.Value =
        match this with
        | Loaded x
        | Reloading x -> Some x
        | _ -> None

    member inline this.IsLoadingNow =
        match this with
        | Loading
        | Reloading _ -> true
        | _ -> false


[<RequireQualifiedAccess>]
module LoadingState =

    let inline map ([<InlineIfLambda>] fn) state =
        match state with
        | LoadingState.NotStartYet -> LoadingState.NotStartYet
        | LoadingState.Loading -> LoadingState.Loading
        | LoadingState.Loaded x -> LoadingState.Loaded(fn x)
        | LoadingState.Reloading x -> LoadingState.Reloading(fn x)

    let inline start state =
        match state with
        | LoadingState.Loaded x
        | LoadingState.Reloading x -> LoadingState.Reloading x
        | _ -> LoadingState.Loading

    let inline ofResult x =
        match x with
        | Ok x -> LoadingState.Loaded x
        | Error _ -> LoadingState.NotStartYet

    let inline ofOption x =
        match x with
        | Some x -> LoadingState.Loaded x
        | None -> LoadingState.NotStartYet

    let inline toOption x =
        match x with
        | LoadingState.Loaded x
        | LoadingState.Reloading x -> Some x
        | LoadingState.NotStartYet
        | LoadingState.Loading -> None

    /// Get the value and an indicator for isLoading
    let inline unzip defaultValue x =
        match x with
        | LoadingState.Loaded x -> x, false
        | LoadingState.Reloading x -> x, true
        | LoadingState.NotStartYet -> defaultValue, false
        | LoadingState.Loading -> defaultValue, true
