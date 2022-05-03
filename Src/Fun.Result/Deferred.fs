namespace Fun.Result


[<RequireQualifiedAccess>]
type DeferredState<'T, 'Error> =
    | NotStartYet
    | Loading
    | Loaded of 'T
    | LoadFailed of 'Error
    | Reloading of 'T
    | ReloadFailed of 'T * 'Error

    member this.Value =
        match this with
        | DeferredState.Loaded x
        | DeferredState.Reloading x
        | DeferredState.ReloadFailed (x, _) -> Some x
        | _ -> None

    member this.IsLoadingNow =
        match this with
        | Loading
        | Reloading _ -> true
        | _ -> false

    member this.Error =
        match this with
        | LoadFailed e
        | ReloadFailed (_, e) -> Some e
        | _ -> None

    member this.StartLoad() =
        match this with
        | Loaded x
        | ReloadFailed (x, _) -> Reloading x
        | _ -> Loading

    member this.WithError e =
        match this with
        | Reloading x -> ReloadFailed(x, e)
        | _ -> LoadFailed e


[<RequireQualifiedAccess>]
module DeferredState =
    let ofOption data =
        match data with
        | Some x -> DeferredState.Loaded x
        | None -> DeferredState.NotStartYet

    let toOption (data: DeferredState<_, _>) = data.Value

    let ofResult data =
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
    let ofOption data =
        match data with
        | Some x -> DeferredOperation.Finished x
        | None -> DeferredOperation.Start

    let toOption data =
        match data with
        | DeferredOperation.Finished x -> Some x
        | _ -> None

    let ofResult data =
        match data with
        | Ok x -> DeferredOperation.Finished x
        | Error e -> DeferredOperation.Failed e



[<RequireQualifiedAccess>]
type LoadingState<'T> =
    | NotStartYet
    | Loading
    | Loaded of 'T
    | Reloading of 'T

    member this.Value =
        match this with
        | Loaded x
        | Reloading x -> Some x
        | _ -> None

    member this.IsLoadingNow =
        match this with
        | Loading
        | Reloading _ -> true
        | _ -> false


[<RequireQualifiedAccess>]
module LoadingState =

    let map fn state =
        match state with
        | LoadingState.NotStartYet -> LoadingState.NotStartYet
        | LoadingState.Loading -> LoadingState.Loading
        | LoadingState.Loaded x -> LoadingState.Loaded(fn x)
        | LoadingState.Reloading x -> LoadingState.Reloading(fn x)

    let start state =
        match state with
        | LoadingState.Loaded x
        | LoadingState.Reloading x -> LoadingState.Reloading x
        | _ -> LoadingState.Loading

    let ofResult x =
        match x with
        | Ok x -> LoadingState.Loaded x
        | Error _ -> LoadingState.NotStartYet

    let ofOption x =
        match x with
        | Some x -> LoadingState.Loaded x
        | None -> LoadingState.NotStartYet

    let toOption x =
        match x with
        | LoadingState.Loaded x
        | LoadingState.Reloading x -> Some x
        | LoadingState.NotStartYet
        | LoadingState.Loading -> None

    /// Get the value and an indicator for isLoading
    let unzip defaultValue x =
        match x with
        | LoadingState.Loaded x -> x, false
        | LoadingState.Reloading x -> x, true
        | LoadingState.NotStartYet -> defaultValue, false
        | LoadingState.Loading -> defaultValue, true
