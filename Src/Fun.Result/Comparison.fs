namespace Fun.Result

[<AutoOpen>]
module Comparison =
    let (|Between|_|) min max x =
        if x > min && x < max then Some()
        else None

    let (|BetweenEqual|_|) min max x =
        if x >= min && x <= max then Some()
        else None

    let (|LessThan|) value x =
        if x < value then Some()
        else None

    let (|LessEqual|) value x =
        if x <= value then Some()
        else None

    let (|GreaterThan|) value x =
        if x > value then Some()
        else None

    let (|GreaterEqual|) value x =
        if x >= value then Some()
        else None
