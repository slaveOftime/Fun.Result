namespace Fun.Result

[<AutoOpen>]
module Comparison =
    let (|Between|_|) min max x =
        if x > min && x < max then Some()
        else None

    let (|BetweenEqual|_|) min max x =
        if x >= min && x <= max then Some()
        else None

    let (|LessThan|_|) value x =
        if x < value then Some()
        else None

    let (|LessEqual|_|) value x =
        if x <= value then Some()
        else None

    let (|GreaterThan|_|) value x =
        if x > value then Some()
        else None

    let (|GreaterEqual|_|) value x =
        if x >= value then Some()
        else None
