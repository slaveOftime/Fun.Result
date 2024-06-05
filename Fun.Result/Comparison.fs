namespace Fun.Result

[<AutoOpen>]
module Comparison =
    let inline (|Between|_|) min max x = if x > min && x < max then Some() else None

    let inline (|BetweenEqual|_|) min max x = if x >= min && x <= max then Some() else None

    let inline (|LessThan|_|) value x = if x < value then Some() else None

    let inline (|LessEqual|_|) value x = if x <= value then Some() else None

    let inline (|GreaterThan|_|) value x = if x > value then Some() else None

    let inline (|GreaterEqual|_|) value x = if x >= value then Some() else None
