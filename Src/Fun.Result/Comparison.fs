namespace Fun.Result

[<AutoOpen>]
module Comparison =
    let (|NumBetween|_|) min max x =
        if x > min && x < max then Some()
        else None

    let (|NumBetweenE|_|) min max x =
        if x >= min && x <= max then Some()
        else None

    let (|SmallThan|) value x =
        if x < value then Some()
        else None

    let (|SmallEqual|) value x =
        if x <= value then Some()
        else None

    let (|LargeThan|) value x =
        if x > value then Some()
        else None

    let (|LargeEqual|) value x =
        if x >= value then Some()
        else None
