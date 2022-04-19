module EuclidsAlgorithm exposing (..)


apply : Int -> Int -> Int
apply first second =
    let
        remainder =
            remainderBy first second
    in
    if remainder == 0 then
        first

    else
        apply remainder first
