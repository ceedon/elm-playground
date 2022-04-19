module EuclidsAlgorithm exposing (..)


applyEuclids : Int -> Int -> Int
applyEuclids first second =
    let
        remainder =
            remainderBy first second
    in
    if remainder == 0 then
        first

    else
        applyEuclids first remainder
