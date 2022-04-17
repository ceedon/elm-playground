module Dots exposing (..)

import Element exposing (..)
import Html exposing (Html)


type alias Point =
    ( Int, Int )


type alias Segment =
    ( Point, Point )


type alias Board =
    List Segment


addSegment : Board -> Segment -> Board
addSegment b s =
    s :: b


type Msg
    = ClickSegment Segment


main : Html msg
main =
    text "Hello, World!"
        |> layout []
