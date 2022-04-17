module DotsTests exposing (..)

import Dots exposing (..)
import Expect
import List exposing (isEmpty)
import Test exposing (Test, describe, test)


singleSegment : Test
singleSegment =
    describe "Marking the first segment"
        [ test "on an empty board" <|
            \_ ->
                Expect.true "Expected the board to be empty." <| isEmpty emptyBoard
        , test "will result in a board containing only that segment." <|
            \_ ->
                Expect.equal [ originToX1 ] <| addSegment emptyBoard originToX1
        ]


twoSegments : Test
twoSegments =
    describe "Marking a second segment"
        [ test "on a board with one segment" <|
            \_ ->
                Expect.equal 1 <| List.length oneSegmentBoard
        , test "will result in a board with 2 segments." <|
            \_ ->
                Expect.equal 2 <| List.length <| addSegment oneSegmentBoard originToY1
        ]


emptyBoard : Board
emptyBoard =
    []


originToX1 : Segment
originToX1 =
    ( ( 0, 0 ), ( 1, 0 ) )


originToY1 : Segment
originToY1 =
    ( ( 0, 0 ), ( 0, 1 ) )


oneSegmentBoard : Board
oneSegmentBoard =
    addSegment emptyBoard originToX1
