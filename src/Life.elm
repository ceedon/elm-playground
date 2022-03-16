module Life exposing (main)

import Array
import Element exposing (..)
import Element.Background exposing (color)
import Element.Events exposing (onClick)
import Browser
import Time


width : Int
width = 10

height : Int
height = 10

type alias Pos = (Int, Int)

type alias Board = List Pos

isAlive : Board -> Pos -> Bool
isAlive b p = List.member p b 

isEmpty : Board -> Pos -> Bool
isEmpty b p = not << isAlive p b 

neighbs : Pos -> List Pos
neighbs (x,y) = List.map wrap
  [ (x-1, y-1)
  , (x,y-1)
  , (x+1,y-1)
  , (x-1,y)
  , (x+1,y)
  , (x-1,y+1)
  , (x,y+1)
  , (x+1,y+1)]


{-| Causes the game board to wrap around once a live cell exceeds the bounds on the board.


Given width & height of 10,
`wrap (11,10)` returns `(1,10)` 
-}
wrap : Pos -> Pos
wrap (x, y) = ((remainderBy width  <| x - 1) + 1
             , (remainderBy height <| y - 1) + 1)


type alias Model =
  { board : Board
  , time  : Time.Posix
  , running : Bool}
type Msg
  = Tick Time.Posix
  | ClickedPause
  | ClickedCell Pos

subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every 1000 Tick

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Tick newTime ->
            if model.running then
              ({ model | time = newTime }
              , Cmd.none)
            else
              (model, Cmd.none)

        ClickedPause ->
            ({ model | paused = not model.running}, Cmd.none)
        
        ClickedCell c ->
            (model, Cmd.none) --- TODO invert c

--- Main ---
main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }
