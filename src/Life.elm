module Life exposing (main)

import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onClick)
import Element.Font exposing (size)
import Html exposing (Html)
import Time


xWidth : Int
xWidth = 50

yHeight : Int
yHeight = 50


type alias Pos = (Int, Int)

type alias Board = List Pos

isAlive : Board -> Pos -> Bool
isAlive b p = List.member p b 

isEmpty : Board -> Pos -> Bool
isEmpty b p = not <| isAlive b p

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

liveNeighbs : Board -> Pos -> Int
liveNeighbs b p = List.length
                <| List.filter (isAlive b)
                <| neighbs p


{-| Causes the game board to wrap around once a live cell exceeds the bounds on the board.


Given width & height of 10,
`wrap (11,10)` returns `(1,10)` 
-}
wrap : Pos -> Pos
wrap (x, y) = ((modBy xWidth  <| x - 1) + 1
             , (modBy yHeight <| y - 1) + 1)

survivors : Board -> List Pos
survivors b = List.filter (\p -> List.member (liveNeighbs b p) [2,3]) b

births : Board -> List Pos
births b =
  let
    ps = dedupe
      <| List.concat
      <| List.map neighbs b
    isBorn x =
      3 == liveNeighbs b x
   in
    List.filter (isEmpty b) ps
    |> List.filter (isBorn)

dedupe : List a -> List a
dedupe list =
  case list of
    [] -> []
    [x] -> [x]
    (x::xs) -> x :: dedupe (List.filter ((/=) x) xs)

step : Board -> Board
step b = survivors b ++ births b


type alias Model =
  { board : Board
  , time  : Time.Posix
  , running : Bool}

type Msg
  = Tick Time.Posix
  | ClickedPause
  | ClickedCell Pos

subscriptions : Model -> Sub Msg
subscriptions _ =
  Time.every 500 Tick

flipCell : Board -> Pos -> Board
flipCell b p =
  if (List.member p b) then
    List.filter ((/=) p) b
  else
    p :: b

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Tick _ ->
            if model.running then
              ({ model | board = step model.board }
              , Cmd.none)
            else
              (model, Cmd.none)

        ClickedPause ->
            ({ model | running = not model.running}, Cmd.none)
        
        ClickedCell c ->
            ({model | board = flipCell model.board c }
            , Cmd.none) --- TODO invert c

--- View ---
pauseGlyph : Char
pauseGlyph = '\u{23F8}'
repeatGlyph : Char
repeatGlyph = '\u{1F501}'

type Shade
  = Dark
  | Light

darkColor : Color
darkColor = rgb 0 0 0
lightColor : Color
lightColor = rgb 1 1 1
cell : Pos -> Shade -> Element Msg
cell p s =
  el [ onClick <| ClickedCell p
     , width <| px 20
     , height <| px 20
     , Border.width 1
     , Border.color <| rgba 0 0 0 0.1
     , Background.color
        <| case s of
             Dark -> darkColor
             Light -> lightColor
      ] none

printBoard : Model -> Html Msg
printBoard model =
  let
    b = model.board

    xs : List Int
    xs = List.range 1 xWidth

    ys : List Int
    ys = List.range 1 yHeight
    
    toRow : Int -> List Pos
    toRow y = List.map (\x -> (x,y)) xs

  in
    layout []
      <| row [] [
          column [] (List.map
                      (\y ->
                      (drawCellRow b (toRow y))) ys)
          , column
              [size 64
              , onClick <| ClickedPause]
              [text (String.fromChar
                      (if model.running then
                        pauseGlyph
                      else
                        repeatGlyph))]]


cellAlive : Board -> Pos -> Bool
cellAlive b c = List.member c b

drawCell : Board -> Pos -> Element Msg
drawCell b p =
  if (cellAlive b p) then
    cell p Dark
  else
    cell p Light

drawCellRow : Board -> List Pos -> Element Msg
drawCellRow b ps =
  row [] (List.map (drawCell b) ps)

view : Model -> Html Msg
view model =
  printBoard model
glider : Board
glider = [(4,4), (4,3), (4,2), (3,4), (2,1)]

init : () -> (Model, Cmd Msg)
init _ = ( Model glider (Time.millisToPosix 0) True
         , Cmd.none)

--- Main ---
main : Program () Model Msg
main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }
