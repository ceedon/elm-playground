module BinarySearch exposing (..)

import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input exposing (button)
import Html exposing (Html)



-- Model


type alias Model =
    { lower : Int
    , upper : Int
    , state : State
    }


init : Model
init =
    { lower = 0
    , upper = 1000
    , state = Picking
    }


guess : Model -> Int
guess model =
    (model.lower + model.upper) // 2



-- Update


type Msg
    = Chose
    | GotIt Int
    | TooLow Int
    | TooHigh Int


type State
    = Picking
    | Playing
    | Won


update : Msg -> Model -> Model
update msg model =
    case msg of
        Chose ->
            { model | state = Playing }

        GotIt _ ->
            { model | state = Won }

        TooLow n ->
            { model | lower = n + 1 }

        TooHigh n ->
            { model | upper = n - 1 }



-- View


view : Model -> Html Msg
view model =
    layout [] <|
        case model.state of
            Picking ->
                pickingView model

            Playing ->
                playingView model

            Won ->
                wonView model


pickingView : Model -> Element Msg
pickingView model =
    column
        []
        [ prompt model
        , pickedButton
        ]


prompt : Model -> Element Msg
prompt model =
    "Pick a number between "
        ++ String.fromInt model.lower
        ++ " and "
        ++ String.fromInt model.upper
        ++ " , and I'll try to guess it once you're ready."
        |> text


pickedButton : Element Msg
pickedButton =
    button
        buttonStyle
        { onPress = Just Chose
        , label = text "Ready"
        }


buttonStyle : List (Attribute msg)
buttonStyle =
    [ Background.color <| rgb 0 0.5 0.5
    , padding 20
    , Border.width 5
    , Border.color <| rgb 1 1 1
    ]


playingView : Model -> Element Msg
playingView model =
    let
        n =
            guess model
    in
    column []
        [ guessText n
        , guessButtons n
        ]


guessTextStyle : List (Attribute Msg)
guessTextStyle =
    [ Font.extraBold
    , Font.size 32
    ]


guessText : Int -> Element Msg
guessText n =
    String.fromInt n
        |> text
        |> el guessTextStyle


guessButtons : Int -> Element Msg
guessButtons n =
    row []
        [ lowButton n
        , gotItButton n
        , highButton n
        ]


lowButton : Int -> Element Msg
lowButton n =
    button
        buttonStyle
        { onPress = Just (TooLow n)
        , label = text "Too low..."
        }


gotItButton : Int -> Element Msg
gotItButton n =
    button
        buttonStyle
        { onPress = Just (GotIt n)
        , label = text "That's my number!"
        }


highButton : Int -> Element Msg
highButton n =
    button
        buttonStyle
        { onPress = Just (TooHigh n)
        , label = text "Too high..."
        }


wonView : Model -> Element Msg
wonView model =
    guess model
        |> String.fromInt
        |> (++) "Your number was "
        |> text
        |> el
            guessTextStyle


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }
