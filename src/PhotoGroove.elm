module PhotoGroove exposing (main)

import Browser exposing (sandbox)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onClick)
import Html exposing (Html)
import Html.Attributes exposing (selected)


type alias Photo =
    { url : String, description : String }


type Msg
    = Clicked Photo


type alias Model =
    { photos : List Photo, selected : Photo }


initialModel : Model
initialModel =
    { photos =
        [ { url = "1.jpeg", description = "One" }
        , { url = "2.jpeg", description = "Two" }
        , { url = "3.jpeg", description = "Three" }
        ]
    , selected = { url = "1.jpeg", description = "One" }
    }


view : Model -> Html Msg
view model =
    layout [ Background.color background ] <|
        row []
            [ responsiveRow <|
                List.map
                    (viewThumbnail eiaUrl model.selected)
                    model.photos
            , column
                []
                [ image
                    []
                    { src = eiaUrl ++ "large/" ++ model.selected.url, description = model.selected.description }
                ]
            ]


background : Color
background =
    rgb 0.2 0.2 0.2


accent : Color
accent =
    rgb 0 1 0


eiaUrl : String
eiaUrl =
    "https://elm-in-action.com/"


viewThumbnail : String -> Photo -> Photo -> Element Msg
viewThumbnail prefix selected thumb =
    image
        [ Border.width 1
        , Border.color <|
            if selected.url == thumb.url then
                accent

            else
                background
        , onClick <| Clicked thumb
        ]
        { src = prefix ++ thumb.url
        , description = thumb.description
        }


responsiveRow : List (Element msg) -> Element msg
responsiveRow elements =
    Element.wrappedRow
        [ Element.centerY, Element.padding 10, Element.spacing 10, Element.centerX ]
        elements



--- Update ---


update : Msg -> Model -> Model
update msg model =
    case msg of
        Clicked thumb ->
            { model | selected = thumb }



--- Main ---


main : Program () Model Msg
main =
    sandbox
        { init = initialModel
        , view = view
        , update = update
        }
