module Conduit exposing (main)

import Array exposing (Array) 
import Browser as Browser
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Html exposing (Html)


primary : Color
primary =
    rgb 0.25 0.8 0.25


white : Color
white =
    rgb 1 1 1


black : Color
black =
    rgb 0 0 0


grey : Color
grey =
    rgba 0 0 0 0.1


banner : Element a
banner =
    column
        [ width fill
        , paddingXY 0 35
        , Background.color primary
        , Font.center
        , Font.color white
        ]
        [ el
            [ width fill
            , height <| px 100
            , Font.size 64
            , Font.semiBold
            , Font.glow black 1.25
            ]
          <|
            text "conduit"
        , el
            [ width fill
            , Font.size 24
            , Font.extraLight
            ]
          <|
            text "A place to share your funky Elm knowledge."
        ]


feed : Element a
feed =
    column [ padding 15 ]
        [ text "ohai"
        , text "nice to see ya"
        ]


viewTag : Tag -> Element a
viewTag t =
    link
        [ Background.color grey
        , Border.rounded 10
        , padding 5
        ]
        { url = "./" ++ String.toLower t
        , label = text t
        }


popularTags : Tags -> Element a
popularTags ts =
    column
        [ alignRight
        , Border.rounded 6
        , Background.color grey
        , padding 15
        ]
        [ el
            [ paddingXY 0 5
            , Font.bold
            ]
          <|
            text "Popular Tags"
        , wrappedRow
            [ width <| px 150 ]
          <|
            List.map viewTag ts
        ]


app : Element a
app =
    row
        [ width fill
        , paddingXY 40 20
        ]
        [ feed
        , popularTags <| List.sortBy String.length sampleTags
        ]


container : Element a
container =
    column [ width fill ]
        [ banner
        , app
        ]

view : Model -> Html Msg
view model =
    layout [ width fill ] container

--- Update ---


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        TagSelected Nothing ->
            (model, Cmd.none)

        TagSelected (Just t) ->
            ({ model | selectedTag = Just t }, Cmd.none)

        ArticleSelected a ->
            ({ model | articleSet = [ a ], selectedTag = Nothing }, Cmd.none)


type Msg
    = TagSelected (Maybe Tag)
    | ArticleSelected Article



--- Model ----


type alias Model =
    { statusText : String
    , selectedTag : Maybe Tag
    , articleSet : Articles
    }


sampleModel : Model
sampleModel =
    { statusText = "Ready"
    , selectedTag = Just "Elm"
    , articleSet = []
    }


type alias Tag =
    String


type alias Tags =
    List Tag


sampleTags : Tags
sampleTags =
    [ "Elm"
    , "Programming"
    , "Sun"
    , "Oak"
    , "Functional"
    ]


type alias Article =
    { title : String
    , author : User
    , tags : Tags
    , body : String
    }


type alias Articles =
    List Article


type alias User =
    { handle : String
    , displayName : String
    , email : String
    }



--- Main ---


main : Program () Model Msg
main =
    Browser.element { init = \flags -> (sampleModel, Cmd.none)
    , view = view
    , update = update
    , subscriptions = \model -> Sub.none }
