module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }


type alias Model =
    { catName : String
    , diagnosis : String
    , treatment : String
    , careContact : String
    , startDate : String
    , endDate : String
    }


init : Model
init =
    { catName = ""
    , diagnosis = ""
    , treatment = ""
    , careContact = ""
    , startDate = ""
    , endDate = ""
    }


type Msg
    = NoOp


update : Msg -> Model -> Model
update msg model =
    case msg of
        NoOp ->
            model


view : Model -> Html Msg
view model =
    div [ class "app" ]
        [ h1 [] [ text "Meowderall" ]
        , p [ class "tagline" ] [ text "Cat medication tracking for shelters" ]
        , p [ class "notice" ] [ text "Data is stored locally in your browser only." ]
        ]
