module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick, onInput)
import Parse exposing (parseScaffold)


type alias Model =
    { input : String
    }


model : Model
model =
    { input = ""
    }


type Message
    = ChangeInput String


update : Message -> Model -> Model
update msg model =
    case msg of
        ChangeInput newInput ->
            { model | input = newInput }


view : Model -> Html Message
view model =
    let
        scaffold =
            parseScaffold model.input
    in
        div []
            [ input [ class "w-full", onInput ChangeInput ] [ text "-" ]
            , text (toString scaffold)
            ]


main : Program Never Model Message
main =
    beginnerProgram
        { model = model
        , view = view
        , update = update
        }
