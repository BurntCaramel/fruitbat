module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (onClick)


model =
    0


type Message
    = Increment
    | Decrement


update msg model =
    case msg of
        Increment ->
            model + 3

        Decrement ->
            model - 1


view model =
    div []
        [ button [ onClick Decrement ] [ text "-" ]
        , text (toString model)
        , button [ onClick Increment ] [ text "+" ]
        ]


main =
    beginnerProgram
        { model = model
        , view = view
        , update = update
        }
