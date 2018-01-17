module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, rows)
import Html.Events exposing (onClick, onInput)
import Parse exposing (AttributeType(..), ModelDefinition, Scaffold(..), parseScaffold)


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


viewAttribute : (String, AttributeType) -> Html Message
viewAttribute attribute =
    tr []
        [ td [ class "border border-black" ]
            [ text (attribute |> Tuple.first) ]
        , td [ class "border border-black" ]
            [ text (attribute |> Tuple.second |> toString) ]
        ]


viewAttributes : List (String, AttributeType) -> Html Message
viewAttributes attributes =
    table []
        [ tbody []
            (List.map viewAttribute attributes)
        ]


viewModelDefinition : ModelDefinition -> Html Message
viewModelDefinition model =
    div []
        [ h2 [] [ text model.name ]
        , viewAttributes model.attributes
        ]


viewScaffold : Scaffold -> Html Message
viewScaffold scaffold =
    case scaffold of
        Model model ->
            viewModelDefinition model


view : Model -> Html Message
view model =
    let
        scaffoldResult =
            parseScaffold model.input
        
        resultHtml =
            case scaffoldResult of
                Ok scaffold ->
                    div []
                        [ text (toString scaffold)
                        , viewScaffold scaffold
                        ]
                
                Err error ->
                    text "Error"
    in
        div [ class "p-4 relative" ]
            [ textarea [ class "w-full p-0 leading-normal font-mono", rows 8, onInput ChangeInput ] [ text model.input ]
            , div [ class "absolute pin-t w-full pt-4 leading-normal font-mono font-bold", rows 8, onInput ChangeInput ] [ text model.input ]
            , resultHtml
            ]


main : Program Never Model Message
main =
    beginnerProgram
        { model = model
        , view = view
        , update = update
        }
