module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, rows)
import Html.Events exposing (onClick, onInput)
import Parse exposing (AttributeType(..), Attribute, ModelDefinition, GenerateCommand(..), parseGenerateCommands)


type alias Model =
    { input : String
    }


model : Model
model =
    { input = """model user email:string
model photo user:references description:text image_url:string
"""
    }


type Message
    = ChangeInput String


update : Message -> Model -> Model
update msg model =
    case msg of
        ChangeInput newInput ->
            { model | input = newInput }


viewAttribute : Parse.Attribute -> Html Message
viewAttribute attribute =
    tr []
        [ td [ class "w-48 px-2 py-1 border border-blue-light" ]
            [ text (attribute.name) ]
        , td [ class "w-32 px-2 py-1 border border-blue-light" ]
            [ text (attribute.type_ |> toString) ]
        ]


viewAttributes : List Parse.Attribute -> Html Message
viewAttributes attributes =
    table []
        [ tbody []
            (List.map viewAttribute attributes)
        ]


viewModelDefinition : ModelDefinition -> Html Message
viewModelDefinition model =
    div [ class "mb-4" ]
        [ h2 [] [ text model.name ]
        , viewAttributes model.attributes
        ]


viewGenerateCommand : GenerateCommand -> Html Message
viewGenerateCommand generateCommand =
    case generateCommand of
        Model model ->
            viewModelDefinition model


viewGenerateCommands : List GenerateCommand -> Html Message
viewGenerateCommands generateCommands =
    div []
        (List.map viewGenerateCommand generateCommands)


view : Model -> Html Message
view model =
    let
        generateCommandsResult =
            parseGenerateCommands model.input
        
        resultHtml =
            case generateCommandsResult of
                Ok generateCommands ->
                    div []
                        [ text ""--(toString generateCommands)
                        , viewGenerateCommands generateCommands
                        ]
                
                Err error ->
                    text ("Error: " ++ (toString error))
    in
        div [ class "p-4 relative" ]
            [ h2 [] [ text "rails generate…" ]
            , textarea [ class "w-full p-0 leading-normal font-mono", rows 8, onInput ChangeInput ] [ text model.input ]
            -- , div [ class "absolute pin-t w-full pt-4 leading-normal font-mono font-bold whitespace-pre", rows 8, onInput ChangeInput ] [ text model.input ]
            , resultHtml
            ]


main : Program Never Model Message
main =
    beginnerProgram
        { model = model
        , view = view
        , update = update
        }
