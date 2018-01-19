module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, rows)
import Html.Events exposing (onClick, onInput)
import Parse exposing (AttributeType(..), Attribute, ModelDefinition, GenerateCommand(..), attributeTypeStrings, parseGenerateCommands)
import Inflexio.Pluralize exposing (singularize, pluralize)
import String.Extra exposing (classify)
import Parser


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
        [ td [ class "w-48 px-2 py-1 border border-blue-light bg-blue-lightest" ]
            [ text (attribute.name) ]
        , td [ class "w-48 px-2 py-1 border border-blue-light text-white bg-blue" ]
            [ text (attribute.type_ |> toString)
            , if attribute.type_ == References then
                strong [] [ text <| " " ++ pluralize attribute.name ]
              else
                text ""
            , text <|
                if attribute.index then
                    "*"
                else
                    ""
            ]
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
        [ h2 [ class "mb-1" ]
            [ text <| pluralize <| model.name
            , text " "
            , text <| classify <| singularize <| model.name
            ]
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


suggestionsForContextDescription : String -> List String
suggestionsForContextDescription name =
    case name of
        "command" ->
            [ "model [name] [attributes…]" ]

        "generate command" ->
            [ "model" ]

        "attribute type" ->
            attributeTypeStrings

        "attribute index" ->
            [ "index" ]

        _ ->
            []


viewGenerateCommandsError : Parser.Error -> Html Message
viewGenerateCommandsError error =
    case error.context of
        context :: _ ->
            let
                summaryHtml =
                    case error.problem of
                        Parser.ExpectingEnd ->
                            div []
                                [ text "Incomplete or invalid "
                                , strong [] [ text context.description ]
                                , text "."
                                ]

                        _ ->
                            div []
                                [ text "Invalid or missing "
                                , strong [] [ text context.description ]
                                , text "."
                                ]

                suggestions =
                    suggestionsForContextDescription context.description

                suggestionsHtml =
                    if List.isEmpty suggestions then
                        []
                    else
                        [ text "Suggestions: "
                        , suggestions
                            |> String.join ", "
                            |> text
                            |> List.singleton
                            |> strong []
                        ]
            in
                div []
                    [ summaryHtml
                    , div [] suggestionsHtml
                    ]

        _ ->
            div [] [ text (toString error.problem) ]


view : Model -> Html Message
view model =
    let
        generateCommandsResult =
            parseGenerateCommands model.input

        resultHtml =
            case generateCommandsResult of
                Ok generateCommands ->
                    div []
                        [ text "" --(toString generateCommands)
                        , viewGenerateCommands generateCommands
                        ]

                Err error ->
                    div [ class "leading-normal" ]
                        [ viewGenerateCommandsError error

                        -- , text (toString error)
                        ]
    in
        div [ class "p-4 relative" ]
            [ h2 [] [ text "rails generate…" ]
            , textarea [ class "w-full p-2 leading-normal font-mono bg-purple-lightest", rows 8, onInput ChangeInput ] [ text model.input ]

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
