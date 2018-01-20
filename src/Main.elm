module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, href, rows)
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
    let
        typeClass =
            case attribute.type_ of
                References ->
                    "bg-purple border-purple-light"
                
                _ ->
                    "bg-blue border-blue-light"
    in
        tr [ class "leading-base" ]
            [ td [ class "w-48 px-2 py-1 text-blue-darkest bg-blue-lightest border border-blue-light" ]
                [ text (attribute.name) ]
            , td [ class <| "w-48 px-2 py-1 border text-white " ++ typeClass ]
                [ text (attribute.type_ |> toString)
                , if attribute.type_ == References then
                    strong [] [ text <| " " ++ pluralize attribute.name ]
                else
                    text ""
                ]
            , if attribute.index then
                td [ class "w-16 pl-2 py-1 text-sm text-white bg-grey-darkest border border-grey-darkest" ]
                    [ text <|
                        if attribute.index then
                            "indexed"
                        else
                            ""
                    ]
            else
                text ""
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


codeForAttribute : Parse.Attribute -> Maybe String
codeForAttribute attribute =
    case attribute.type_ of
        References ->
            Just <| "  belongs_to :" ++ attribute.name
        
        _ ->
            Nothing


viewAppCodeForModel : ModelDefinition -> Html Message
viewAppCodeForModel model =
    let
        className =
            model.name
                |> singularize
                |> classify
        
        codeLines =
            List.filterMap codeForAttribute model.attributes
        
        methodsCode =
            if List.isEmpty codeLines then
                ""
            else
                String.join "\n" codeLines ++ "\n"
    in
        
    div []
        [ h3 [ class "mt-1" ]
            [ text <| className ++ ".rb" ]
        , textarea [ rows (List.length codeLines + 2), class "w-full p-2 mb-4 leading-normal font-mono bg-red-lightest" ]
            [ text <|
                "class " ++ className ++ " < ApplicationRecord" ++ "\n"
                ++ methodsCode
                ++ "end"
            ]
        ]


viewAppCodeSection : List GenerateCommand -> Html Message
viewAppCodeSection generateCommands =
    let
        models =
            List.filterMap (\command ->
                case command of
                    Model model ->
                        Just model
            ) generateCommands
    in
        
    div []
        [ h2 [] [ text "app/models/" ]
        , div [ class "pl-4" ]
            (List.map viewAppCodeForModel models)
        ]


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
                        [ viewGenerateCommands generateCommands
                        , viewAppCodeSection generateCommands
                        ]

                Err error ->
                    div [ class "leading-normal" ]
                        [ viewGenerateCommandsError error
                        -- , text (toString error)
                        ]
    in
        section []
            [ nav [ class "flex justify-between px-4 py-1 text-black border-b border-grey-lighter" ]
                [ h1 [ class "text-lg" ] [ text "Fruitbat 🦇" ]
                , div [ class "text-sm leading-normal" ]
                    [ a [ href "https://twitter.com/concreteniche" ] [ text "by @concreteniche" ]
                    , a [ href "https://github.com/RoyalIcing/fruitbat" ] [ text "Fork on GitHub" ]
                    ]
                ]
            , div [ class "p-4 relative" ]
                [ h2 [ class "" ] [ text "rails generate …" ]
                , textarea [ class "w-full p-2 mb-4 leading-normal font-mono bg-purple-lightest", rows 8, onInput ChangeInput ] [ text model.input ]

                -- , div [ class "absolute pin-t w-full pt-4 leading-normal font-mono font-bold whitespace-pre", rows 8, onInput ChangeInput ] [ text model.input ]
                , resultHtml
                ]
            ]


main : Program Never Model Message
main =
    beginnerProgram
        { model = model
        , view = view
        , update = update
        }
