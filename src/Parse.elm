module Parse
    exposing
        ( AttributeType(..)
        , Attribute
        , ModelDefinition
        , GenerateCommand(..)
        , attributeTypeStrings
        , attribute
        , attributes
        , model
        , parseGenerateCommands
        )

import String
import Char
import Dict exposing (Dict)
import Parser exposing (..)
import String.Extra


type AttributeType
    = String
    | Text
    | Integer
    | Decimal
    | Float
    | Boolean
    | Timestamp
    | Date
    | DateTime
    | Time
    | Binary
    | References


type alias Attribute =
    { name : String
    , type_ : AttributeType
    , index : Bool
    }


columnNameForAttribute : Attribute -> String
columnNameForAttribute attribute =
    case attribute.type_ of
        References ->
            attribute.name ++ "_id"

        _ ->
            attribute.name


type alias ModelDefinition =
    { name : String
    , attributes : List Attribute
    }


type GenerateCommand
    = Model ModelDefinition


isSpace : Char -> Bool
isSpace c =
    c == ' '


isNewline : Char -> Bool
isNewline c =
    c == '\n'


isWordHeadChar : Char -> Bool
isWordHeadChar c =
    Char.isLower c
        || Char.isUpper c


isWordChar : Char -> Bool
isWordChar c =
    Char.isLower c
        || Char.isUpper c
        || Char.isDigit c
        || c
        == '_'


word : Parser String
word =
    source <|
        ignore (Exactly 1) isWordHeadChar
            |. ignore zeroOrMore isWordChar


name : Parser String
name =
    succeed String.Extra.underscored
        |= word


attributeTypeStringsTable : List ( String, AttributeType )
attributeTypeStringsTable =
    [ ( "string", String )
    , ( "text", Text )
    , ( "integer", Integer )
    , ( "decimal", Decimal )
    , ( "float", Float )
    , ( "boolean", Boolean )
    , ( "timestamp", Timestamp )
    , ( "date", Date )
    , ( "datetime", DateTime )
    , ( "time", Time )
    , ( "binary", Binary )
    , ( "references", References )
    ]


attributeTypeStrings : List String
attributeTypeStrings =
    attributeTypeStringsTable
        |> List.map Tuple.first


attributeTypeStringsDict : Dict String AttributeType
attributeTypeStringsDict =
    Dict.fromList attributeTypeStringsTable


attributeTypeFromString : String -> Maybe AttributeType
attributeTypeFromString s =
    Dict.get (String.toLower s) attributeTypeStringsDict


attributeType : Parser AttributeType
attributeType =
    let
        fromString : String -> Parser AttributeType
        fromString s =
            case attributeTypeFromString s of
                Just attributeType ->
                    succeed attributeType

                Nothing ->
                    fail ("Invalid attribute type: " ++ s)

        parser : Parser AttributeType
        parser =
            succeed identity
                |= word
                |> andThen fromString
    in
        parser


attributeIndex : Parser Bool
attributeIndex =
    keyword "index"
        |> map (\_ -> True)


attribute : Parser Attribute
attribute =
    succeed Attribute
        |= name
        |= oneOf
            [ succeed identity
                |. symbol ":"
                |= attributeType
                |> inContext "attribute type"
            , succeed String -- default type
            ]
        |= oneOf
            [ succeed identity
                |. symbol ":"
                |= attributeIndex
                |> inContext "attribute index"
            , succeed False -- default index
            ]
        |> map
            (\a ->
                case a.type_ of
                    References ->
                        { a | index = True }

                    _ ->
                        a
            )


nextAttribute : Parser Attribute
nextAttribute =
    delayedCommit (ignore oneOrMore isSpace) <|
        succeed identity
            |= attribute


attributesHelp : List Attribute -> Parser (List Attribute)
attributesHelp revAttributes =
    oneOf
        [ nextAttribute
            |> andThen (\a -> attributesHelp (a :: revAttributes))
        , lazy (\_ -> succeed (List.reverse revAttributes))
        ]


attributes : Parser (List Attribute)
attributes =
    inContext "attributes" <|
        succeed identity
            |= oneOf
                [ succeed identity
                    |= andThen (\a -> attributesHelp [ a ]) attribute
                , succeed []
                    |. ignore zeroOrMore isSpace
                ]


model : Parser ModelDefinition
model =
    let
        modelName =
            inContext "model name" <|
                succeed identity
                    |. ignore oneOrMore isSpace
                    |= name
    in
        inContext "model" <|
            succeed ModelDefinition
                |. keyword "model"
                |= modelName
                |= delayedCommit (ignore zeroOrMore isSpace) attributes
                |. ignore zeroOrMore isSpace


generateCommand : Parser GenerateCommand
generateCommand =
    inContext "generate command" <|
        succeed identity
            |= oneOf
                [ succeed Model
                    |= model
                ]


nextCommand : Parser GenerateCommand
nextCommand =
    delayedCommit (ignore oneOrMore isNewline) <|
        succeed identity
            |. ignore zeroOrMore isSpace
            |= generateCommand


commandsHelp : List GenerateCommand -> Parser (List GenerateCommand)
commandsHelp revCommands =
    oneOf
        [ nextCommand
            |> andThen (\command -> commandsHelp (command :: revCommands))
        , succeed (List.reverse revCommands)
        ]


commands : Parser (List GenerateCommand)
commands =
    inContext "command" <|
        succeed identity
            |. ignore zeroOrMore isNewline
            |= oneOf
                [ succeed identity
                    |= andThen (\c -> commandsHelp [ c ]) generateCommand
                , succeed []
                    |. ignore zeroOrMore isSpace
                ]
            |. ignore zeroOrMore isNewline
            |. end


parseGenerateCommands : String -> Result Parser.Error (List GenerateCommand)
parseGenerateCommands input =
    run commands input
