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
import String.Extra exposing (..)


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
    -- inContext "name" <|
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
        inContext "attribute type" parser


attributeIndex : Parser Bool
attributeIndex =
    oneOf
        [ keyword "index"
            |> map (\_ -> True)
        , succeed False
        ]


attribute : Parser Attribute
attribute =
    succeed Attribute
        |= name
        |= oneOf
            [ delayedCommit (symbol ":") attributeType
            , succeed String
            ]
        |= oneOf
            [ delayedCommit (symbol ":") attributeIndex
            , succeed False
            ]


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
        tableName =
            inContext "table name" <|
                succeed identity
                    |. ignore zeroOrMore isSpace
                    |= name
    in
        inContext "model" <|
            succeed ModelDefinition
                |= tableName
                |= delayedCommit (ignore zeroOrMore isSpace) attributes
                |. ignore zeroOrMore isSpace


generateCommand : Parser GenerateCommand
generateCommand =
    inContext "generate command" <|
        succeed identity
            |= oneOf
                [ succeed Model
                    |. keyword "model"
                    -- |. ignore oneOrMore isSpace
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
            -- |. ignore oneOrMore isNewline
            |> andThen (\command -> commandsHelp (command :: revCommands))
        , succeed (List.reverse revCommands)
        ]


commands : Parser (List GenerateCommand)
commands =
    inContext "commands" <|
        succeed identity
            |. ignore zeroOrMore isNewline
            -- |= commandsHelp []
            |= oneOf
                [ succeed identity
                    |= andThen (\c -> commandsHelp [ c ]) generateCommand
                , succeed []
                    |. ignore zeroOrMore isSpace
                ]


parseGenerateCommands : String -> Result Parser.Error (List GenerateCommand)
parseGenerateCommands input =
    run commands input
