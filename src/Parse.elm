module Parse exposing
    ( AttributeType(..)
    , ModelDefinition
    , GenerateCommand(..)
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


type alias ModelDefinition =
    { name : String
    , attributes: List (String, AttributeType)
    }


type GenerateCommand =
    Model ModelDefinition


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
        || c == '_'


word : Parser String
word =
    source <|
        ignore (Exactly 1) isWordHeadChar
            |. ignore zeroOrMore isWordChar


name : Parser String
name =
    inContext "name" <|
        succeed String.Extra.underscored
            |= word


attributeTypeStrings : Dict String AttributeType
attributeTypeStrings =
    Dict.fromList
        [ ("string", String)
        , ("text", Text)
        , ("integer", Integer)
        , ("decimal", Decimal)
        , ("float", Float)
        , ("boolean", Boolean)
        , ("timestamp", Timestamp)
        , ("date", Date)
        , ("datetime", DateTime)
        , ("binary", Binary)
        , ("references", References)
        ]


attributeTypeFromString : String -> Maybe AttributeType
attributeTypeFromString s =
    Dict.get (String.toLower s) attributeTypeStrings


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


attribute : Parser (String, AttributeType)
attribute =
    succeed (,)
        |= name
        |= oneOf
            [ succeed identity
                |. symbol ":"
                |= attributeType
            , succeed String
            ]


attributesHelp : List (String, AttributeType) -> Parser (List (String, AttributeType))
attributesHelp revAttributes =
    oneOf
        [ attribute
            |. ignore zeroOrMore isSpace
            |> andThen (\a -> attributesHelp (a :: revAttributes))
        , lazy (\_ -> succeed (List.reverse revAttributes))
        ]


attributes : Parser (List (String, AttributeType))
attributes =
    inContext "attributes" <|
        succeed identity
            |. ignore zeroOrMore isSpace
            |= attributesHelp []


model : Parser ModelDefinition
model =
    inContext "model" <|
        succeed ModelDefinition
            |. ignore zeroOrMore isSpace
            |= name
            |. ignore zeroOrMore isSpace
            |= attributes


generateCommand : Parser GenerateCommand
generateCommand =
    oneOf
        [ succeed Model
            |= model
        ]


nextCommand : Parser GenerateCommand
nextCommand =
    delayedCommit (ignore zeroOrMore isNewline) <|
        generateCommand


commandsHelp : List GenerateCommand -> Parser (List GenerateCommand)
commandsHelp revCommands =
    oneOf
        [ nextCommand
            |> andThen (\command -> commandsHelp (command :: revCommands))
        , succeed (List.reverse revCommands)
        ]


commands : Parser (List GenerateCommand)
commands =
    inContext "commands" <|
        succeed identity
            -- |= generateCommand
            |. ignore zeroOrMore isNewline
            |= commandsHelp []


parseGenerateCommands : String -> Result Parser.Error (List GenerateCommand)
parseGenerateCommands input =
    run commands input
