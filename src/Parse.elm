module Parse exposing
    ( AttributeType(..)
    , attribute
    , attributes
    )

import String
import Char
import Set
import Dict exposing (Dict)
import Parser exposing (..)
import Parser.LanguageKit exposing (variable)


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


isSpace : Char -> Bool
isSpace c =
    c == ' '


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


spaces : Parser ()
spaces =
    ignore oneOrMore isSpace


optionalSpaces : Parser ()
optionalSpaces =
    ignore zeroOrMore isSpace


word : Parser String
word =
    variable isWordHeadChar isWordChar Set.empty


modelName : Parser String
modelName =
    inContext "model name" <|
        word


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
        |= word
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
            |. optionalSpaces
            |> andThen (\a -> attributesHelp (a :: revAttributes))
        , succeed (List.reverse revAttributes)
        ]


attributes : Parser (List (String, AttributeType))
attributes =
    inContext "attributes" <|
        succeed identity
            |. optionalSpaces
            |= attributesHelp []

