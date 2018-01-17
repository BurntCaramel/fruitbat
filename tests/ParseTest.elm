module ParseTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Parse exposing (AttributeType(..), attribute, attributes, model)
import Parser exposing (run)


suite : Test
suite =
    describe "parsing"
        [ describe "single attribute"
            [ test "attribute with string type" <|
                \_ ->
                    run attribute "name:string"
                        |> Expect.equal
                            (Ok ("name", String))
            , test "attribute with text type" <|
                \_ ->
                    run attribute "name:text"
                        |> Expect.equal
                            (Ok ("name", Text))
            , test "attribute with integer type" <|
                \_ ->
                    run attribute "name:integer"
                        |> Expect.equal
                            (Ok ("name", Integer))
            , test "attribute with decimal type" <|
                \_ ->
                    run attribute "name:decimal"
                        |> Expect.equal
                            (Ok ("name", Decimal))
            , test "attribute with uppercase type" <|
                \_ ->
                    run attribute "name:INTEGER"
                        |> Expect.equal
                            (Ok ("name", Integer))
            , test "attribute with missing type" <|
                \_ ->
                    run attribute "name"
                        |> Expect.equal
                            (Ok ("name", String))
            ]
        , describe "multiple attributes"
            [ test "attributes with specified types" <|
                \_ ->
                    run attributes "first:text second:integer"
                        |> Expect.equal
                            (Ok [("first", Text), ("second", Integer)])
            , test "attributes with specified types; spaces" <|
                \_ ->
                    run attributes "   first:text second:integer   "
                        |> Expect.equal
                            (Ok [("first", Text), ("second", Integer)])
            , test "attributes with implied types" <|
                \_ ->
                    run attributes "first second"
                        |> Expect.equal
                            (Ok [("first", String), ("second", String)])
            , test "attributes with implied types; spaces" <|
                \_ ->
                    run attributes "  first second  "
                        |> Expect.equal
                            (Ok [("first", String), ("second", String)])
            , test "attributes with specified then implied types" <|
                \_ ->
                    run attributes "first:decimal second"
                        |> Expect.equal
                            (Ok [("first", Decimal), ("second", String)])
            , test "attributes with specified then implied types; spaces" <|
                \_ ->
                    run attributes "  first:decimal second  "
                        |> Expect.equal
                            (Ok [("first", Decimal), ("second", String)])
            , test "attributes with specified then implied types; spaces, uppercase names" <|
                \_ ->
                    run attributes "  first_thing:decimal SecondThing  "
                        |> Expect.equal
                            (Ok [("first_thing", Decimal), ("second_thing", String)])
            , test "attributes with implied then specified types" <|
                \_ ->
                    run attributes "first second:decimal"
                        |> Expect.equal
                            (Ok [("first", String), ("second", Decimal)])
            , test "attributes with implied then specified types; spaces" <|
                \_ ->
                    run attributes "  first second:decimal  "
                        |> Expect.equal
                            (Ok [("first", String), ("second", Decimal)])
            , test "no attributes" <|
                \_ ->
                    run attributes ""
                        |> Expect.equal
                            (Ok [])
            , test "no attributes; spaces" <|
                \_ ->
                    run attributes "   "
                        |> Expect.equal
                            (Ok [])
            ]
        , describe "model"
            [ test "model with no attributes" <|
                \_ ->
                    run model "photo"
                        |> Expect.equal
                            (Ok { name = "photo"
                                , attributes = []
                                }
                            )
            , test "model with attributes" <|
                \_ ->
                    run model "photo image_url description:text"
                        |> Expect.equal
                            (Ok { name = "photo"
                                , attributes =
                                    [ ("image_url", String)
                                    , ("description", Text)
                                    ]
                                }
                            )
            , test "model with attributes; capitalized name" <|
                \_ ->
                    run model "   Photo image_url description:text   "
                        |> Expect.equal
                            (Ok { name = "photo"
                                , attributes =
                                    [ ("image_url", String)
                                    , ("description", Text)
                                    ]
                                }
                            )
            , test "model with attributes; all caps name" <|
                \_ ->
                    run model "   PHOTO image_url description:text   "
                        |> Expect.equal
                            (Ok { name = "photo"
                                , attributes =
                                    [ ("image_url", String)
                                    , ("description", Text)
                                    ]
                                }
                            )
            , test "model with attributes; camel case name" <|
                \_ ->
                    run model "   MediaPhoto image_url description:text   "
                        |> Expect.equal
                            (Ok { name = "media_photo"
                                , attributes =
                                    [ ("image_url", String)
                                    , ("description", Text)
                                    ]
                                }
                            )
            ]
        ]