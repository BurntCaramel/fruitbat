module ParseTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Parse exposing (AttributeType(..), Attribute, attribute, attributes, model)
import Parser exposing (run)


suite : Test
suite =
    describe "parsing"
        [ describe "single attribute"
            [ test "attribute with string type" <|
                \_ ->
                    run attribute "name:string"
                        |> Expect.equal
                            (Ok { name = "name", type_ = String, index = False })
            , test "attribute with text type" <|
                \_ ->
                    run attribute "name:text"
                        |> Expect.equal
                            (Ok { name = "name", type_ = Text, index = False })
            , test "attribute with integer type" <|
                \_ ->
                    run attribute "name:integer"
                        |> Expect.equal
                            (Ok { name = "name", type_ = Integer, index = False })
            , test "attribute with decimal type" <|
                \_ ->
                    run attribute "name:decimal"
                        |> Expect.equal
                            (Ok { name = "name", type_ = Decimal, index = False })
            , test "attribute with uppercase type" <|
                \_ ->
                    run attribute "name:INTEGER"
                        |> Expect.equal
                            (Ok { name = "name", type_ = Integer, index = False })
            , test "attribute with missing type" <|
                \_ ->
                    run attribute "name"
                        |> Expect.equal
                            (Ok { name = "name", type_ = String, index = False })
            ]
        , describe "multiple attributes"
            [ test "attributes with specified types" <|
                \_ ->
                    run attributes "first:text second:integer"
                        |> Expect.equal
                            (Ok
                                [ { name = "first", type_ = Text, index = False }
                                , { name = "second", type_ = Integer, index = False }
                                ]
                            )
            , test "attributes with implied types" <|
                \_ ->
                    run attributes "first second"
                        |> Expect.equal
                            (Ok
                                [ { name = "first", type_ = String, index = False }
                                , { name = "second", type_ = String, index = False }
                                ]
                            )
            , test "attributes with specified then implied types" <|
                \_ ->
                    run attributes "first:decimal second"
                        |> Expect.equal
                            (Ok
                                [ { name = "first", type_ = Decimal, index = False }
                                , { name = "second", type_ = String, index = False }
                                ]
                            )
            , test "attributes with specified then implied types; uppercase names" <|
                \_ ->
                    run attributes "first_thing:decimal SecondThing"
                        |> Expect.equal
                            (Ok
                                [ { name = "first_thing", type_ = Decimal, index = False }
                                , { name = "second_thing", type_ = String, index = False }
                                ]
                            )
            , test "attributes with implied then specified types" <|
                \_ ->
                    run attributes "first second:decimal"
                        |> Expect.equal
                            (Ok
                                [ { name = "first", type_ = String, index = False }
                                , { name = "second", type_ = Decimal, index = False }
                                ]
                            )
            , test "attributes with index" <|
                \_ ->
                    run attributes "first:string:index second:decimal:index"
                        |> Expect.equal
                            (Ok
                                [ { name = "first", type_ = String, index = True }
                                , { name = "second", type_ = Decimal, index = True }
                                ]
                            )
            , test "attributes with references" <|
                \_ ->
                    run attributes "first:references second:references"
                        |> Expect.equal
                            (Ok
                                [ { name = "first", type_ = References, index = True }
                                , { name = "second", type_ = References, index = True }
                                ]
                            )
            , test "no attributes" <|
                \_ ->
                    run attributes ""
                        |> Expect.equal
                            (Ok [])
            ]
        , describe "model"
            [ test "model with no attributes" <|
                \_ ->
                    run model "model photo"
                        |> Expect.equal
                            (Ok { name = "photo"
                                , attributes = []
                                }
                            )
            , test "model with attributes" <|
                \_ ->
                    run model "model photo image_url description:text"
                        |> Expect.equal
                            (Ok { name = "photo"
                                , attributes =
                                    [ { name = "image_url", type_ = String, index = False }
                                    , { name = "description", type_ = Text, index = False }
                                    ]
                                }
                            )
            , test "model with attributes; capitalized name" <|
                \_ ->
                    run model "model Photo image_url description:text   "
                        |> Expect.equal
                            (Ok { name = "photo"
                                , attributes =
                                    [ { name = "image_url", type_ = String, index = False }
                                    , { name = "description", type_ = Text, index = False }
                                    ]
                                }
                            )
            , test "model with attributes; all caps name" <|
                \_ ->
                    run model "model PHOTO image_url description:text   "
                        |> Expect.equal
                            (Ok { name = "photo"
                                , attributes =
                                    [ { name = "image_url", type_ = String, index = False }
                                    , { name = "description", type_ = Text, index = False }
                                    ]
                                }
                            )
            , test "model with attributes; camel case name" <|
                \_ ->
                    run model "model MediaPhoto image_url description:text   "
                        |> Expect.equal
                            (Ok { name = "media_photo"
                                , attributes =
                                    [ { name = "image_url", type_ = String, index = False }
                                    , { name = "description", type_ = Text, index = False }
                                    ]
                                }
                            )
            ]
        ]