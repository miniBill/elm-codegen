module Declare exposing (suite)

{-| -}

import Elm
import Elm.Annotation as Type
import Elm.Arg
import Elm.Declare
import Elm.Expect
import Elm.Op
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Elm.Declare"
        [ declarations
        , aliasTest
        , aliasWithTest
        , customTypeTest
        , customTypeWithTest
        , newStyleTest
        ]


newStyleTest : Test
newStyleTest =
    describe "New style"
        [ test "usage" <|
            \_ ->
                Elm.Expect.renderedAs inner
                    """\\( a, b ) c -> ( a, b, c )"""
        , test "declaration" <|
            \_ ->
                Elm.Expect.declarationAs newStyle.declaration
                    """name : ( a, b ) -> c -> ( a, b, c )
name ( a, b ) c =
    ( a, b, c )"""
        ]


inner : Elm.Expression
inner =
    Elm.fnBuilder (\( a, b ) c -> Elm.triple a b c)
        |> Elm.fnArg (Elm.Arg.tuple (Elm.Arg.var "a") (Elm.Arg.var "b"))
        |> Elm.fnArg (Elm.Arg.var "c")
        |> Elm.fnDone


newStyle : Elm.Declare.Function (Elm.Expression -> Elm.Expression -> Elm.Expression)
newStyle =
    Elm.Declare.fnX "name" (\( a, b ) c -> Elm.triple a b c)
        |> Elm.Declare.fnArg (Elm.Arg.tuple (Elm.Arg.var "a") (Elm.Arg.var "b"))
        |> Elm.Declare.fnArg (Elm.Arg.var "c")
        |> Elm.Declare.fnDone


myFn : Elm.Declare.Function (Elm.Expression -> Elm.Expression)
myFn =
    Elm.Declare.fn "myFn"
        (Elm.Arg.var "myInt")
        (Elm.Op.plus (Elm.int 5))


declarations : Test
declarations =
    describe "declarations"
        [ test "Basic function dec" <|
            \_ ->
                Elm.Expect.declarationAs myFn.declaration
                    """
myFn : Int -> Int
myFn myInt =
    5 + myInt
"""
        , test "Call correctly" <|
            \_ ->
                Elm.Expect.declarationAs
                    (Elm.declaration "mySweetNumber"
                        (myFn.call (Elm.int 82))
                    )
                    """
mySweetNumber : Int
mySweetNumber =
    myFn 82

"""
        ]


aliasTest : Test
aliasTest =
    test "Elm.alias" <|
        \_ ->
            Elm.Expect.declarationAs
                (Elm.alias "MyAlias"
                    (Type.record
                        [ ( "one", Type.var "oneVar" )
                        , ( "two", Type.var "twoVar" )
                        , ( "three", Type.var "threeVar" )
                        ]
                    )
                )
                """
type alias MyAlias oneVar twoVar threeVar =
    { one : oneVar, two : twoVar, three : threeVar }
"""


aliasWithTest : Test
aliasWithTest =
    test "Elm.aliasWith" <|
        \_ ->
            Elm.Expect.declarationAs
                (Elm.aliasWith "MyAlias"
                    [ "twoVar", "nonexistingVar", "oneVar" ]
                    (Type.record
                        [ ( "one", Type.var "oneVar" )
                        , ( "two", Type.var "twoVar" )
                        , ( "three", Type.var "threeVar" )
                        ]
                    )
                )
                """
type alias MyAlias twoVar oneVar threeVar =
    { one : oneVar, two : twoVar, three : threeVar }
"""


customTypeTest : Test
customTypeTest =
    test "Elm.customType" <|
        \_ ->
            Elm.Expect.declarationAs
                (Elm.customType "MyType"
                    [ Elm.variantWith "One"
                        [ Type.var "oneVar" ]
                    , Elm.variantWith "Two"
                        [ Type.var "twoVar" ]
                    ]
                )
                """
type MyType oneVar twoVar
    = One oneVar
    | Two twoVar
"""


customTypeWithTest : Test
customTypeWithTest =
    test "Elm.customTypeWith" <|
        \_ ->
            Elm.Expect.declarationAs
                (Elm.customTypeWith "MyType"
                    [ "addVar", "twoVar" ]
                    [ Elm.variantWith "One"
                        [ Type.var "oneVar" ]
                    , Elm.variantWith "Two"
                        [ Type.var "twoVar" ]
                    ]
                )
                """
type MyType addVar twoVar oneVar
    = One oneVar
    | Two twoVar
"""
