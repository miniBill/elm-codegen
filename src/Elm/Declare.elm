module Elm.Declare exposing
    ( Function, fn, fn2, fn3, fn4, fn5, fn6
    , Value, value
    , function
    , Module, module_
    , with, withUnexposed, placeholder
    , Annotation, alias, customType
    , Internal
    , toFile
    )

{-| You may run into situations where you want to generate a function, and then call that generated function somewhere else.

This module will help you do that.

Here's an example, let's define a new function called `add42`

    renderFile =
        let
            add42 =
                Elm.Declare.fn "add42"
                    ( "firstInt", Nothing )
                    (\firstArgument ->
                        Elm.plus
                            (Elm.int 42)
                            firstArgument
                    )
        in
        Elm.file [ "MyFile" ]
            -- add our declaration to our file
            [ add42.declaration

            -- and another place where we call that function!
            , Elm.declaration "mySweetNumber"
                (add42.call (Elm.int 82))
            ]

Depending on your situation, you may want to define a function in one file, but call it from another.

In that case you can do something like this using `callFrom`:

    renderFileList =
        let
            add42 =
                Elm.Declare.fn "add42"
                    ( "firstInt", Nothing )
                    (\firstArgument ->
                        Elm.plus
                            (Elm.int 42)
                            firstArgument
                    )
        in
        [ Elm.file [ "MyFile" ]
            -- add our declaration to our file
            [ add42.declaration
            ]
        , Elm.file [ "MyOtherFile" ]
            -- and call from another file
            [ Elm.declaration "mySweetNumber"
                (add42.callFrom [ "MyFile" ] (Elm.int 82))
            ]
        ]

@docs Function, fn, fn2, fn3, fn4, fn5, fn6

@docs Value, value

@docs function

@docs Module, module_

@docs with, withUnexposed, placeholder

@docs Annotation, alias, customType

@docs Internal

@docs toFile

-}

import Elm exposing (Expression)
import Elm.Annotation
import Elm.Arg
import Internal.Format as Format


{-| -}
type alias Module val =
    { name : List String
    , declarations : List Elm.Declaration
    , call : val
    }


{-| -}
type alias Annotation =
    { annotation : Elm.Annotation.Annotation
    , declaration : Elm.Declaration
    , internal : Internal Elm.Annotation.Annotation
    }


{-| -}
type alias Function tipe =
    { call : tipe
    , value : Elm.Expression
    , declaration : Elm.Declaration
    , internal : Internal tipe
    }


{-| -}
type alias Value =
    { value : Elm.Expression
    , declaration : Elm.Declaration
    , internal : Internal Elm.Expression
    }


{-| -}
type Internal val
    = Internal (List String -> val)


{-| -}
module_ : List String -> val -> Module val
module_ name call =
    { name = name
    , call = call
    , declarations = []
    }


{-| -}
alias : String -> Elm.Annotation.Annotation -> Annotation
alias name annotation =
    { annotation = Elm.Annotation.named [] name
    , declaration = Elm.alias name annotation
    , internal = Internal (\mod -> Elm.Annotation.named mod name)
    }


{-| -}
customType : String -> List Elm.Variant -> Annotation
customType name variants =
    { annotation = Elm.Annotation.named [] name
    , declaration = Elm.customType name variants
    , internal = Internal (\mod -> Elm.Annotation.named mod name)
    }


{-| -}
with : { a | declaration : Elm.Declaration, internal : Internal required } -> Module (required -> val) -> Module val
with decl mod =
    let
        (Internal call) =
            decl.internal
    in
    { name = mod.name
    , declarations = decl.declaration :: mod.declarations
    , call = mod.call (call mod.name)
    }


{-| -}
withUnexposed : { a | declaration : Elm.Declaration } -> Module val -> Module val
withUnexposed { declaration } mod =
    { mod | declarations = declaration :: mod.declarations }


{-| -}
fn :
    String
    -> Elm.Arg.Arg value
    -> (value -> Expression)
    -> Function (Expression -> Expression)
fn name one toExp =
    let
        funcExp : Expression
        funcExp =
            Elm.fnBuilder toExp
                |> Elm.arg one
                |> Elm.fnDone

        call : Expression -> Expression -> Expression
        call expr argOne =
            Elm.apply
                expr
                [ argOne
                ]
    in
    innerFunction name funcExp call


{-| -}
fn2 :
    String
    -> Elm.Arg.Arg one
    -> Elm.Arg.Arg two
    -> (one -> two -> Expression)
    -> Function (Expression -> Expression -> Expression)
fn2 name one two toExp =
    let
        funcExp : Expression
        funcExp =
            Elm.fnBuilder toExp
                |> Elm.arg one
                |> Elm.arg two
                |> Elm.fnDone

        call : Expression -> Expression -> Expression -> Expression
        call expr argOne argTwo =
            Elm.apply
                expr
                [ argOne
                , argTwo
                ]
    in
    innerFunction name funcExp call


{-| -}
fn3 :
    String
    -> Elm.Arg.Arg one
    -> Elm.Arg.Arg two
    -> Elm.Arg.Arg three
    -> (one -> two -> three -> Expression)
    -> Function (Expression -> Expression -> Expression -> Expression)
fn3 name one two three toExp =
    let
        funcExp : Expression
        funcExp =
            Elm.fnBuilder toExp
                |> Elm.arg one
                |> Elm.arg two
                |> Elm.arg three
                |> Elm.fnDone

        call : Expression -> Expression -> Expression -> Expression -> Expression
        call expr argOne argTwo argThree =
            Elm.apply
                expr
                [ argOne
                , argTwo
                , argThree
                ]
    in
    innerFunction name funcExp call


{-| -}
fn4 :
    String
    -> Elm.Arg.Arg one
    -> Elm.Arg.Arg two
    -> Elm.Arg.Arg three
    -> Elm.Arg.Arg four
    -> (one -> two -> three -> four -> Expression)
    -> Function (Expression -> Expression -> Expression -> Expression -> Expression)
fn4 name one two three four toExp =
    let
        funcExp : Expression
        funcExp =
            Elm.fnBuilder toExp
                |> Elm.arg one
                |> Elm.arg two
                |> Elm.arg three
                |> Elm.arg four
                |> Elm.fnDone

        call : Expression -> Expression -> Expression -> Expression -> Expression -> Expression
        call expr argOne argTwo argThree argFour =
            Elm.apply
                expr
                [ argOne
                , argTwo
                , argThree
                , argFour
                ]
    in
    innerFunction name funcExp call


{-| -}
fn5 :
    String
    -> Elm.Arg.Arg one
    -> Elm.Arg.Arg two
    -> Elm.Arg.Arg three
    -> Elm.Arg.Arg four
    -> Elm.Arg.Arg five
    -> (one -> two -> three -> four -> five -> Expression)
    -> Function (Expression -> Expression -> Expression -> Expression -> Expression -> Expression)
fn5 name one two three four five toExp =
    let
        funcExp : Expression
        funcExp =
            Elm.fnBuilder toExp
                |> Elm.arg one
                |> Elm.arg two
                |> Elm.arg three
                |> Elm.arg four
                |> Elm.arg five
                |> Elm.fnDone

        call : Expression -> Expression -> Expression -> Expression -> Expression -> Expression -> Expression
        call expr argOne argTwo argThree argFour argFive =
            Elm.apply
                expr
                [ argOne
                , argTwo
                , argThree
                , argFour
                , argFive
                ]
    in
    innerFunction name funcExp call


{-| -}
fn6 :
    String
    -> Elm.Arg.Arg one
    -> Elm.Arg.Arg two
    -> Elm.Arg.Arg three
    -> Elm.Arg.Arg four
    -> Elm.Arg.Arg five
    -> Elm.Arg.Arg six
    -> (one -> two -> three -> four -> five -> six -> Expression)
    -> Function (Expression -> Expression -> Expression -> Expression -> Expression -> Expression -> Expression)
fn6 name one two three four five six toExp =
    let
        funcExp : Expression
        funcExp =
            Elm.fnBuilder toExp
                |> Elm.arg one
                |> Elm.arg two
                |> Elm.arg three
                |> Elm.arg four
                |> Elm.arg five
                |> Elm.arg six
                |> Elm.fnDone

        call : Expression -> Expression -> Expression -> Expression -> Expression -> Expression -> Expression -> Expression
        call expr argOne argTwo argThree argFour argFive argSix =
            Elm.apply
                expr
                [ argOne
                , argTwo
                , argThree
                , argFour
                , argFive
                , argSix
                ]
    in
    innerFunction name funcExp call


{-| -}
function :
    String
    -> List ( String, Maybe Elm.Annotation.Annotation )
    -> (List Expression -> Expression)
    -> Function (List Expression -> Expression)
function name params toExp =
    innerFunction name (Elm.function params toExp) Elm.apply


innerFunction :
    String
    -> Expression
    -> (Expression -> tipe)
    -> Function tipe
innerFunction name funcExp call =
    { value = Elm.val name
    , call = call (Elm.val name)
    , declaration = Elm.declaration name funcExp
    , internal =
        Internal
            (\modName ->
                call
                    (Elm.value
                        { importFrom = modName
                        , name = Format.sanitize name
                        , annotation = Nothing
                        }
                    )
            )
    }


{-| -}
value :
    String
    -> Elm.Expression
    -> Value
value name expression =
    { value = Elm.val name
    , declaration = Elm.declaration name expression
    , internal =
        Internal
            (\modName ->
                Elm.value
                    { importFrom = modName
                    , name = name
                    , annotation = Nothing
                    }
            )
    }


{-| You may want a placeholder function body if you're defining a function using `Declare` with the intention of _calling_ the function instead of defining it.

In that case you can use `placeholder`!

Of note, if you generate the actual body of `placeholder`, it'll generate `Debug.todo "Placeholder function body"`.

-}
placeholder : Elm.Expression
placeholder =
    Elm.apply
        (Elm.value
            { importFrom = [ "Debug" ]
            , name = "todo"
            , annotation = Just (Elm.Annotation.var "a")
            }
        )
        [ Elm.string "Placeholder function body" ]


{-| -}
toFile : Module val -> Elm.File
toFile mod =
    Elm.file mod.name
        (List.reverse mod.declarations)
