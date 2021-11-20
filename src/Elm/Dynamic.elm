module Elm.Dynamic exposing
    ( Expression
    , value, valueFrom, valueWith
    , withType
    , record, field, Field, get, updateRecord
    , customType, Variant, variant, variantWith
    , alias
    , Dynamic, fromDynamic, toDynamic
    , lambda
    )

{-|

@docs Expression


## Basics

@docs value, valueFrom, valueWith

@docs withType


## Records

@docs record, field, Field, get, updateRecord


## Custom Types

@docs customType, Variant, variant, variantWith

@docs alias


## Interop with statically typed `Expression`s

@docs Dynamic, fromDynamic, toDynamic

-}

import Elm exposing (Declaration)
import Elm.Annotation
import Elm.Let as Let
import Elm.Parser
import Elm.Pattern exposing (Pattern)
import Elm.Processing
import Elm.Syntax.Declaration as Declaration
import Elm.Syntax.Exposing as Expose
import Elm.Syntax.Expression as Exp
import Elm.Syntax.Infix as Infix
import Elm.Syntax.Module
import Elm.Syntax.Node as Node
import Elm.Syntax.Pattern as Pattern
import Elm.Syntax.Range as Range
import Elm.Syntax.TypeAnnotation as Annotation
import Internal.Comments
import Internal.Compiler as Compiler exposing (Expression(..))
import Internal.Write
import Set


type Dynamic
    = Dynamic


type alias Expression =
    Compiler.Expression Dynamic


{-| -}
type Field
    = Field String Expression


{-| -}
record : List Field -> Expression
record fields =
    let
        unified =
            fields
                |> List.foldl
                    (\(Field unformattedFieldName (Compiler.Expression exp)) found ->
                        let
                            fieldName =
                                Compiler.formatValue unformattedFieldName
                        in
                        { fields =
                            ( Compiler.nodify fieldName
                            , Compiler.nodify exp.expression
                            )
                                :: found.fields
                        , errors =
                            if Set.member fieldName found.passed then
                                Compiler.DuplicateFieldInRecord fieldName :: found.errors

                            else
                                case exp.annotation of
                                    Err [] ->
                                        Compiler.SomeOtherIssue :: found.errors

                                    Err errs ->
                                        errs ++ found.errors

                                    Ok ann ->
                                        found.errors
                        , fieldAnnotations =
                            case exp.annotation of
                                Err err ->
                                    found.fieldAnnotations

                                Ok ann ->
                                    ( Compiler.formatValue fieldName
                                    , ann
                                    )
                                        :: found.fieldAnnotations
                        , passed = Set.insert fieldName found.passed
                        , imports = exp.imports ++ found.imports
                        }
                    )
                    { fields = []
                    , errors = []
                    , fieldAnnotations = []
                    , passed = Set.empty
                    , imports = []
                    }
    in
    Compiler.Expression
        { expression =
            unified.fields
                |> List.reverse
                |> Compiler.nodifyAll
                |> Exp.RecordExpr
        , annotation =
            case unified.errors of
                [] ->
                    List.reverse unified.fieldAnnotations
                        |> List.map
                            (\( name, ann ) ->
                                ( Compiler.nodify name
                                , Compiler.nodify ann
                                )
                            )
                        |> Compiler.nodifyAll
                        |> Annotation.Record
                        |> Ok

                errs ->
                    Err errs
        , imports =
            unified.imports
        , skip = False
        }


{-| -}
updateRecord : String -> List Field -> Expression
updateRecord name fields =
    Compiler.Expression
        { expression =
            fields
                |> List.map
                    (\(Field fieldName fieldExp) ->
                        Compiler.nodify
                            ( Compiler.nodify fieldName
                            , Compiler.nodify (Compiler.getInnerExpression fieldExp)
                            )
                    )
                |> Exp.RecordUpdateExpression (Compiler.nodify name)
        , annotation =
            Err []
        , imports =
            List.concatMap
                (\(Field _ exp) -> Compiler.getImports exp)
                fields
        , skip = False
        }


{-| -}
field : String -> Expression -> Field
field name payload =
    Field name (Compiler.unsafe payload)


{-| -}
value : String -> Expression
value =
    valueFrom []


{-| -}
valueFrom : List String -> String -> Expression
valueFrom mod name =
    Compiler.Expression
        { expression =
            Exp.FunctionOrValue mod
                (Compiler.sanitize name)
        , annotation = Err []
        , imports = [ mod ]
        , skip = False
        }


{-| Add an annotation to a value.

**Note** this may not _literally_ add an annotation to the code, but will inform `elm-prefab`s type inference so that top level values can be auto-annotated.

So, for example, if we have.

    Elm.list
        [ Elm.valueWith myModule "myString" Elm.Annotation.string
        , Elm.valueWith myModule "myOtherString" Elm.Annotation.string
        ]

Then, when that list is generated, it will automatically have the type signature `List String`

-}
valueWith : List String -> String -> Elm.Annotation.Annotation t -> Expression
valueWith mod name ann =
    Compiler.Expression
        { expression = Exp.FunctionOrValue mod (Compiler.sanitize name)
        , annotation = Ok (Compiler.getInnerAnnotation ann)
        , imports = mod :: Compiler.getAnnotationImports ann
        , skip = False
        }


{-| Sometimes you may need to add a manual type annotation.

    import Elm.Annotation as Type

    Elm.value "myString"
        |> Elm.withType (Type.string)

Though be sure elm-prefab isn't already doing this automatically for you!

-}
withType : Elm.Annotation.Annotation a -> Expression -> Expression
withType ann (Compiler.Expression exp) =
    Compiler.Expression
        { exp
            | annotation = Ok (Compiler.getInnerAnnotation ann)
            , imports = exp.imports ++ Compiler.getAnnotationImports ann
        }


{-|

    record
        |> Elm.get "field"

results in

    record.field

-}
get : String -> Expression -> Expression
get selector (Compiler.Expression expr) =
    Compiler.Expression
        { expression =
            Exp.RecordAccess (Compiler.nodify expr.expression) (Compiler.nodify (Compiler.formatValue selector))
        , annotation =
            case expr.annotation of
                Ok (Annotation.Record fields) ->
                    case getField (Compiler.formatValue selector) fields of
                        Just ann ->
                            Ok ann

                        Nothing ->
                            Err [ Compiler.CouldNotFindField selector ]

                Ok (Annotation.GenericRecord name fields) ->
                    case getField (Compiler.formatValue selector) (Compiler.denode fields) of
                        Just ann ->
                            Ok ann

                        Nothing ->
                            Err [ Compiler.CouldNotFindField selector ]

                otherwise ->
                    otherwise
        , imports = expr.imports
        , skip = False
        }


getField :
    String
    -> List (Node.Node ( Node.Node String, Node.Node b ))
    -> Maybe b
getField selector fields =
    case fields of
        [] ->
            Nothing

        nodifiedTop :: remain ->
            case Compiler.denode nodifiedTop of
                ( fieldname, contents ) ->
                    if Compiler.denode fieldname == selector then
                        Just (Compiler.denode contents)

                    else
                        getField selector remain


{-| -}
fromDynamic : Expression -> Compiler.Expression a
fromDynamic (Expression e) =
    Expression e


{-| -}
toDynamic : Compiler.Expression a -> Expression
toDynamic (Expression e) =
    Expression e


{-| A custom type declaration.

    Elm.customType "MyType"
        [ Elm.variant "One"
        , Elm.variantWith "Two" [ Elm.Annotation.list Elm.Annotation.string ]
        ]

Will result in

    type MyType
        = One
        | Two (List String)

-}
customType : String -> List Variant -> Declaration
customType name variants =
    Compiler.Declaration Compiler.NotExposed
        (List.concatMap
            (\(Variant _ listAnn) ->
                List.concatMap Compiler.getAnnotationImports listAnn
            )
            variants
        )
        (Declaration.CustomTypeDeclaration
            { documentation = Nothing
            , name = Compiler.nodify (Compiler.formatType name)
            , generics =
                List.concatMap
                    (\(Variant _ listAnn) ->
                        listAnn
                            |> List.concatMap
                                Compiler.getGenerics
                    )
                    variants
            , constructors =
                List.map
                    (\(Variant varName vars) ->
                        Compiler.nodify
                            { name = Compiler.nodify (Compiler.formatType varName)
                            , arguments =
                                List.map
                                    (Compiler.getInnerAnnotation
                                        >> Compiler.nodify
                                    )
                                    vars
                            }
                    )
                    variants
            }
        )


{-| -}
type Variant
    = Variant String (List Compiler.AnnotationDetails)


{-| -}
variant : String -> Variant
variant name =
    Variant name []


{-| -}
variantWith : String -> List Compiler.AnnotationDetails -> Variant
variantWith =
    Variant


{-| A custom type declaration.

    import Elm.Annotation as Type

    Elm.alias "MyAlias"
        (Type.record
            [ ( "one", Type.string )
            , ( "two", Type.int )
            , ( "three", Type.var "content" )
            ]
        )

Should result in

    type alias MyAlias content =
        { one : String
        , two : Int
        , three : content
        }

-}
alias : String -> Elm.Annotation.Annotation -> Declaration
alias name innerAnnotation =
    Compiler.Declaration Compiler.NotExposed
        (Compiler.getAnnotationImports innerAnnotation)
        (Declaration.AliasDeclaration
            { documentation = Nothing
            , name = Compiler.nodify (Compiler.formatType name)
            , generics =
                Compiler.getGenerics innerAnnotation
            , typeAnnotation = Compiler.nodify (Compiler.getInnerAnnotation innerAnnotation)
            }
        )


{-| -}
lambda : String -> Elm.Annotation.Annotation -> (Expression -> Expression) -> Expression
lambda argBaseName argType toExpression =
    let
        arg1 =
            valueWith [] argBaseName argType

        (Compiler.Expression expr) =
            toExpression arg1
    in
    Compiler.Expression
        { expression =
            Exp.LambdaExpression
                { args = [ Compiler.nodify (Pattern.VarPattern argBaseName) ]
                , expression = Compiler.nodify expr.expression
                }
        , annotation =
            case expr.annotation of
                Err err ->
                    Err err

                Ok return ->
                    List.foldr
                        (\ann fnbody ->
                            Annotation.FunctionTypeAnnotation
                                (Compiler.nodify ann)
                                (Compiler.nodify fnbody)
                        )
                        return
                        [ Compiler.getInnerAnnotation argType ]
                        |> Ok
        , imports = expr.imports
        , skip = False
        }


{-| -}
lambdaWith : List ( Pattern t e, Elm.Annotation.Annotation ) -> Expression -> Expression
lambdaWith args (Compiler.Expression expr) =
    Compiler.Expression
        { expression =
            Exp.LambdaExpression
                { args = Compiler.nodifyAll (List.map Tuple.first args)
                , expression = Compiler.nodify expr.expression
                }
        , annotation =
            case expr.annotation of
                Err err ->
                    Err err

                Ok return ->
                    List.foldr
                        (\ann fnbody ->
                            Annotation.FunctionTypeAnnotation
                                (Compiler.nodify ann)
                                (Compiler.nodify fnbody)
                        )
                        return
                        (List.map (Compiler.getInnerAnnotation << Tuple.second) args)
                        |> Ok
        , imports = expr.imports
        , skip = False
        }
