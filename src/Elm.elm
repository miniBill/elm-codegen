module Elm exposing
    ( File, file
    , Expression
    , bool, int, float, char, string, hex, unit
    , maybe, list, tuple, triple
    , caseOf, letIn, ifThen
    , apply
    , lambda1, lambda2, lambda3, lambda4, lambda5, lambdaBetaReduced
    , Declaration
    , comment, declaration
    , withDocumentation
    , fn, fn2, fn3, fn4, fn5
    , expose, exposeConstructor
    , exposeAndGroup, exposeConstructorAndGroup
    , fileWith
    , equal, notEqual
    , append, cons
    , plus, minus, multiply, divide, intDivide, power
    , lt, gt, lte, gte, and, or
    , pipe, pipeLeft, compose, composeLeft
    , portIncoming, portOutgoing
    , parse
    , toString, expressionImports
    , declarationToString, declarationImports
    , pass
    -- , keep, skip
    -- , slash, question
    )

{-|

@docs File, file


## Basics

@docs Expression

@docs bool, int, float, char, string, hex, unit

@docs maybe, list, tuple, triple


## Flow control

@docs caseOf, letIn, ifThen

@docs apply

@docs lambda1, lambda2, lambda3, lambda4, lambda5, lambdaBetaReduced


## Top level

@docs Declaration

@docs comment, declaration

@docs withDocumentation

@docs fn, fn2, fn3, fn4, fn5, fn6, functionWith


## Exposing values

By default, everything is exposed for your module.

However, you can tag specific declarations you want exposed, and then only those things will be exposed.

@docs expose, exposeConstructor


## Grouping exposed values in the module comment

You can also add a group tag to an exposed value. This will automatically group the `docs` statements in the module docs.

For precise control over what is rendered for the module comment, use [fileWith](#fileWith)

@docs exposeAndGroup, exposeConstructorAndGroup

@docs fileWith


# Operators

@docs equal, notEqual

@docs append, cons

@docs plus, minus, multiply, divide, intDivide, power

@docs lt, gt, lte, gte, and, or

@docs pipe, pipeLeft, compose, composeLeft


## Parsing

@docs keep, skip


## Url parsing

@docs slash, question


# Ports

@docs portIncoming, portOutgoing


# Parsing existing Elm

@docs parse


# Rendering to string

@docs toString, expressionImports

@docs declarationToString, declarationImports

@docs pass

-}

import Elm.Annotation
import Elm.Let as Let
import Elm.Parser
import Elm.Processing
import Elm.Syntax.Declaration as Declaration exposing (Declaration(..))
import Elm.Syntax.Exposing as Expose
import Elm.Syntax.Expression as Exp
import Elm.Syntax.Infix as Infix
import Elm.Syntax.Module
import Elm.Syntax.Node as Node
import Elm.Syntax.Pattern as Pattern
import Elm.Syntax.Range as Range
import Elm.Syntax.TypeAnnotation as Annotation
import Internal.Comments
import Internal.Compiler as Compiler
import Internal.Write
import Set


{-| -}
type alias Expression generated =
    Compiler.Expression generated


{-| -}
expressionImports : Expression generated -> String
expressionImports (Compiler.Expression exp) =
    List.filterMap (Compiler.makeImport []) exp.imports
        |> Internal.Write.writeImports


{-| -}
toString : Expression generated -> String
toString (Compiler.Expression exp) =
    Internal.Write.writeExpression exp.expression


{-| -}
declarationImports : Declaration -> String
declarationImports decl =
    case decl of
        Compiler.Declaration _ imps _ ->
            List.filterMap (Compiler.makeImport []) imps
                |> Internal.Write.writeImports

        Compiler.Comment _ ->
            ""


{-| -}
declarationToString : Declaration -> String
declarationToString dec =
    Internal.Write.writeDeclaration dec


{-| Build a file!

    Elm.file [ "My", "Module" ]
        [ Elm.declaration "placeholder"
            (Elm.string "a fancy string!")
        ]

-}
file : List String -> List Declaration -> File
file mod decs =
    render renderStandardComment
        { moduleDefinition = mod
        , imports =
            reduceDeclarationImports mod decs ( Set.empty, [] )
                |> Tuple.second
        , body = decs
        , aliases = []
        , moduleComment = ""
        }


renderStandardComment :
    List
        { group : Maybe String
        , members : List String
        }
    -> String
renderStandardComment groups =
    if List.isEmpty groups then
        ""

    else
        List.foldl
            (\grouped str ->
                str ++ "@docs " ++ String.join ", " grouped.members ++ "\n\n"
            )
            "\n\n"
            groups


{-| Same as [file](#file), but you have more control over how the module comment is generated!

Pass in a function that determines how to render a `@doc` comment.

Each exposed item is grouped based on the string used in [exposeAndGroup](#exposeAndGroup)

-}
fileWith :
    List String
    ->
        { docs :
            List
                { group : Maybe String
                , members : List String
                }
            -> String
        , aliases : List ( List String, String )
        }
    -> List Declaration
    -> File
fileWith mod options decs =
    render options.docs
        { moduleDefinition = mod
        , imports =
            reduceDeclarationImports mod decs ( Set.empty, [] )
                |> Tuple.second
        , aliases = options.aliases
        , body = decs
        , moduleComment = ""
        }


{-| -}
render :
    (List
        { group : Maybe String
        , members : List String
        }
     -> String
    )
    -> FileDetails
    -> File
render toDocComment fileDetails =
    let
        mod =
            fileDetails.moduleDefinition

        exposed =
            Compiler.getExposed fileDetails.body

        exposedGroups =
            Compiler.getExposedGroups fileDetails.body

        body =
            Internal.Write.write
                { moduleDefinition =
                    (if Compiler.hasPorts fileDetails.body then
                        Elm.Syntax.Module.PortModule

                     else
                        Elm.Syntax.Module.NormalModule
                    )
                        { moduleName = Compiler.nodify mod
                        , exposingList =
                            case exposed of
                                [] ->
                                    Compiler.nodify
                                        (Expose.All Range.emptyRange)

                                _ ->
                                    Compiler.nodify
                                        (Expose.Explicit
                                            (Compiler.nodifyAll exposed)
                                        )
                        }
                , aliases = fileDetails.aliases
                , imports =
                    List.filterMap (Compiler.makeImport fileDetails.aliases) fileDetails.imports
                , declarations = fileDetails.body
                , comments =
                    Just
                        (Internal.Comments.addPart
                            Internal.Comments.emptyComment
                            (Internal.Comments.Markdown
                                (toDocComment exposedGroups)
                            )
                        )
                }
    in
    { path =
        String.join "/" mod ++ ".elm"
    , contents = body
    }


type alias Module =
    List String


reduceDeclarationImports : Module -> List Declaration -> ( Set.Set String, List Module ) -> ( Set.Set String, List Module )
reduceDeclarationImports self decs imports =
    case decs of
        [] ->
            imports

        (Compiler.Comment _) :: remain ->
            reduceDeclarationImports self
                remain
                imports

        (Compiler.Declaration _ newImports body) :: remain ->
            reduceDeclarationImports self
                remain
                (addImports self newImports imports)


addImports : Module -> List Module -> ( Set.Set String, List Module ) -> ( Set.Set String, List Module )
addImports self newImports ( set, deduped ) =
    case newImports of
        [] ->
            ( set, deduped )

        new :: remain ->
            let
                full =
                    Compiler.fullModName new
            in
            if Set.member full set || full == Compiler.fullModName self then
                -- skip
                addImports self remain ( set, deduped )

            else
                addImports self
                    remain
                    ( Set.insert full set, new :: deduped )


{-| -}
type alias File =
    { path : String
    , contents : String
    }


type alias FileDetails =
    { moduleDefinition : Module
    , imports : List Module
    , aliases : List ( Module, String )
    , body : List Declaration
    , moduleComment : String
    }


basicExpression tipe expr =
    Compiler.Expression
        { expression = expr
        , annotation = Ok (Compiler.getInnerAnnotation tipe)
        , imports = []
        , skip = False
        }


{-| -}
unit : Expression ()
unit =
    basicExpression Elm.Annotation.unit Exp.UnitExpr


{-| -}
bool : Bool -> Expression Bool
bool on =
    basicExpression Elm.Annotation.bool <|
        Exp.FunctionOrValue [] <|
            if on then
                "True"

            else
                "False"


{-| -}
int : Int -> Expression Int
int intVal =
    basicExpression Elm.Annotation.int <| Exp.Integer intVal


{-| -}
hex : Int -> Expression Int
hex hexVal =
    basicExpression Elm.Annotation.int <| Exp.Hex hexVal


{-| -}
float : Float -> Expression Float
float floatVal =
    basicExpression Elm.Annotation.float <| Exp.Floatable floatVal


{-| -}
string : String -> Expression String
string literal =
    basicExpression Elm.Annotation.string <| Exp.Literal literal


{-| -}
char : Char -> Expression Char
char charVal =
    basicExpression Elm.Annotation.char <| Exp.CharLiteral charVal



-- {-|
-- -}
-- glsl : String -> Expression
-- glsl expr =
--     Exp.GLSLExpression expr


{-| -}
tuple : Expression a -> Expression b -> Expression ( a, b )
tuple (Compiler.Expression one) (Compiler.Expression two) =
    Compiler.Expression
        { expression = Exp.TupledExpression (Compiler.nodifyAll [ one.expression, two.expression ])
        , annotation =
            Result.map2
                (\oneA twoA ->
                    Elm.Annotation.tuple
                        (Compiler.noImports oneA)
                        (Compiler.noImports twoA)
                        |> Compiler.getInnerAnnotation
                )
                one.annotation
                two.annotation
        , imports = one.imports ++ two.imports
        , skip = False
        }


{-| -}
triple : Expression a -> Expression b -> Expression c -> Expression ( a, b, c )
triple (Compiler.Expression one) (Compiler.Expression two) (Compiler.Expression three) =
    Compiler.Expression
        { expression =
            Exp.TupledExpression
                (Compiler.nodifyAll
                    [ one.expression, two.expression, three.expression ]
                )
        , annotation =
            Result.map3
                (\oneA twoA threeA ->
                    Elm.Annotation.triple
                        (Compiler.noImports oneA)
                        (Compiler.noImports twoA)
                        (Compiler.noImports threeA)
                        |> Compiler.getInnerAnnotation
                )
                one.annotation
                two.annotation
                three.annotation
        , imports = one.imports ++ two.imports ++ three.imports
        , skip = False
        }


{-| -}
maybe : Maybe (Expression a) -> Expression (Maybe a)
maybe content =
    Compiler.Expression
        { expression =
            case content of
                Nothing ->
                    Exp.FunctionOrValue []
                        "Nothing"

                Just inner ->
                    Exp.Application
                        [ Exp.FunctionOrValue []
                            "Just"
                            |> Compiler.nodify
                        , Exp.ParenthesizedExpression
                            (Compiler.nodify (Compiler.getInnerExpression inner))
                            |> Compiler.nodify
                        ]
        , annotation =
            case content of
                Nothing ->
                    Ok
                        (Compiler.getInnerAnnotation
                            (Elm.Annotation.maybe (Elm.Annotation.var "a"))
                        )

                Just inner ->
                    Result.map
                        (\ann ->
                            Annotation.Typed
                                (Compiler.nodify ( [], "Maybe" ))
                                [ Compiler.nodify ann ]
                        )
                        (Compiler.getAnnotation inner)
        , imports =
            Maybe.map getImports content
                |> Maybe.withDefault []
        , skip = False
        }


{-| -}
list : List (Expression a) -> Expression a
list exprs =
    Compiler.Expression
        { expression = Exp.ListExpr (List.map toList exprs)
        , annotation =
            Compiler.unify exprs
                |> Result.map
                    (\inner ->
                        Annotation.Typed
                            (Compiler.nodify ( [], "List" ))
                            [ Compiler.nodify inner ]
                    )
        , imports = List.concatMap getImports exprs
        , skip = False
        }


toList : Expression a -> Node.Node Exp.Expression
toList (Compiler.Expression exp) =
    Compiler.nodify exp.expression


{-| A let block.

Check out `Elm.Let` to add things to it.

    import Elm.Let as Let

    Elm.letIn
        [ Let.value "one" (Elm.int 5)
        , Let.value "two" (Elm.int 10)
        ]
        (Elm.add (Elm.value "one") (Elm.value "two"))

-}
letIn : List Let.Declaration -> Expression a -> Expression a
letIn decls (Compiler.Expression within) =
    let
        gathered =
            List.foldr
                (\(Compiler.LetDeclaration mods dec) accum ->
                    { declarations =
                        dec :: accum.declarations
                    , imports = accum.imports ++ mods
                    }
                )
                { declarations = []
                , imports = []
                }
                decls
    in
    Compiler.Expression
        { expression =
            Exp.LetExpression
                { declarations = Compiler.nodifyAll gathered.declarations
                , expression = Compiler.nodify within.expression
                }
        , imports = gathered.imports
        , annotation =
            within.annotation
        , skip = False
        }


{-|

    ifThen (Elm.bool True)
        (Elm.string "yes")
        (Elm.string "no")

    if True then
        "yes"

    else
        "no"

-}
ifThen : Expression Bool -> Expression a -> Expression a -> Expression a
ifThen (Compiler.Expression condition) (Compiler.Expression thenBranch) (Compiler.Expression elseBranch) =
    Compiler.Expression
        { expression =
            Exp.IfBlock
                (Compiler.nodify condition.expression)
                (Compiler.nodify thenBranch.expression)
                (Compiler.nodify elseBranch.expression)
        , annotation =
            thenBranch.annotation
        , imports = condition.imports ++ thenBranch.imports ++ elseBranch.imports
        , skip = False
        }


{-| -}
caseOf : Expression incoming -> List ( Pattern, Expression result ) -> Expression result
caseOf (Compiler.Expression expr) cases =
    let
        gathered =
            List.foldl
                (\( pattern, Compiler.Expression exp ) accum ->
                    { cases = ( Compiler.nodify pattern, Compiler.nodify exp.expression ) :: accum.cases
                    , imports = accum.imports ++ exp.imports
                    , annotation =
                        case accum.annotation of
                            Nothing ->
                                Just exp.annotation

                            Just exist ->
                                if exist == exp.annotation then
                                    accum.annotation

                                else
                                    Just (Err [ Compiler.CaseBranchesReturnDifferentTypes ])
                    }
                )
                { cases = []
                , imports = []
                , annotation = Nothing
                }
                cases
    in
    Compiler.Expression
        { expression =
            Exp.CaseExpression
                { expression = Compiler.nodify expr.expression
                , cases = List.reverse gathered.cases
                }
        , annotation =
            case gathered.annotation of
                Nothing ->
                    Err [ Compiler.EmptyCaseStatement ]

                Just ann ->
                    ann
        , imports = expr.imports ++ gathered.imports
        , skip = False
        }


{-| Not exposed, this should be done automatically!
-}
parens : Exp.Expression -> Exp.Expression
parens expr =
    Exp.ParenthesizedExpression (Compiler.nodify expr)


getExpression : Expression a -> Exp.Expression
getExpression (Compiler.Expression exp) =
    exp.expression


getImports : Expression a -> List Compiler.Module
getImports (Compiler.Expression exp) =
    exp.imports



-- {-| -}
-- apply : Expression -> List Expression -> Expression
-- apply ((Compiler.Expression exp) as top) allArgs =
--     let
--         args =
--             List.filter (\(Compiler.Expression arg) -> not arg.skip) allArgs
--     in
--     Compiler.Expression
--         { expression =
--             Exp.Application (Compiler.nodifyAll (exp.expression :: List.map (parens << getExpression) args))
--         , annotation =
--             Compiler.applyType top args
--         , imports = exp.imports ++ List.concatMap getImports args
--         , skip = False
--         }


{-| -}
apply : Expression (a -> b) -> Expression a -> Expression b
apply ((Compiler.Expression exp) as top) arg =
    let
        args =
            List.filter (\(Compiler.Expression a) -> not a.skip) [ arg ]
    in
    Compiler.Expression
        { expression =
            Exp.Application (Compiler.nodifyAll (exp.expression :: List.map (parens << getExpression) args))
        , annotation =
            Compiler.applyType top args
        , imports = exp.imports ++ List.concatMap getImports args
        , skip = False
        }


{-| -}
type Pattern tipe exprs
    = Pattern Pattern.Pattern


{-| -}
lambda1 : Pattern tipe exprs -> (exprs -> Expression result) -> Expression (tipe -> result)
lambda1 argBaseName argType toExpression =
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
lambdaBetaReduced : String -> Elm.Annotation.Annotation -> (Expression -> Expression) -> Expression
lambdaBetaReduced argBaseName argType toExpression =
    let
        arg1 =
            valueWith [] argBaseName argType

        (Compiler.Expression expr) =
            toExpression arg1
    in
    Compiler.Expression
        { expression =
            betaReduce <|
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


betaReduce : Exp.Expression -> Exp.Expression
betaReduce e =
    let
        extractLastArg arg =
            case arg of
                Exp.FunctionOrValue [] n ->
                    Just n

                Exp.ParenthesizedExpression p ->
                    extractLastArg <| Compiler.denode p

                _ ->
                    Nothing

        -- If the list is nonempty, returns a tuple with the beginning of the list and the last element (denoded).
        popLast : List (Node.Node a) -> Maybe ( List (Node.Node a), a )
        popLast lst =
            case List.reverse lst of
                [] ->
                    Nothing

                last :: initReverse ->
                    Just ( List.reverse initReverse, Compiler.denode last )
    in
    case e of
        Exp.LambdaExpression { args, expression } ->
            case popLast args of
                Just ( initLambdaArgs, Pattern.VarPattern lastLambdaArg ) ->
                    case Compiler.denode expression of
                        Exp.RecordAccess argNode fieldNode ->
                            let
                                fieldName =
                                    Compiler.denode fieldNode

                                arg =
                                    Compiler.denode argNode
                            in
                            case arg of
                                Exp.FunctionOrValue [] argName ->
                                    if argName == lastLambdaArg then
                                        Exp.RecordAccessFunction <| "." ++ fieldName

                                    else
                                        e

                                _ ->
                                    e

                        Exp.Application applicationArgs ->
                            case popLast applicationArgs of
                                Just ( [], uniqueApplicationArg ) ->
                                    if extractLastArg uniqueApplicationArg == Just lastLambdaArg then
                                        Exp.FunctionOrValue [] "identity"

                                    else
                                        e

                                Just ( initApplicationArgs, lastApplicationArg ) ->
                                    if extractLastArg lastApplicationArg == Just lastLambdaArg then
                                        if List.isEmpty initLambdaArgs then
                                            case initApplicationArgs of
                                                [ s ] ->
                                                    betaReduce <| Compiler.denode s

                                                _ ->
                                                    Exp.Application initApplicationArgs

                                        else
                                            betaReduce <|
                                                Exp.LambdaExpression
                                                    { args = initLambdaArgs
                                                    , expression =
                                                        Compiler.nodify <|
                                                            Exp.Application initApplicationArgs
                                                    }

                                    else
                                        e

                                _ ->
                                    e

                        _ ->
                            e

                _ ->
                    e

        _ ->
            e


{-| -}
lambda2 :
    String
    -> Elm.Annotation.Annotation
    -> Elm.Annotation.Annotation
    -> (Expression a -> Expression b -> Expression c)
    -> Expression c
lambda2 argBaseName oneType twoType toExpression =
    let
        arg1 =
            valueWith [] argBaseName oneType

        arg2 =
            valueWith [] (argBaseName ++ "2") twoType

        (Compiler.Expression expr) =
            toExpression arg1 arg2
    in
    Compiler.Expression
        { expression =
            Exp.LambdaExpression
                { args =
                    [ Compiler.nodify (Pattern.VarPattern argBaseName)
                    , Compiler.nodify (Pattern.VarPattern (argBaseName ++ "2"))
                    ]
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
                        [ Compiler.getInnerAnnotation oneType
                        , Compiler.getInnerAnnotation twoType
                        ]
                        |> Ok
        , imports = expr.imports
        , skip = False
        }


{-| -}
lambda3 :
    String
    -> Elm.Annotation.Annotation
    -> Elm.Annotation.Annotation
    -> Elm.Annotation.Annotation
    -> (Expression a -> Expression b -> Expression c -> Expression d)
    -> Expression d
lambda3 argBaseName oneType twoType threeType toExpression =
    let
        arg1 =
            valueWith [] argBaseName oneType

        arg2 =
            valueWith [] (argBaseName ++ "2") twoType

        arg3 =
            valueWith [] (argBaseName ++ "3") threeType

        (Compiler.Expression expr) =
            toExpression arg1 arg2 arg3
    in
    Compiler.Expression
        { expression =
            Exp.LambdaExpression
                { args =
                    [ Compiler.nodify (Pattern.VarPattern argBaseName)
                    , Compiler.nodify (Pattern.VarPattern (argBaseName ++ "2"))
                    , Compiler.nodify (Pattern.VarPattern (argBaseName ++ "3"))
                    ]
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
                        [ Compiler.getInnerAnnotation oneType
                        , Compiler.getInnerAnnotation twoType
                        , Compiler.getInnerAnnotation threeType
                        ]
                        |> Ok
        , imports = expr.imports
        , skip = False
        }


{-| -}
lambda4 :
    String
    -> Elm.Annotation.Annotation
    -> Elm.Annotation.Annotation
    -> Elm.Annotation.Annotation
    -> Elm.Annotation.Annotation
    -> (Expression a -> Expression b -> Expression c -> Expression d -> Expression e)
    -> Expression e
lambda4 argBaseName oneType twoType threeType fourType toExpression =
    let
        arg1 =
            valueWith [] argBaseName oneType

        arg2 =
            valueWith [] (argBaseName ++ "2") twoType

        arg3 =
            valueWith [] (argBaseName ++ "3") threeType

        arg4 =
            valueWith [] (argBaseName ++ "4") fourType

        (Compiler.Expression expr) =
            toExpression arg1 arg2 arg3 arg4
    in
    Compiler.Expression
        { expression =
            Exp.LambdaExpression
                { args =
                    [ Compiler.nodify (Pattern.VarPattern argBaseName)
                    , Compiler.nodify (Pattern.VarPattern (argBaseName ++ "2"))
                    , Compiler.nodify (Pattern.VarPattern (argBaseName ++ "3"))
                    , Compiler.nodify (Pattern.VarPattern (argBaseName ++ "4"))
                    ]
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
                        [ Compiler.getInnerAnnotation oneType
                        , Compiler.getInnerAnnotation twoType
                        , Compiler.getInnerAnnotation threeType
                        , Compiler.getInnerAnnotation fourType
                        ]
                        |> Ok
        , imports = expr.imports
        , skip = False
        }


{-| -}
lambda5 :
    String
    -> Elm.Annotation.Annotation
    -> Elm.Annotation.Annotation
    -> Elm.Annotation.Annotation
    -> Elm.Annotation.Annotation
    -> Elm.Annotation.Annotation
    -> (Expression a -> Expression b -> Expression c -> Expression d -> Expression e -> Expression f)
    -> Expression f
lambda5 argBaseName oneType twoType threeType fourType fiveType toExpression =
    let
        arg1 =
            valueWith [] argBaseName oneType

        arg2 =
            valueWith [] (argBaseName ++ "2") twoType

        arg3 =
            valueWith [] (argBaseName ++ "3") threeType

        arg4 =
            valueWith [] (argBaseName ++ "4") fourType

        arg5 =
            valueWith [] (argBaseName ++ "5") fiveType

        (Compiler.Expression expr) =
            toExpression arg1 arg2 arg3 arg4 arg5
    in
    Compiler.Expression
        { expression =
            Exp.LambdaExpression
                { args =
                    [ Compiler.nodify (Pattern.VarPattern argBaseName)
                    , Compiler.nodify (Pattern.VarPattern (argBaseName ++ "2"))
                    , Compiler.nodify (Pattern.VarPattern (argBaseName ++ "3"))
                    , Compiler.nodify (Pattern.VarPattern (argBaseName ++ "4"))
                    , Compiler.nodify (Pattern.VarPattern (argBaseName ++ "5"))
                    ]
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
                        [ Compiler.getInnerAnnotation oneType
                        , Compiler.getInnerAnnotation twoType
                        , Compiler.getInnerAnnotation threeType
                        , Compiler.getInnerAnnotation fourType
                        , Compiler.getInnerAnnotation fiveType
                        ]
                        |> Ok
        , imports = expr.imports
        , skip = False
        }


{-| -}
type alias Declaration =
    Compiler.Declaration


{-| -}
comment : String -> Declaration
comment content =
    Compiler.Comment ("{- " ++ content ++ " -}")


{-| -}
declaration : String -> Expression a -> Declaration
declaration name (Compiler.Expression body) =
    --function name [] body
    { documentation = Compiler.nodifyMaybe Nothing
    , signature =
        case body.annotation of
            Ok sig ->
                Just
                    (Compiler.nodify
                        { name = Compiler.nodify (Compiler.formatValue name)
                        , typeAnnotation =
                            Compiler.nodify sig
                        }
                    )

            Err _ ->
                Nothing
    , declaration =
        Compiler.nodify
            { name = Compiler.nodify (Compiler.formatValue name)
            , arguments = []
            , expression = Compiler.nodify body.expression
            }
    }
        |> Declaration.FunctionDeclaration
        |> Compiler.Declaration Compiler.NotExposed body.imports



-- {-| -}
-- functionWith : String -> List ( Elm.Annotation.Annotation, Pattern ) -> Expression -> Declaration
-- functionWith name args (Compiler.Expression body) =
--     { documentation = Compiler.nodifyMaybe Nothing
--     , signature =
--         case body.annotation of
--             Ok return ->
--                 Just
--                     (Compiler.nodify
--                         { name = Compiler.nodify (Compiler.formatValue name)
--                         , typeAnnotation =
--                             Compiler.nodify <|
--                                 Compiler.getInnerAnnotation <|
--                                     Elm.Annotation.function
--                                         (List.map Tuple.first args)
--                                         (Compiler.noImports return)
--                         }
--                     )
--             Err _ ->
--                 Nothing
--     , declaration =
--         Compiler.nodify
--             { name = Compiler.nodify (Compiler.formatValue name)
--             , arguments = Compiler.nodifyAll (List.map Tuple.second args)
--             , expression = Compiler.nodify body.expression
--             }
--     }
--         |> Declaration.FunctionDeclaration
--         |> Compiler.Declaration Compiler.NotExposed
--             (List.concatMap
--                 (Tuple.first
--                     >> Compiler.getAnnotationImports
--                 )
--                 args
--                 ++ body.imports
--             )


{-| -}
fn : String -> ( String, Elm.Annotation.Annotation ) -> (Expression a -> Expression b) -> Declaration
fn name ( oneName, oneType ) toBody =
    let
        arg1 =
            valueWith [] oneName oneType

        (Compiler.Expression body) =
            toBody arg1
    in
    { documentation = Compiler.nodifyMaybe Nothing
    , signature =
        case body.annotation of
            Ok return ->
                Just
                    (Compiler.nodify
                        { name = Compiler.nodify (Compiler.formatValue name)
                        , typeAnnotation =
                            Compiler.nodify <|
                                Compiler.getInnerAnnotation <|
                                    Elm.Annotation.function
                                        [ oneType ]
                                        (Compiler.noImports return)
                        }
                    )

            Err _ ->
                Nothing
    , declaration =
        Compiler.nodify
            { name = Compiler.nodify (Compiler.formatValue name)
            , arguments =
                [ Compiler.nodify (Pattern.VarPattern oneName)
                ]
            , expression = Compiler.nodify body.expression
            }
    }
        |> Declaration.FunctionDeclaration
        |> Compiler.Declaration Compiler.NotExposed
            (Compiler.getAnnotationImports oneType
                ++ body.imports
            )


{-| -}
fn2 :
    String
    -> ( String, Elm.Annotation.Annotation )
    -> ( String, Elm.Annotation.Annotation )
    -> (Expression a -> Expression b -> Expression c)
    -> Declaration
fn2 name ( oneName, oneType ) ( twoName, twoType ) toBody =
    let
        arg1 =
            valueWith [] oneName oneType

        arg2 =
            valueWith [] twoName twoType

        (Compiler.Expression body) =
            toBody arg1 arg2
    in
    { documentation = Compiler.nodifyMaybe Nothing
    , signature =
        case body.annotation of
            Ok return ->
                Just
                    (Compiler.nodify
                        { name = Compiler.nodify (Compiler.formatValue name)
                        , typeAnnotation =
                            Compiler.nodify <|
                                Compiler.getInnerAnnotation <|
                                    Elm.Annotation.function
                                        [ oneType, twoType ]
                                        (Compiler.noImports return)
                        }
                    )

            Err _ ->
                Nothing
    , declaration =
        Compiler.nodify
            { name = Compiler.nodify (Compiler.formatValue name)
            , arguments =
                [ Compiler.nodify (Pattern.VarPattern oneName)
                , Compiler.nodify (Pattern.VarPattern twoName)
                ]
            , expression = Compiler.nodify body.expression
            }
    }
        |> Declaration.FunctionDeclaration
        |> Compiler.Declaration Compiler.NotExposed
            (Compiler.getAnnotationImports oneType
                ++ Compiler.getAnnotationImports twoType
                ++ body.imports
            )


{-| -}
fn3 :
    String
    -> ( String, Elm.Annotation.Annotation )
    -> ( String, Elm.Annotation.Annotation )
    -> ( String, Elm.Annotation.Annotation )
    -> (Expression a -> Expression b -> Expression c -> Expression d)
    -> Declaration
fn3 name ( oneName, oneType ) ( twoName, twoType ) ( threeName, threeType ) toBody =
    let
        arg1 =
            valueWith [] oneName oneType

        arg2 =
            valueWith [] twoName twoType

        arg3 =
            valueWith [] threeName threeType

        (Compiler.Expression body) =
            toBody arg1 arg2 arg3
    in
    { documentation = Compiler.nodifyMaybe Nothing
    , signature =
        case body.annotation of
            Ok return ->
                Just
                    (Compiler.nodify
                        { name = Compiler.nodify (Compiler.formatValue name)
                        , typeAnnotation =
                            Compiler.nodify <|
                                Compiler.getInnerAnnotation <|
                                    Elm.Annotation.function
                                        [ oneType, twoType, threeType ]
                                        (Compiler.noImports return)
                        }
                    )

            Err _ ->
                Nothing
    , declaration =
        Compiler.nodify
            { name = Compiler.nodify (Compiler.formatValue name)
            , arguments =
                [ Compiler.nodify (Pattern.VarPattern oneName)
                , Compiler.nodify (Pattern.VarPattern twoName)
                , Compiler.nodify (Pattern.VarPattern threeName)
                ]
            , expression = Compiler.nodify body.expression
            }
    }
        |> Declaration.FunctionDeclaration
        |> Compiler.Declaration Compiler.NotExposed
            (Compiler.getAnnotationImports oneType
                ++ Compiler.getAnnotationImports twoType
                ++ Compiler.getAnnotationImports threeType
                ++ body.imports
            )


{-| -}
fn4 :
    String
    -> ( String, Elm.Annotation.Annotation )
    -> ( String, Elm.Annotation.Annotation )
    -> ( String, Elm.Annotation.Annotation )
    -> ( String, Elm.Annotation.Annotation )
    -> (Expression a -> Expression b -> Expression c -> Expression d -> Expression e)
    -> Declaration
fn4 name ( oneName, oneType ) ( twoName, twoType ) ( threeName, threeType ) ( fourName, fourType ) toBody =
    let
        arg1 =
            valueWith [] oneName oneType

        arg2 =
            valueWith [] twoName twoType

        arg3 =
            valueWith [] threeName threeType

        arg4 =
            valueWith [] fourName fourType

        (Compiler.Expression body) =
            toBody arg1 arg2 arg3 arg4
    in
    { documentation = Compiler.nodifyMaybe Nothing
    , signature =
        case body.annotation of
            Ok return ->
                Just
                    (Compiler.nodify
                        { name = Compiler.nodify (Compiler.formatValue name)
                        , typeAnnotation =
                            Compiler.nodify <|
                                Compiler.getInnerAnnotation <|
                                    Elm.Annotation.function
                                        [ oneType, twoType, threeType, fourType ]
                                        (Compiler.noImports return)
                        }
                    )

            Err _ ->
                Nothing
    , declaration =
        Compiler.nodify
            { name = Compiler.nodify (Compiler.formatValue name)
            , arguments =
                [ Compiler.nodify (Pattern.VarPattern oneName)
                , Compiler.nodify (Pattern.VarPattern twoName)
                , Compiler.nodify (Pattern.VarPattern threeName)
                , Compiler.nodify (Pattern.VarPattern fourName)
                ]
            , expression = Compiler.nodify body.expression
            }
    }
        |> Declaration.FunctionDeclaration
        |> Compiler.Declaration Compiler.NotExposed
            (Compiler.getAnnotationImports oneType
                ++ Compiler.getAnnotationImports twoType
                ++ Compiler.getAnnotationImports threeType
                ++ Compiler.getAnnotationImports fourType
                ++ body.imports
            )


{-| -}
fn5 :
    String
    -> ( String, Elm.Annotation.Annotation )
    -> ( String, Elm.Annotation.Annotation )
    -> ( String, Elm.Annotation.Annotation )
    -> ( String, Elm.Annotation.Annotation )
    -> ( String, Elm.Annotation.Annotation )
    -> (Expression a -> Expression b -> Expression c -> Expression d -> Expression e -> Expression f)
    -> Declaration
fn5 name ( oneName, oneType ) ( twoName, twoType ) ( threeName, threeType ) ( fourName, fourType ) ( fiveName, fiveType ) toBody =
    let
        arg1 =
            valueWith [] oneName oneType

        arg2 =
            valueWith [] twoName twoType

        arg3 =
            valueWith [] threeName threeType

        arg4 =
            valueWith [] fourName fourType

        arg5 =
            valueWith [] fiveName fiveType

        (Compiler.Expression body) =
            toBody arg1 arg2 arg3 arg4 arg5
    in
    { documentation = Compiler.nodifyMaybe Nothing
    , signature =
        case body.annotation of
            Ok return ->
                Just
                    (Compiler.nodify
                        { name = Compiler.nodify (Compiler.formatValue name)
                        , typeAnnotation =
                            Compiler.nodify <|
                                Compiler.getInnerAnnotation <|
                                    Elm.Annotation.function
                                        [ oneType, twoType, threeType, fourType, fiveType ]
                                        (Compiler.noImports return)
                        }
                    )

            Err _ ->
                Nothing
    , declaration =
        Compiler.nodify
            { name = Compiler.nodify (Compiler.formatValue name)
            , arguments =
                [ Compiler.nodify (Pattern.VarPattern oneName)
                , Compiler.nodify (Pattern.VarPattern twoName)
                , Compiler.nodify (Pattern.VarPattern threeName)
                , Compiler.nodify (Pattern.VarPattern fourName)
                , Compiler.nodify (Pattern.VarPattern fiveName)
                ]
            , expression = Compiler.nodify body.expression
            }
    }
        |> Declaration.FunctionDeclaration
        |> Compiler.Declaration Compiler.NotExposed
            (Compiler.getAnnotationImports oneType
                ++ Compiler.getAnnotationImports twoType
                ++ Compiler.getAnnotationImports threeType
                ++ Compiler.getAnnotationImports fourType
                ++ Compiler.getAnnotationImports fiveType
                ++ body.imports
            )


{-| -}
fn6 :
    String
    -> ( String, Elm.Annotation.Annotation )
    -> ( String, Elm.Annotation.Annotation )
    -> ( String, Elm.Annotation.Annotation )
    -> ( String, Elm.Annotation.Annotation )
    -> ( String, Elm.Annotation.Annotation )
    -> ( String, Elm.Annotation.Annotation )
    -> (Expression -> Expression -> Expression -> Expression -> Expression -> Expression -> Expression)
    -> Declaration
fn6 name ( oneName, oneType ) ( twoName, twoType ) ( threeName, threeType ) ( fourName, fourType ) ( fiveName, fiveType ) ( sixName, sixType ) toBody =
    let
        arg1 =
            valueWith [] oneName oneType

        arg2 =
            valueWith [] twoName twoType

        arg3 =
            valueWith [] threeName threeType

        arg4 =
            valueWith [] fourName fourType

        arg5 =
            valueWith [] fiveName fiveType

        arg6 =
            valueWith [] sixName sixType

        (Compiler.Expression body) =
            toBody arg1 arg2 arg3 arg4 arg5 arg6
    in
    { documentation = Compiler.nodifyMaybe Nothing
    , signature =
        case body.annotation of
            Ok return ->
                Just
                    (Compiler.nodify
                        { name = Compiler.nodify (Compiler.formatValue name)
                        , typeAnnotation =
                            Compiler.nodify <|
                                Compiler.getInnerAnnotation <|
                                    Elm.Annotation.function
                                        [ oneType, twoType, threeType, fourType, fiveType, sixType ]
                                        (Compiler.noImports return)
                        }
                    )

            Err _ ->
                Nothing
    , declaration =
        Compiler.nodify
            { name = Compiler.nodify (Compiler.formatValue name)
            , arguments =
                [ Compiler.nodify (Pattern.VarPattern oneName)
                , Compiler.nodify (Pattern.VarPattern twoName)
                , Compiler.nodify (Pattern.VarPattern threeName)
                , Compiler.nodify (Pattern.VarPattern fourName)
                , Compiler.nodify (Pattern.VarPattern fiveName)
                , Compiler.nodify (Pattern.VarPattern sixName)
                ]
            , expression = Compiler.nodify body.expression
            }
    }
        |> Declaration.FunctionDeclaration
        |> Compiler.Declaration Compiler.NotExposed
            (Compiler.getAnnotationImports oneType
                ++ Compiler.getAnnotationImports twoType
                ++ Compiler.getAnnotationImports threeType
                ++ Compiler.getAnnotationImports fourType
                ++ Compiler.getAnnotationImports fiveType
                ++ Compiler.getAnnotationImports sixType
                ++ body.imports
            )


{-| Add documentation to a declaration!
-}
withDocumentation : String -> Declaration -> Declaration
withDocumentation =
    Compiler.documentation


{-| -}
expose : Declaration -> Declaration
expose =
    Compiler.expose


{-| -}
exposeAndGroup : String -> Declaration -> Declaration
exposeAndGroup =
    Compiler.exposeAndGroup


{-| -}
exposeConstructor : Declaration -> Declaration
exposeConstructor =
    Compiler.exposeConstructor


{-| -}
exposeConstructorAndGroup : String -> Declaration -> Declaration
exposeConstructorAndGroup =
    Compiler.exposeConstructorAndGroup


{-|

    import Elm.Annotation as Type

    Elm.portIncoming "receiveMessageFromTheWorld"
        [ Type.string
        , Type.int
        ]

Results in

    port receiveMessageFromTheWorld :
        (String -> Int -> msg)
        -> Sub msg

**Note** You generally only need one incoming and one outgoing port!

If you want to vary the messages going in and out of your app, don't use a huge number of ports, instead write Json encoders and decoders.

This will give you more flexibility in the future and save you having to wire up a bunch of stuff.

**Another note** - You may need to expose your port explicitly using `Elm.expose`

-}
portIncoming : String -> List Elm.Annotation.Annotation -> Declaration
portIncoming name args =
    { name = Compiler.nodify name
    , typeAnnotation =
        Compiler.nodify
            (case args of
                [] ->
                    Annotation.FunctionTypeAnnotation
                        (Compiler.nodify (Annotation.GenericType "msg"))
                        (Compiler.nodify sub)

                start :: remain ->
                    Annotation.FunctionTypeAnnotation
                        (groupAnn
                            (Compiler.nodify
                                (Compiler.getInnerAnnotation
                                    (Elm.Annotation.function
                                        args
                                        (Elm.Annotation.var "msg")
                                    )
                                )
                            )
                        )
                        (Compiler.nodify sub)
            )
    }
        |> Declaration.PortDeclaration
        |> Compiler.Declaration Compiler.NotExposed
            (List.concatMap Compiler.getAnnotationImports args)


groupAnn ann =
    Annotation.Tupled
        [ ann ]
        |> Compiler.nodify


sub : Annotation.TypeAnnotation
sub =
    Annotation.Typed
        (Compiler.nodify ( [ "Platform", "Sub" ], "Sub" ))
        [ Compiler.nodify (Annotation.GenericType "msg") ]


{-| Create a port that can send messages to the outside world!

    import Elm.Annotation as Type

    Elm.portOutgoing "tellTheWorld" Type.string

will generate

    port tellTheWorld : String -> Cmd msg

-}
portOutgoing : String -> Elm.Annotation.Annotation -> Declaration
portOutgoing name arg =
    { name = Compiler.nodify name
    , typeAnnotation =
        Compiler.nodify
            (Annotation.FunctionTypeAnnotation
                (Compiler.nodify (Compiler.getInnerAnnotation arg))
                (Compiler.nodify cmd)
            )
    }
        |> Declaration.PortDeclaration
        |> Compiler.Declaration Compiler.NotExposed (Compiler.getAnnotationImports arg)


cmd : Annotation.TypeAnnotation
cmd =
    Annotation.Typed
        (Compiler.nodify ( [ "Platform", "Cmd" ], "Cmd" ))
        [ Compiler.nodify (Annotation.GenericType "msg") ]



{- Infix operators!

   The goal is to make the following work


       one
           |> Elm.or two
           |> Elm.or three


       Elm.or one two




   We're not really worried about allowing operators to be partially applied in a way that results in the following code.

       (<=) 5

   I mean, come on, we're generating code.  Let's make it clearer.


   We're also not worried about recreating infix notation in this lib.  So no need to do:

       applyBinOp (int 2) plus (int 3)




-}


{-| Represents all of the binary operators allowed in Elm.
-}
type BinOp
    = BinOp String Infix.InfixDirection Int


{-| `>>`
-}
compose : Expression (a -> b) -> Expression (b -> c) -> Expression (a -> c)
compose =
    applyBinOp (BinOp ">>" Infix.Left 9)


{-| `<<`
-}
composeLeft : Expression (b -> c) -> Expression (a -> b) -> Expression (a -> c)
composeLeft =
    applyBinOp (BinOp "<<" Infix.Right 9)


{-| The to-the-power-of operator `^`
-}
power : Expression number -> Expression number -> Expression number
power =
    applyInfix (BinOp "^" Infix.Right 8)
        (valueWith
            []
            "^"
            (Elm.Annotation.function
                [ Elm.Annotation.var "number", Elm.Annotation.var "number" ]
                (Elm.Annotation.var "number")
            )
        )


{-| `*`
-}
multiply : Expression number -> Expression number -> Expression number
multiply =
    applyInfix (BinOp "*" Infix.Left 7)
        (valueWith
            []
            "*"
            (Elm.Annotation.function
                [ Elm.Annotation.var "number", Elm.Annotation.var "number" ]
                (Elm.Annotation.var "number")
            )
        )


{-| `/`
-}
divide : Expression Float -> Expression Float -> Expression Float
divide =
    applyInfix (BinOp "/" Infix.Left 7)
        (valueWith
            []
            "/"
            (Elm.Annotation.function
                [ Elm.Annotation.float, Elm.Annotation.float ]
                Elm.Annotation.float
            )
        )


{-| `//`
-}
intDivide : Expression Int -> Expression Int -> Expression Int
intDivide =
    applyInfix (BinOp "//" Infix.Left 7)
        (valueWith
            []
            "/"
            (Elm.Annotation.function
                [ Elm.Annotation.int, Elm.Annotation.int ]
                Elm.Annotation.int
            )
        )


{-| `+`
-}
plus : Expression number -> Expression number -> Expression number
plus =
    applyInfix (BinOp "+" Infix.Left 6)
        (valueWith
            []
            "max"
            (Elm.Annotation.function
                [ Elm.Annotation.var "number", Elm.Annotation.var "number" ]
                (Elm.Annotation.var "number")
            )
        )


{-| `-`
-}
minus : Expression number -> Expression number -> Expression number
minus =
    applyInfix (BinOp "-" Infix.Left 6)
        (valueWith
            []
            "max"
            (Elm.Annotation.function
                [ Elm.Annotation.var "number", Elm.Annotation.var "number" ]
                (Elm.Annotation.var "number")
            )
        )


{-| `++`
-}
append : Expression (List a) -> Expression (List a) -> Expression (List a)
append =
    applyInfix
        (BinOp "++" Infix.Right 5)
        (valueWith
            []
            "append"
            (Elm.Annotation.function
                [ Elm.Annotation.namedWith
                    []
                    "List"
                    [ Elm.Annotation.var "a" ]
                , Elm.Annotation.namedWith
                    []
                    "List"
                    [ Elm.Annotation.var "a" ]
                ]
                (Elm.Annotation.namedWith
                    []
                    "List"
                    [ Elm.Annotation.var "a" ]
                )
            )
        )


{-| `::`
-}
cons : Expression a -> Expression (List a) -> Expression (List a)
cons =
    applyInfix (BinOp "::" Infix.Right 5)
        (valueWith
            []
            "cons"
            (Elm.Annotation.function
                [ Elm.Annotation.var "a"
                , Elm.Annotation.list (Elm.Annotation.var "a")
                ]
                (Elm.Annotation.list (Elm.Annotation.var "a"))
            )
        )


{-| `==`
-}
equal : Expression a -> Expression a -> Expression Bool
equal =
    applyInfix (BinOp "==" Infix.Left 4)
        (valueWith
            []
            "equal"
            (Elm.Annotation.function
                [ Elm.Annotation.var "a"
                , Elm.Annotation.var "a"
                ]
                Elm.Annotation.bool
            )
        )


{-| `/=`
-}
notEqual : Expression a -> Expression a -> Expression Bool
notEqual =
    applyInfix (BinOp "/=" Infix.Left 4)
        (valueWith
            []
            "equal"
            (Elm.Annotation.function
                [ Elm.Annotation.var "a", Elm.Annotation.var "a" ]
                Elm.Annotation.bool
            )
        )


{-| `<`
-}
lt : Expression comparable -> Expression comparable -> Expression Bool
lt =
    applyInfix (BinOp "<" Infix.Non 4)
        (valueWith
            []
            "equal"
            (Elm.Annotation.function
                [ Elm.Annotation.var "comparable", Elm.Annotation.var "comparable" ]
                Elm.Annotation.bool
            )
        )


{-| `>`
-}
gt : Expression comparable -> Expression comparable -> Expression Bool
gt =
    applyInfix (BinOp ">" Infix.Non 4)
        (valueWith
            []
            "equal"
            (Elm.Annotation.function
                [ Elm.Annotation.var "comparable", Elm.Annotation.var "comparable" ]
                Elm.Annotation.bool
            )
        )


{-| `<=`
-}
lte : Expression comparable -> Expression comparable -> Expression Bool
lte =
    applyInfix (BinOp "<=" Infix.Non 4)
        (valueWith
            []
            "equal"
            (Elm.Annotation.function
                [ Elm.Annotation.var "comparable", Elm.Annotation.var "comparable" ]
                Elm.Annotation.bool
            )
        )


{-| `>=`
-}
gte : Expression comparable -> Expression comparable -> Expression Bool
gte =
    applyInfix (BinOp ">=" Infix.Non 4)
        (valueWith
            []
            "equal"
            (Elm.Annotation.function
                [ Elm.Annotation.var "comparable", Elm.Annotation.var "comparable" ]
                Elm.Annotation.bool
            )
        )


{-| `&&`
-}
and : Expression Bool -> Expression Bool -> Expression Bool
and =
    applyInfix (BinOp "&&" Infix.Right 3)
        (valueWith
            []
            "equal"
            (Elm.Annotation.function
                [ Elm.Annotation.bool, Elm.Annotation.bool ]
                Elm.Annotation.bool
            )
        )


{-| `||`
-}
or : Expression Bool -> Expression Bool -> Expression Bool
or =
    applyInfix (BinOp "||" Infix.Right 2)
        (valueWith
            []
            "equal"
            (Elm.Annotation.function
                [ Elm.Annotation.bool, Elm.Annotation.bool ]
                Elm.Annotation.bool
            )
        )



-- {-| used in the `elm/parser` library
-- `|=`
-- -}
-- keep : Expression -> Expression -> Expression
-- keep =
--     applyBinOp (BinOp "|=" Infix.Left 5)
-- {-| `|.`
-- -}
-- skip : Expression -> Expression -> Expression
-- skip =
--     applyBinOp (BinOp "|." Infix.Left 6)
-- {-| `</>` used in url parsing
-- -}
-- slash : Expression -> Expression -> Expression
-- slash =
--     applyBinOp (BinOp "</>" Infix.Right 7)
-- {-| `<?>` used in url parsing
-- -}
-- question : Expression -> Expression -> Expression
-- question =
--     applyBinOp (BinOp "<?>" Infix.Left 8)


{-| `|>`

    Elm.value "thang"
        |> Elm.pipe (Elm.value "thang2")
        |> Elm.pipe (Elm.value "thang3")

Results in

    thang
        |> thang2
        |> thang3

-}
pipe : Expression a -> Expression (a -> b) -> Expression b
pipe r l =
    applyBinOp (BinOp "|>" Infix.Left 0) r l


{-| `<|`
-}
pipeLeft : Expression (a -> b) -> Expression a -> Expression b
pipeLeft =
    applyBinOp (BinOp "<|" Infix.Right 0)


applyBinOp : BinOp -> Expression a -> Expression b -> Expression c
applyBinOp (BinOp symbol dir _) (Compiler.Expression exprl) (Compiler.Expression exprr) =
    Compiler.Expression
        { expression =
            Exp.OperatorApplication symbol dir (Compiler.nodify exprl.expression) (Compiler.nodify exprr.expression)
        , annotation = Err [ Compiler.SomeOtherIssue ]
        , imports = exprl.imports ++ exprr.imports
        , skip = False
        }


applyInfix : BinOp -> Expression a -> Expression b -> Expression c -> Expression d
applyInfix (BinOp symbol dir _) fnAnnotation (Compiler.Expression left) (Compiler.Expression right) =
    Compiler.Expression
        { expression =
            Exp.OperatorApplication symbol dir (Compiler.nodify left.expression) (Compiler.nodify right.expression)
        , annotation =
            Compiler.applyType fnAnnotation
                [ Compiler.Expression left
                , Compiler.Expression right
                ]
        , imports = left.imports ++ right.imports
        , skip = False
        }


{-| -}
pass : Expression skip
pass =
    Compiler.skip


{-| -}
parse : String -> Result String { declarations : List Declaration }
parse source =
    case Elm.Parser.parse source of
        Err deadends ->
            Err "Uh oh"

        Ok raw ->
            let
                parsedFile =
                    Elm.Processing.process Elm.Processing.init
                        raw

                exposedList =
                    Compiler.denode parsedFile.moduleDefinition
                        |> Elm.Syntax.Module.exposingList

                declarations =
                    List.map
                        (\dec ->
                            let
                                declar =
                                    Compiler.denode dec
                            in
                            ( Node.range dec
                                |> (.start >> .row)
                            , Compiler.Declaration
                                (determineExposure declar exposedList)
                                []
                                declar
                            )
                        )
                        parsedFile.declarations

                comments =
                    List.map
                        (\nodedComment ->
                            ( Node.range nodedComment
                                |> (.start >> .row)
                            , Compiler.Comment
                                (Compiler.denode nodedComment)
                            )
                        )
                        parsedFile.comments
            in
            Ok
                { declarations =
                    List.sortBy
                        Tuple.first
                        (declarations ++ comments)
                        |> List.map Tuple.second
                }


determineExposure : Declaration.Declaration -> Expose.Exposing -> Compiler.Expose
determineExposure dec exposedDec =
    case exposedDec of
        Expose.All _ ->
            Compiler.Exposed { group = Nothing, exposeConstructor = True }

        Expose.Explicit nodes ->
            case dec of
                Declaration.FunctionDeclaration myFn ->
                    case Compiler.denode myFn.declaration of
                        implementation ->
                            case Compiler.denode implementation.name of
                                name ->
                                    if List.any (valueIsExposed name) nodes then
                                        Compiler.Exposed { group = Nothing, exposeConstructor = False }

                                    else
                                        Compiler.NotExposed

                Declaration.AliasDeclaration typeAlias ->
                    case Compiler.denode typeAlias.name of
                        name ->
                            if List.any (typeIsExposed name) nodes then
                                Compiler.Exposed { group = Nothing, exposeConstructor = False }

                            else
                                Compiler.NotExposed

                Declaration.CustomTypeDeclaration type_ ->
                    case Compiler.denode type_.name of
                        name ->
                            if List.any (typeIsExposed name) nodes then
                                Compiler.Exposed { group = Nothing, exposeConstructor = False }

                            else if List.any (typeConstructorIsExposed name) nodes then
                                Compiler.Exposed { group = Nothing, exposeConstructor = True }

                            else
                                Compiler.NotExposed

                Declaration.PortDeclaration sig ->
                    Compiler.NotExposed

                Declaration.InfixDeclaration infixDec ->
                    Compiler.NotExposed

                Declaration.Destructuring pattern exp ->
                    Compiler.NotExposed


valueIsExposed : String -> Node.Node Expose.TopLevelExpose -> Bool
valueIsExposed name node =
    case Compiler.denode node of
        Expose.InfixExpose _ ->
            False

        Expose.FunctionExpose fnName ->
            fnName == name

        Expose.TypeOrAliasExpose _ ->
            False

        Expose.TypeExpose _ ->
            False


typeIsExposed : String -> Node.Node Expose.TopLevelExpose -> Bool
typeIsExposed name node =
    case Compiler.denode node of
        Expose.InfixExpose _ ->
            False

        Expose.FunctionExpose fnName ->
            False

        Expose.TypeOrAliasExpose typeName ->
            name == typeName

        Expose.TypeExpose _ ->
            False


typeConstructorIsExposed : String -> Node.Node Expose.TopLevelExpose -> Bool
typeConstructorIsExposed name node =
    case Compiler.denode node of
        Expose.InfixExpose _ ->
            False

        Expose.FunctionExpose fnName ->
            False

        Expose.TypeOrAliasExpose typeName ->
            name == typeName

        Expose.TypeExpose myType ->
            name == myType.name
