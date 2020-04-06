port module Command exposing
    ( display
    , displayInput
    , echo
    , get
    , help
    , helpCmd
    , load
    , parse
    , parseInput
    , put
    , rcl
    , sto
    )

import ArgList exposing (ArgList)
import BLParser.Block as Block
import BLParser.BlockTree as BlockTree
import Cmd.Extra exposing (withCmd, withNoCmd)
import HTree
import Model exposing (Model, Msg(..))
import Tree
import Tree.Extra


port get : (String -> msg) -> Sub msg


port put : String -> Cmd msg


displayInput : Model -> ArgList -> String -> ( Model, Cmd Msg )
displayInput model _ input =
    let
        arg =
            String.dropLeft
                3
                input
                |> String.replace "\\" "\n"
    in
    model |> withCmd (put <| "arg: " ++ arg)


{-|

    ld a t1 -- load register a with contents of file source/t1
    ld b t2 -- load register b with contents of file source/t1
    -- there are only two registers

-}
load : Model -> ArgList -> String -> ( Model, Cmd Msg )
load model _ input =
    { model | registerM = Just input } |> withCmd (put "loaded into register M")


parseInput : Model -> ArgList -> String -> ( Model, Cmd Msg )
parseInput model args input =
    let
        arg =
            String.dropLeft
                3
                input
                |> String.replace "\\" "\n"

        output =
            transform arg
    in
    model |> withCmd (put <| output)


display : Model -> ArgList -> String -> ( Model, Cmd Msg )
display model argList _ =
    let
        register =
            ArgList.get 0 argList
    in
    case register of
        "a" ->
            model |> displayRegisterContents "A" model.registerA

        "b" ->
            model |> displayRegisterContents "B" model.registerB

        "c" ->
            model |> displayRegisterContents "C" model.registerC

        "d" ->
            model |> displayRegisterContents "D" model.registerD

        "e" ->
            model |> displayRegisterContents "E" model.registerE

        "f" ->
            model |> displayRegisterContents "F" model.registerF

        "m" ->
            model |> displayRegisterContents "M" model.registerM

        _ ->
            model |> displayRegisterContents "M" model.registerM


echo : Model -> ArgList -> String -> ( Model, Cmd Msg )
echo model _ input =
    model |> withCmd (put ("echo: " ++ input))


help : Model -> ( Model, Cmd Msg )
help model =
    model |> withCmd helpCmd


helpCmd : Cmd Msg
helpCmd =
    put helpText


sto : Model -> ArgList -> String -> ( Model, Cmd Msg )
sto model argList _ =
    let
        register =
            ArgList.get 0 argList
    in
    case register of
        "a" ->
            { model | registerA = model.registerM } |> withCmd (put "M > A")

        "b" ->
            { model | registerB = model.registerM } |> withCmd (put "M > B")

        "c" ->
            { model | registerC = model.registerM } |> withCmd (put "M > C")

        "d" ->
            { model | registerD = model.registerM } |> withCmd (put "M > D")

        "e" ->
            { model | registerE = model.registerM } |> withCmd (put "M > E")

        "f" ->
            { model | registerF = model.registerM } |> withCmd (put "M > F")

        _ ->
            model |> withCmd (put "no change in registers")


rcl model argList _ =
    let
        register =
            ArgList.get 0 argList
    in
    case register of
        "a" ->
            { model | registerM = model.registerA } |> withCmd (put "A > M")

        "b" ->
            { model | registerM = model.registerB } |> withCmd (put "B > M")

        "c" ->
            { model | registerM = model.registerC } |> withCmd (put "C > M")

        "d" ->
            { model | registerM = model.registerD } |> withCmd (put "D > M")

        "e" ->
            { model | registerM = model.registerE } |> withCmd (put "D > M")

        "f" ->
            { model | registerM = model.registerF } |> withCmd (put "F > M")

        _ ->
            model |> withCmd (put "no change in registers")


displayRegisterContents : String -> Maybe String -> Model -> ( Model, Cmd Msg )
displayRegisterContents registerName maybeStr =
    case maybeStr of
        Nothing ->
            withCmd (put <| "Nothing in register " ++ registerName)

        Just contents ->
            withCmd (put <| "register " ++ registerName ++ ": " ++ contents)


parse : Model -> ArgList -> String -> ( Model, Cmd Msg )
parse model argList input =
    case model.registerM of
        Nothing ->
            model |> withCmd (put <| "Nothing to parse")

        Just registerContents ->
            let
                register =
                    ArgList.get 0 argList
            in
            case register of
                "a" ->
                    model |> parseRegisterContents "A" model.registerA

                "b" ->
                    model |> parseRegisterContents "B" model.registerB

                "c" ->
                    model |> parseRegisterContents "C" model.registerC

                "d" ->
                    model |> parseRegisterContents "D" model.registerD

                "e" ->
                    model |> parseRegisterContents "E" model.registerE

                "f" ->
                    model |> parseRegisterContents "F" model.registerF

                "m" ->
                    model |> parseRegisterContents "M" model.registerM

                _ ->
                    model |> parseRegisterContents "M" model.registerM


parseRegisterContents : String -> Maybe String -> Model -> ( Model, Cmd Msg )
parseRegisterContents registerName maybeStr =
    case maybeStr of
        Nothing ->
            withCmd (put <| "Nothing in register " ++ registerName)

        Just contents ->
            withCmd (put <| "register " ++ registerName ++ " parse tree:\n" ++ transform contents)


transform : String -> String
transform input_ =
    let
        input =
            String.trim input_

        output =
            BlockTree.blockTreeOfString input
                |> Tree.map (Block.stringOf >> String.replace "\n" "@")
                |> Tree.Extra.tagWithDepth
                |> HTree.toOutline (\( b, d ) -> String.fromInt d ++ ": " ++ b)
    in
    output


putTransformedString : String -> Cmd msg
putTransformedString input =
    put (transform input)


helpText =
    """  ---------------------------------------------------------------------------------------
Classic HP-style calculator for Parser operations
---------------------------------------------------------------------------------------
This calculator has registers A, B, C, D, E, F, and M, each of which can hold a string.

> .load source/t1 -- load file source/t1; it will be stored in register M
> d               -- display contents of register M
> d a             -- display contents of register As
> p               -- parse contents of M
> p a             -- parse contents of A
> rcl a           -- store contents of A in M
> sto a           -- store contents of M in A
> h               -- show help
---------------------------------------------------------------------------------------
"""
