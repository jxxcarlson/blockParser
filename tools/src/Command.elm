port module Command exposing
    ( display
    , displayInput
    , echo
    , get
    , load
    , parse
    , parseInput
    , put
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


echo : Model -> ArgList -> String -> ( Model, Cmd Msg )
echo model _ input =
    model |> withCmd (put ("echo: " ++ input))


{-|

    ld a t1 -- load register a with contents of file source/t1
    ld b t2 -- load register b with contents of file source/t1
    -- there are only two registers

-}
load : Model -> ArgList -> String -> ( Model, Cmd Msg )
load model _ input =
    { model | registerM = Just input } |> withNoCmd


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


sto : Model -> ArgList -> String -> Model
sto model argList _ =
    let
        register =
            ArgList.get 0 argList
    in
    case register of
        "a" ->
            { model | registerA = model.registerM }

        "b" ->
            { model | registerA = model.registerM }

        "c" ->
            { model | registerA = model.registerM }

        "d" ->
            { model | registerA = model.registerM }

        "e" ->
            { model | registerA = model.registerM }

        "f" ->
            { model | registerA = model.registerM }

        _ ->
            model


displayRegisterContents : String -> Maybe String -> Model -> ( Model, Cmd Msg )
displayRegisterContents registerName maybeStr =
    case maybeStr of
        Nothing ->
            withCmd (put <| "Nothing in register " ++ registerName)

        Just contents ->
            withCmd (put contents)


parse : Model -> ArgList -> String -> ( Model, Cmd Msg )
parse model args input =
    case model.registerM of
        Nothing ->
            model |> withCmd (put <| "Nothing to parse")

        Just registerContents ->
            model |> withCmd (putTransformedString registerContents)


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
