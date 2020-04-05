port module Command exposing
    ( displayInput
    , echo
    , get
    , load
    , null
    , parse
    , put
    )

import ArgList exposing (ArgList)
import BLParser.Block as Block
import BLParser.BlockTree as BlockTree
import Cmd.Extra exposing (withCmd, withCmds, withNoCmd)
import File exposing (File)
import File.Select as Select
import HTree
import Model exposing (Model, Msg(..))
import Task
import Tree
import Tree.Extra


port get : (String -> msg) -> Sub msg


port put : String -> Cmd msg


displayInput : model -> String -> ArgList -> ( model, Cmd Msg )
displayInput model input args =
    let
        arg =
            String.dropLeft
                3
                input
                |> String.replace "\\" "\n"
    in
    model |> withCmd (put <| "arg: " ++ arg)


echo : Model -> String -> ArgList -> ( Model, Cmd Msg )
echo model input args =
    model |> withCmd (put ("echo: " ++ input))


load : Model -> String -> ArgList -> ( Model, Cmd Msg )
load model input args =
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


parse : Model -> String -> ArgList -> ( Model, Cmd Msg )
parse model input args =
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


null : Model -> String -> ArgList -> ( Model, Cmd Msg )
null model input args =
    model |> withCmd (put <| "Unrecognized command")


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


putTransformation : String -> Cmd msg
putTransformation kIn =
    put (transform kIn)
