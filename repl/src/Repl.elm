module Repl exposing (main)

{-| A repl for experimenting with BlockParser
-}

import ArgList exposing (ArgList)
import Cmd.Extra exposing (withCmd, withNoCmd)
import Command
import Json.Decode as Decode
import Json.Encode as Encode
import Model exposing (Flags, Model, Msg(..), initModel)
import Platform exposing (Program)


main : Program Flags Model Msg
main =
    Platform.worker
        { init = \f -> ( initModel f, Cmd.none )
        , update = update
        , subscriptions = subscriptions
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Input input ->
            case input == "" of
                True ->
                    model |> withNoCmd

                False ->
                    commandProcessor model input

        ReceivedDataFromJS value ->
            case decodeFileContents value of
                Nothing ->
                    model |> withCmd (Command.put "Couldn't load file")

                Just data ->
                    { model | registerM = Just data } |> withCmd (Command.put "File > M")


commandProcessor : Model -> String -> ( Model, Cmd Msg )
commandProcessor model input =
    let
        args_ =
            input |> String.words |> List.map String.trim

        args =
            List.drop 1 args_
                |> ArgList.init
    in
    case List.head args_ of
        Nothing ->
            model |> withNoCmd

        Just cmd ->
            let
                input_ =
                    String.dropLeft (String.length cmd) input
                        |> String.trim
            in
            executeCommand model cmd args input_


executeCommand : Model -> String -> ArgList -> String -> ( Model, Cmd Msg )
executeCommand model cmd args input =
    case cmd of
        "r" ->
            Command.display model args

        "a" ->
            Command.display model (ArgList.init [ "a" ])

        "b" ->
            Command.display model (ArgList.init [ "b" ])

        "c" ->
            Command.display model (ArgList.init [ "c" ])

        "d" ->
            Command.display model (ArgList.init [ "d" ])

        "e" ->
            Command.display model (ArgList.init [ "e" ])

        "f" ->
            Command.display model (ArgList.init [ "f" ])

        "m" ->
            Command.display model (ArgList.init [ "m" ])

        "edit" ->
            Command.edit model args

        "h" ->
            Command.help model

        "load" ->
            Command.loadFile model args

        "p" ->
            Command.parse model args

        "pt" ->
            Command.prunedTree model args

        "rcl" ->
            Command.rcl model args

        "st" ->
            Command.spanningTree model args

        "sto" ->
            Command.sto model args

        _ ->
            model |> withCmd (Command.put "")


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch [ Command.get Input, Command.receiveData ReceivedDataFromJS ]


decodeFileContents : Encode.Value -> Maybe String
decodeFileContents value =
    case Decode.decodeValue Decode.string value of
        Ok str ->
            Just str

        Err _ ->
            Nothing
