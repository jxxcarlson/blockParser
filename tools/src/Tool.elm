module Tool exposing (main)

{-| A simple Platform.worker program with
a simple command-line interface:

`$ sh make.sh` -- (1)
`$ chmod u+x cli; alias cli='./cli'` -- (2)
`$ cli 77` -- (3)
`232`

1.  Compile Main.elm to `./run/main.js` and
    copy `src/cli.js` to `./run/cli.js`

2.  Make `cli` executable and make an alias for it
    to avoid awkward typing.

3.  Try it out. The program `cli.js` communicates
    with runtime for the `Platform.worker` program.
    The worker accepts input, computes some output,
    and send the output back through ports.

To do something more interesting, replace
the `transform` function in `Main.elm`.

-}

import ArgList exposing (ArgList)
import Cmd.Extra exposing (withCmd, withNoCmd)
import Command
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
            model |> withNoCmd


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
        "echo" ->
            Command.echo model args input

        "d" ->
            Command.display model args input

        "e" ->
            Command.edit model args input

        "h" ->
            Command.help model

        "l" ->
            Command.loadFile model args

        "p" ->
            Command.parse model args input

        "pt" ->
            Command.prunedTree model args

        "rcl" ->
            Command.rcl model args input

        "st" ->
            Command.spanningTree model args

        "sto" ->
            Command.sto model args input

        _ ->
            model |> withCmd (Command.put "")


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch [ Command.get Input, Command.receiveData ReceivedDataFromJS ]
