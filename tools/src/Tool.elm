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
import Cmd.Extra exposing (withCmd, withCmds, withNoCmd)
import Command
import File exposing (File)
import File.Select as Select
import Model exposing (Model, Msg(..))
import Platform exposing (Program)
import Task


main : Program Flags Model Msg
main =
    Platform.worker
        { init = init
        , update = update
        , subscriptions = subscriptions
        }


type alias Flags =
    ()


init : Flags -> ( Model, Cmd Msg )
init _ =
    { sourceText = Nothing } |> withNoCmd


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Input input ->
            commandProcessor model input

        SourceRequested ->
            model |> withCmd (Select.file [ "text/txt" ] SourceSelected)

        SourceSelected file ->
            model |> withCmd (Task.perform SourceLoaded (File.toString file))

        SourceLoaded content ->
            { model | sourceText = Just content } |> withNoCmd


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
            executeCommand model cmd input_ args


executeCommand : Model -> String -> String -> ArgList -> ( Model, Cmd Msg )
executeCommand model cmd input args =
    case cmd of
        "echo" ->
            Command.echo model input args

        "arg" ->
            Command.displayInput model input args

        "ps" ->
            Command.parse model input args

        _ ->
            Command.null model input args


subscriptions : Model -> Sub Msg
subscriptions _ =
    Command.get Input



{- Below is the input-to-output transformation. -}
