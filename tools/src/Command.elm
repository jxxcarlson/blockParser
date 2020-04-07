port module Command exposing
    ( display
    , echo
    , edit
    , get
    , help
    , helpCmd
    , load
    , loadFile
    , parse
    , prunedTree
    , put
    , rcl
    , receiveData
    , spanningTree
    , sto
    )

import ArgList exposing (ArgList)
import BLParser.Block as Block
import BLParser.BlockTree as BlockTree
import BLParser.Edit as Edit
import BLParser.Id as Id
import BLParser.Parse as Parse
import BLParser.Source as Source
import Cmd.Extra exposing (withCmd, withCmds, withNoCmd)
import HTree
import Json.Encode as E
import Model exposing (Model, Msg(..))
import Tree
import Tree.Extra



-- https://elmprogramming.com/receiving-data-from-javascript.html


port get : (String -> msg) -> Sub msg


port put : String -> Cmd msg


port sendFileName : E.Value -> Cmd msg


port receiveData : (E.Value -> msg) -> Sub msg


load : Model -> ArgList -> String -> ( Model, Cmd Msg )
load model _ input =
    { model | registerM = Just input } |> withCmd (put "loaded into register M")


getRegister : Model -> String -> Maybe String
getRegister model registerName =
    case registerName of
        "a" ->
            model.registerA

        "b" ->
            model.registerB

        "c" ->
            model.registerC

        "d" ->
            model.registerD

        "e" ->
            model.registerE

        "f" ->
            model.registerF

        "m" ->
            model.registerM

        _ ->
            model.registerM


setRegister : String -> String -> Model -> Model
setRegister registerName registerContents model =
    case registerName of
        "a" ->
            { model | registerA = Just registerContents }

        "b" ->
            { model | registerB = Just registerContents }

        "c" ->
            { model | registerC = Just registerContents }

        "d" ->
            { model | registerD = Just registerContents }

        "e" ->
            { model | registerE = Just registerContents }

        "f" ->
            { model | registerF = Just registerContents }

        "m" ->
            { model | registerM = Just registerContents }

        _ ->
            model


display : Model -> ArgList -> String -> ( Model, Cmd Msg )
display model argList _ =
    let
        reg_ =
            ArgList.get 0 argList

        reg =
            if reg_ == "_none_" then
                "m"

            else
                reg_
    in
    model |> displayRegisterContents (String.toUpper reg) (getRegister model reg)


echo : Model -> ArgList -> String -> ( Model, Cmd Msg )
echo model _ input =
    model |> withCmd (put ("echo: " ++ input))


edit : Model -> ArgList -> String -> ( Model, Cmd Msg )
edit model argList _ =
    let
        from_ =
            String.toInt (ArgList.get 0 argList)

        to_ =
            String.toInt (ArgList.get 1 argList)

        insertionTextRegister =
            ArgList.get 2 argList

        sourceTextRegister =
            ArgList.get 3 argList
    in
    case ( from_, to_ ) of
        ( Nothing, _ ) ->
            model |> withCmd (put "'from' is not an integer")

        ( _, Nothing ) ->
            model |> withCmd (put "'to' is not an integer")

        ( Just from, Just to ) ->
            let
                insertionText_ =
                    getRegister model insertionTextRegister

                sourceText_ =
                    getRegister model sourceTextRegister
            in
            case ( insertionText_, sourceText_ ) of
                ( Nothing, _ ) ->
                    model |> withCmd (put "No insertion text")

                ( _, Nothing ) ->
                    model |> withCmd (put "No source text")

                ( Just insertionText, Just sourceText ) ->
                    let
                        parserState =
                            Parse.parseString Id.initial sourceText

                        insertionSource =
                            Source.fromString insertionText

                        newParserState =
                            Edit.edit from to insertionSource parserState
                    in
                    case Maybe.map Parse.getSource newParserState of
                        Nothing ->
                            model |> withCmd (put "Edit: error")

                        Just source ->
                            let
                                so =
                                    Debug.log "SO" (source |> Source.toString)
                            in
                            model |> setRegister "m" so |> withCmd (put "Edited text > M")


help : Model -> ( Model, Cmd Msg )
help model =
    model |> withCmd helpCmd


helpCmd : Cmd Msg
helpCmd =
    put helpText


sto : Model -> ArgList -> String -> ( Model, Cmd Msg )
sto model argList _ =
    let
        reg =
            ArgList.get 0 argList

        message =
            "M > " ++ String.toUpper reg
    in
    case model.registerM of
        Nothing ->
            model |> withCmd (put "no change in registers")

        Just registerContents ->
            setRegister reg registerContents model |> withCmd (put message)


loadFile : Model -> ArgList -> ( Model, Cmd Msg )
loadFile model argList =
    let
        fileName =
            ArgList.get 0 argList
    in
    model |> withCmd (loadFileCmd fileName)


loadFileCmd : String -> Cmd msg
loadFileCmd fileName =
    sendFileName (E.string <| "./source/" ++ fileName)


rcl model argList _ =
    let
        reg =
            ArgList.get 0 argList

        message =
            String.toUpper reg ++ " > M"
    in
    case getRegister model reg of
        Nothing ->
            model |> withCmd (put ("M is empty; no change to " ++ String.toUpper reg))

        Just registerContents ->
            setRegister "m" registerContents model |> withCmd (put message)


displayRegisterContents : String -> Maybe String -> Model -> ( Model, Cmd Msg )
displayRegisterContents registerName maybeStr =
    case maybeStr of
        Nothing ->
            withCmd (put <| "Nothing in register " ++ registerName)

        Just contents ->
            let
                contents_ =
                    contents
                        |> String.lines
                        |> List.indexedMap (\index line -> String.fromInt index ++ ": " ++ line)
                        |> String.join "\n"
            in
            withCmd (put <| "register " ++ registerName ++ ":\n" ++ contents_)


parse : Model -> ArgList -> String -> ( Model, Cmd Msg )
parse model argList _ =
    let
        reg_ =
            ArgList.get 0 argList

        reg =
            case reg_ of
                "_none_" ->
                    "m"

                _ ->
                    reg_
    in
    model |> parseRegisterContents reg (getRegister model reg)


spanningTree : Model -> ArgList -> ( Model, Cmd Msg )
spanningTree model argList =
    let
        from_ =
            String.toInt (ArgList.get 0 argList)

        to_ =
            String.toInt (ArgList.get 1 argList)

        sourceTextRegister =
            ArgList.get 2 argList
    in
    case ( from_, to_ ) of
        ( Nothing, _ ) ->
            model |> withCmd (put "'from' is not an integer")

        ( _, Nothing ) ->
            model |> withCmd (put "'to' is not an integer")

        ( Just from, Just to ) ->
            let
                sourceText_ =
                    getRegister model sourceTextRegister
            in
            case sourceText_ of
                Nothing ->
                    model |> withCmd (put "No source text")

                Just sourceText ->
                    let
                        parserState =
                            Parse.parseString Id.initial sourceText

                        maybeSpanningTree : Maybe (Tree.Tree Block.Block)
                        maybeSpanningTree =
                            Edit.spanningTreeOfSourceRange from to parserState
                    in
                    case maybeSpanningTree of
                        Nothing ->
                            model |> withCmd (put "Spanning tree: error")

                        Just tree ->
                            let
                                output =
                                    tree
                                        |> Tree.map (\b -> ( Block.stringOf b, Block.idOf b ))
                                        |> Tree.Extra.tagWithDepth
                                        |> HTree.toOutline stringOfNode
                            in
                            model |> withCmd (put output)


prunedTree : Model -> ArgList -> ( Model, Cmd Msg )
prunedTree model argList =
    let
        from_ =
            String.toInt (ArgList.get 0 argList)

        to_ =
            String.toInt (ArgList.get 1 argList)

        sourceTextRegister =
            ArgList.get 2 argList
    in
    case ( from_, to_ ) of
        ( Nothing, _ ) ->
            model |> withCmd (put "'from' is not an integer")

        ( _, Nothing ) ->
            model |> withCmd (put "'to' is not an integer")

        ( Just from, Just to ) ->
            let
                sourceText_ =
                    getRegister model sourceTextRegister
            in
            case sourceText_ of
                Nothing ->
                    model |> withCmd (put "No source text")

                Just sourceText ->
                    let
                        parserState =
                            Parse.parseString Id.initial sourceText

                        maybePrunedTree =
                            Edit.separate from to parserState
                                |> Maybe.map .prunedTree
                    in
                    case maybePrunedTree of
                        Nothing ->
                            model |> withCmd (put "Spanning tree: error")

                        Just tree ->
                            let
                                output =
                                    tree
                                        |> Tree.map (\b -> ( Block.stringOf b, Block.idOf b ))
                                        |> Tree.Extra.tagWithDepth
                                        |> HTree.toOutline stringOfNode
                            in
                            model |> withCmd (put output)


parseRegisterContents : String -> Maybe String -> Model -> ( Model, Cmd Msg )
parseRegisterContents registerName maybeStr =
    case maybeStr of
        Nothing ->
            withCmd (put <| "Nothing in register " ++ registerName)

        Just contents ->
            withCmd (put <| "register " ++ String.toUpper registerName ++ " parse tree:\n" ++ transform contents)


transform : String -> String
transform input_ =
    let
        input =
            String.trim input_

        output =
            BlockTree.blockTreeOfString input
                -- |> Tree.map (Block.stringOf >> String.replace "\n" "•")
                |> Tree.map (\b -> ( Block.stringOf b, Block.idOf b ))
                |> Tree.Extra.tagWithDepth
                |> HTree.toOutline stringOfNode
    in
    output


stringOfNode : ( ( String, Maybe Id.Id ), Int ) -> String
stringOfNode ( ( str, maybeId ), depth ) =
    let
        id =
            maybeId
                |> Maybe.withDefault Id.initial
                |> Id.stringValue
    in
    String.fromInt depth ++ " (" ++ id ++ "): " ++ String.replace "\n" " ‡ " str


helpText =
    """---------------------------------------------------------------------------------------
Classic HP-style calculator for Parser operations
---------------------------------------------------------------------------------------
This calculator has registers A, B, C, D, E, F, and M, each of which can hold a string.
Registers A, B, M are loaded on startup.  Type 'd' to display register M, type 'p' to
parse its contents.  The symbol • which you see in the parsed output is shorthand for
a newline.  There is one node per line, with indentation and an integer label indicating
the depth of the node in the parse tree.

Type 'd a' to display register A. Etc.

The command 'e 2 3 b a' will edit the text in A, replacing lines 2-3 by the text in B.
The result is placed in register M


Command summary
---------------------------------------------------------------------------------------
> .load source/t1 -- load file source/t1; it will be stored in register M
> d               -- display contents of register M
> d a             -- display contents of register As
> p               -- parse contents of M
> p a             -- parse contents of A
> rcl a           -- store contents of A in M
> sto a           -- store contents of M in A
> e i j b a       -- edit the source in A, replacing lines i to j by the text in A
> h               -- show help
---------------------------------------------------------------------------------------
"""
