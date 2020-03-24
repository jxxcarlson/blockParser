module Block exposing (Block, BlockData, BlockType(..), getBlock)

import Array exposing (Array)
import Loop exposing (Step(..), loop)


getBlock : Int -> Array String -> BlockData
getBlock blockStart array =
    loop (init blockStart array) nextBlockState


type alias Block =
    { id : String
    , content : Array ( Int, String )
    , kind : BlockType
    , args : List String
    }


type BlockType
    = Paragraph
    | Section Int
    | Math
    | Quotation (List String)
    | Environment (List String)


isTightBlock : List String -> Bool
isTightBlock args =
    case List.head args of
        Nothing ->
            True

        Just name ->
            not (List.member name looseBlockNames)


looseBlockNames =
    [ "quotation" ]


type alias BlockState =
    { currentLineNumber : Int
    , array : Array String
    , blockStart : Int
    , blockEnd : Int
    , arrayLength : Int
    , scanning : ScanState
    }


type ScanState
    = BeginScan
    | InTightBlock String
    | InLooseBlock String
    | InParagraph
    | EndScan


type alias BlockData =
    { blockStart : Int
    , blockEnd : Int
    , array : Array String
    }


{-| Think of the List String as the arguments of a BlockHeading

    Example: | section 1 Intro
    --> BlockHeading ["section", "1", "Intro"]

-}
type LineType
    = Blank
    | Text
    | BlockHeading (List String)
    | BlockEnd String


type alias Line =
    { lineNumber : Int
    , content : String
    , lineType : LineType
    }


init : Int -> Array String -> BlockState
init line array =
    let
        length =
            Array.length array
    in
    { currentLineNumber = line
    , array = array
    , blockStart = line
    , blockEnd = length
    , arrayLength = length
    , scanning = BeginScan
    }


nextBlockState : BlockState -> Step BlockState BlockData
nextBlockState blockState =
    if blockState.currentLineNumber >= blockState.arrayLength || blockState.scanning == EndScan then
        Done
            { blockStart = blockState.blockStart
            , blockEnd = blockState.currentLineNumber
            , array = Array.slice blockState.blockStart blockState.blockEnd blockState.array
            }

    else
        case Array.get blockState.currentLineNumber blockState.array of
            Nothing ->
                Done
                    { blockStart = blockState.blockStart
                    , blockEnd = blockState.currentLineNumber
                    , array = Array.slice blockState.blockStart blockState.blockEnd blockState.array
                    }

            Just line ->
                let
                    lineData =
                        classify blockState.currentLineNumber line
                in
                case lineData.lineType of
                    Blank ->
                        case blockState.scanning of
                            InParagraph ->
                                Loop { blockState | scanning = EndScan }

                            InTightBlock _ ->
                                Loop { blockState | scanning = EndScan }

                            _ ->
                                Loop { blockState | currentLineNumber = blockState.currentLineNumber + 1 }

                    Text ->
                        if blockState.scanning == BeginScan then
                            Loop { blockState | scanning = InParagraph, currentLineNumber = blockState.currentLineNumber + 1 }

                        else if blockState.scanning == InParagraph then
                            Loop { blockState | currentLineNumber = blockState.currentLineNumber + 1 }

                        else
                            Loop { blockState | scanning = EndScan }

                    BlockHeading args ->
                        if blockState.scanning == BeginScan then
                            Loop { blockState | scanning = EndScan }

                        else
                            Loop { blockState | scanning = EndScan }

                    BlockEnd name ->
                        Loop { blockState | scanning = EndScan }


{-|

    > classify 1 "| Math"
    Line { content = "| Math", lineNumber = 1, lineType = BlockHeading ["Math"] }

    >  classify 1 "| Section 1 Introduction"
    Line { content = "| Section 1 Introduction", lineNumber = 1, lineType = BlockHeading ["Section","1","Introduction"] }

    > classify 1 ".quote"
    Line { content = ".quote", lineNumber = 1, lineType = BlockEnd "quote" }

    > classify 1 "This is a test."
    Line { content = "This is a test.", lineNumber = 1, lineType = Text }

    > classify 1 ""
    Line { content = "", lineNumber = 1, lineType = Blank }

-}
classify : Int -> String -> Line
classify lineNumber str =
    case String.left 1 str of
        "|" ->
            parseBlockHeading lineNumber str

        "." ->
            parseBlockEnd lineNumber str

        _ ->
            parseTextLine lineNumber str


parseBlockHeading : Int -> String -> Line
parseBlockHeading lineNumber str =
    let
        args : List String
        args =
            str
                |> String.dropLeft 1
                |> String.words
                |> List.map String.trim
    in
    { lineNumber = lineNumber
    , content = str
    , lineType = BlockHeading args
    }


parseBlockEnd : Int -> String -> Line
parseBlockEnd lineNumber str =
    let
        arg : String
        arg =
            str
                |> String.dropLeft 1
                |> String.trim
    in
    { lineNumber = lineNumber
    , content = str
    , lineType = BlockEnd arg
    }


parseTextLine : Int -> String -> Line
parseTextLine lineNumber str =
    let
        lineType =
            if str == "" then
                Blank

            else
                Text
    in
    { lineNumber = lineNumber
    , content = str
    , lineType = lineType
    }
