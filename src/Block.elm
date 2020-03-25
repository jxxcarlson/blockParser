module Block exposing (Block, BlockData, BlockType(..), arrayFromString, getBlock)

import Array exposing (Array)
import Loop exposing (Step(..), loop)


arrayFromString : String -> Array String
arrayFromString str =
    str
        |> String.lines
        |> Array.fromList


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
    = None
    | Paragraph
    | Section Int (List String)
    | Math
    | Quotation (List String)
    | Environment (List String)


type BlockKind
    = Tight String
    | Loose String
    | Unclassified


type BlockScanState
    = BeginScan
    | InTightBlock
    | InLooseBlock
    | InParagraph
    | EndScan


type alias BlockData =
    { blockStart : Int -- index in source array
    , blockEnd : Int -- index in source array
    , array : Array String -- slice of source array
    , blockType : BlockType
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


blockType : List String -> BlockType
blockType args =
    case List.head args of
        Nothing ->
            Paragraph

        Just name ->
            case name of
                "part" ->
                    Section -1 (List.drop 1 args)

                "chapter" ->
                    Section 0 (List.drop 1 args)

                "section" ->
                    Section 1 (List.drop 1 args)

                "subsection" ->
                    Section 2 (List.drop 1 args)

                "subsubsection" ->
                    Section 3 (List.drop 1 args)

                "subsubsubsection" ->
                    Section 4 (List.drop 1 args)

                "subheading" ->
                    Section 5 (List.drop 1 args)

                "math" ->
                    Math

                "quotation" ->
                    Quotation (List.drop 1 args)

                "env" ->
                    Environment (List.drop 1 args)

                _ ->
                    -- maybe Error instead?
                    Paragraph


getBlockKind : List String -> BlockKind
getBlockKind args =
    case List.head args of
        Nothing ->
            Unclassified

        Just name ->
            case List.member name looseBlockNames of
                True ->
                    Loose name

                False ->
                    Tight name


looseBlockNames =
    [ "quotation" ]


type alias BlockState =
    { currentLineNumber : Int
    , array : Array String
    , blockStart : Int
    , blockEnd : Int
    , arrayLength : Int
    , scanning : BlockScanState
    , blockType : BlockType
    , blockKind : BlockKind
    }


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
    , blockType = None
    , blockKind = Unclassified
    }


nextBlockState : BlockState -> Step BlockState BlockData
nextBlockState blockState =
    if Debug.log "line" blockState.currentLineNumber >= blockState.arrayLength || blockState.scanning == EndScan then
        Done
            { blockStart = blockState.blockStart
            , blockEnd = blockState.currentLineNumber
            , array = Array.slice blockState.blockStart blockState.currentLineNumber blockState.array
            , blockType = blockState.blockType
            }

    else
        case Array.get blockState.currentLineNumber blockState.array of
            Nothing ->
                Done
                    { blockStart = blockState.blockStart
                    , blockEnd = blockState.currentLineNumber
                    , array = Array.slice blockState.blockStart blockState.currentLineNumber blockState.array
                    , blockType = Paragraph
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
                                Loop { blockState | scanning = Debug.log "bl, ip, scan" EndScan }

                            InTightBlock ->
                                Loop { blockState | scanning = Debug.log "bl, itb, scan" EndScan }

                            InLooseBlock ->
                                Loop { blockState | currentLineNumber = Debug.log "bl, ilb, line" blockState.currentLineNumber + 1 }

                            _ ->
                                Loop { blockState | currentLineNumber = Debug.log "bl, _, line" <| blockState.currentLineNumber + 1 }

                    Text ->
                        if blockState.scanning == BeginScan then
                            Loop
                                { blockState
                                    | scanning = Debug.log "txt, bs, scan" InParagraph
                                    , currentLineNumber = blockState.currentLineNumber + 1
                                    , blockType = Paragraph
                                }

                        else if blockState.scanning == InParagraph then
                            Loop { blockState | currentLineNumber = Debug.log "txt, ip, line" blockState.currentLineNumber + 1 }

                        else if blockState.scanning == InTightBlock then
                            Loop { blockState | currentLineNumber = Debug.log "txt, itb line" blockState.currentLineNumber + 1 }

                        else if blockState.scanning == InLooseBlock then
                            Loop { blockState | currentLineNumber = Debug.log "txt, ilb, line" blockState.currentLineNumber + 1 }

                        else
                            Loop { blockState | scanning = Debug.log "txt, else, scan" EndScan }

                    BlockHeading args ->
                        if blockState.scanning == BeginScan then
                            let
                                blockKind =
                                    getBlockKind args

                                scanning =
                                    case blockKind of
                                        Loose _ ->
                                            InLooseBlock

                                        Tight _ ->
                                            InTightBlock

                                        Unclassified ->
                                            -- error instead?
                                            EndScan
                            in
                            Loop
                                { blockState
                                    | scanning = Debug.log "bh, bs, scan" scanning
                                    , blockKind = blockKind
                                    , currentLineNumber = blockState.currentLineNumber + 1
                                    , blockType = blockType (Debug.log "bt, args" args) |> Debug.log "BlockType"
                                }

                        else
                            Loop { blockState | scanning = Debug.log "bh, bs, else" EndScan }

                    BlockEnd name ->
                        Loop { blockState | scanning = Debug.log "be, scan" EndScan }


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
