module Edit.Block exposing
    ( Block
    , arrayOf
    , blockEnd
    , blockTypeOf
    , get
    , gte
    , idOf
    , init
    , order
    , root
    , setId
    , typeOf
    )

import Array exposing (Array)
import Edit.BlockType as BlockType exposing (BlockType(..))
import Edit.Id as Id exposing (Id)
import Edit.Line as Line exposing (LineType(..))
import Edit.Source as Source exposing (Source)
import Loop exposing (Step(..), loop)
import Stack exposing (Stack)
import Tree.Zipper as Zipper exposing (Zipper)


type Block
    = Block
        { id : Maybe Id
        , blockType : BlockType
        , blockStart : Int
        , blockEnd : Int
        , source : Source
        }


init : Int -> Int -> Source -> Block
init version nodeId source =
    Block
        { id = Just (Id.init version nodeId)
        , blockType = None
        , source = source
        , blockStart = 0
        , blockEnd = Source.length source
        }


arrayOf : Block -> Array String
arrayOf (Block data) =
    Source.toArray data.source


blockTypeOf : Block -> BlockType
blockTypeOf (Block data) =
    data.blockType


blockEnd : Block -> Int
blockEnd (Block data) =
    data.blockEnd


typeOf : Block -> BlockType
typeOf (Block data) =
    data.blockType


idOf : Block -> Maybe Id
idOf (Block data) =
    data.id


setId : Maybe Id -> Block -> Block
setId id (Block data) =
    Block { data | id = id }


gte : Block -> Block -> Bool
gte a b =
    case order a b of
        GT ->
            True

        EQ ->
            True

        LT ->
            False


order : Block -> Block -> Order
order (Block data1) (Block data2) =
    BlockType.order data1.blockType data2.blockType



-- BELOW: WORK IN PROGRESS


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


type alias BlockZipperState =
    { zipper : Zipper Block, stack : Stack BlockType }


type alias Position =
    { line : Int, column : Int }



-- |> updateSourceMap


get : Int -> Source -> Block
get blockStart source =
    loop (initMachine blockStart source) nextBlockState


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


rootData =
    { blockStart = 0
    , blockEnd = 0
    , blockType = Root
    , source = Source.fromList []
    , id = Just (Id.init 0 0)
    }


root =
    Block rootData


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


initMachine : Int -> Source -> BlockState
initMachine line source =
    let
        length =
            Source.length source
    in
    { currentLineNumber = line
    , array = Source.toArray source
    , blockStart = line
    , blockEnd = length
    , arrayLength = length
    , scanning = BeginScan
    , blockType = None
    , blockKind = Unclassified
    }


nextBlockState : BlockState -> Step BlockState Block
nextBlockState blockState =
    if blockState.currentLineNumber >= blockState.arrayLength || blockState.scanning == EndScan then
        Done <|
            Block
                { blockStart = blockState.blockStart
                , blockEnd = blockState.currentLineNumber
                , source = Source.fromArray <| Array.slice blockState.blockStart blockState.currentLineNumber blockState.array
                , blockType = blockState.blockType
                , id = Nothing
                }

    else
        case Array.get blockState.currentLineNumber blockState.array of
            Nothing ->
                Done <|
                    Block
                        { blockStart = blockState.blockStart
                        , blockEnd = blockState.currentLineNumber
                        , source = Source.fromArray <| Array.slice blockState.blockStart blockState.currentLineNumber blockState.array
                        , blockType = Paragraph
                        , id = Nothing
                        }

            Just line ->
                let
                    lineData =
                        Line.classify blockState.currentLineNumber line
                in
                case lineData.lineType of
                    Blank ->
                        case blockState.scanning of
                            InParagraph ->
                                Loop { blockState | scanning = EndScan }

                            InTightBlock ->
                                Loop { blockState | scanning = EndScan }

                            InLooseBlock ->
                                Loop { blockState | currentLineNumber = blockState.currentLineNumber + 1 }

                            _ ->
                                Loop { blockState | currentLineNumber = blockState.currentLineNumber + 1 }

                    Text ->
                        if blockState.scanning == BeginScan then
                            Loop
                                { blockState
                                    | scanning = InParagraph
                                    , currentLineNumber = blockState.currentLineNumber + 1
                                    , blockType = Paragraph
                                }

                        else if blockState.scanning == InParagraph then
                            Loop { blockState | currentLineNumber = blockState.currentLineNumber + 1 }

                        else if blockState.scanning == InTightBlock then
                            Loop { blockState | currentLineNumber = blockState.currentLineNumber + 1 }

                        else if blockState.scanning == InLooseBlock then
                            Loop { blockState | currentLineNumber = blockState.currentLineNumber + 1 }

                        else
                            Loop { blockState | scanning = EndScan }

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
                                    | scanning = scanning
                                    , blockKind = blockKind
                                    , currentLineNumber = blockState.currentLineNumber + 1
                                    , blockType = BlockType.blockType args
                                }

                        else
                            Loop { blockState | scanning = EndScan }

                    BlockEnd name ->
                        Loop { blockState | scanning = EndScan }
