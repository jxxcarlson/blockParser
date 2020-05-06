module Language.B.Block exposing
    ( get
    , gte
    , init
    , order
    , root
    )

import Array exposing (Array)
import Language.B.BlockType as B
import Language.B.Line as Line exposing (LineType(..))
import MU.Block exposing (Block(..), BlockKind(..))
import MU.Id as Id exposing (Id)
import MU.Source as Source exposing (Source)
import Util.Loop exposing (Step(..), loop)


type BlockScanState
    = BeginScan
    | InTightBlock
    | InLooseBlock
    | InParagraph
    | EndScan


type alias BlockState a =
    { currentLineNumber : Int
    , array : Array String
    , blockStart : Int
    , blockEnd : Int
    , arrayLength : Int
    , scanning : BlockScanState
    , blockType : a
    , blockKind : BlockKind
    , counter : Int
    }


get : Int -> Source -> Block B.BlockType
get blockStart_ source =
    loop (initMachine blockStart_ source) nextBlockState


root : Block B.BlockType
root =
    Block
        { blockStart = 0
        , blockEnd = 0
        , blockType = B.Root
        , source = Source.fromList []
        , id = Just (Id.init 0 0)
        }


init : Int -> Int -> Source -> Block B.BlockType
init version nodeId source =
    Block
        { id = Just (Id.init version nodeId)
        , blockType = B.None
        , source = source
        , blockStart = 0
        , blockEnd = Source.length source
        }


gte : Block B.BlockType -> Block B.BlockType -> Bool
gte a b =
    case order a b of
        GT ->
            True

        EQ ->
            True

        LT ->
            False


order : Block B.BlockType -> Block B.BlockType -> Order
order (Block data1) (Block data2) =
    B.order data1.blockType data2.blockType


initMachine : Int -> Source -> BlockState B.BlockType
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
    , blockType = B.None
    , blockKind = Unclassified
    , counter = 0
    }


nextBlockState : BlockState B.BlockType -> Step (BlockState B.BlockType) (Block B.BlockType)
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
                        , blockType = B.Paragraph
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
                                    , blockType = B.Paragraph
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
                                    B.getBlockKind args

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
                                    , blockType = B.blockType args
                                }

                        else
                            Loop { blockState | scanning = EndScan }

                    BlockEnd name ->
                        Loop { blockState | scanning = EndScan }
