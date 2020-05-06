module Language.C.Block exposing
    ( get
    , gte
    , root
    )

import Array exposing (Array)
import BLParser.Block exposing (Block(..), BlockKind(..), BlockScanState(..), BlockState)
import BLParser.Id as Id exposing (Id)
import BLParser.Source as Source exposing (Source)
import Language.C.BlockType as C
import Language.C.Line as Line exposing (LineType(..))
import Loop exposing (Step(..), loop)


get : Int -> Source -> Block C.BlockType
get blockStart_ source =
    loop (initMachine blockStart_ source) nextBlockState


root : Block C.BlockType
root =
    Block
        { blockStart = 0
        , blockEnd = 0
        , blockType = C.Root
        , source = Source.fromList []
        , id = Just (Id.init 0 0)
        }


initMachine : Int -> Source -> BlockState C.BlockType
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
    , blockType = C.None
    , blockKind = Unclassified
    , counter = 0
    }


nextBlockState : BlockState C.BlockType -> Step (BlockState C.BlockType) (Block C.BlockType)
nextBlockState blockState =
    --let
    --    --_ =
    --    --    Debug.log "N" blockState.counter
    --    --
    --    --_ =
    --    --    Debug.log "(currentLineNumber, scanning, blockType)"
    --    --        ( blockState.currentLineNumber, blockState.scanning, blockState.blockType )
    --in
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
                        , blockType = C.Paragraph 0
                        , id = Nothing
                        }

            Just line ->
                let
                    --_ =
                    --    Debug.log "LINE" ( blockState.currentLineNumber, line )
                    --_ =
                    --    Debug.log "(currentLineNumber, scanning, blockType)"
                    --        ( blockState.currentLineNumber, blockState.scanning, blockState.blockType )
                    lineData =
                        Line.classify blockState.currentLineNumber line

                    --_ =
                    --    Debug.log "LINE DATA" lineData.lineType
                in
                case lineData.lineType of
                    Blank ->
                        case blockState.scanning of
                            InParagraph level ->
                                Loop { blockState | currentLineNumber = blockState.currentLineNumber + 1, counter = blockState.counter + 1 }

                            InTightBlock level ->
                                Loop { blockState | currentLineNumber = blockState.currentLineNumber + 1, counter = blockState.counter + 1 }

                            InLooseBlock level ->
                                Loop { blockState | currentLineNumber = blockState.currentLineNumber + 1, counter = blockState.counter + 1 }

                            _ ->
                                Loop { blockState | currentLineNumber = blockState.currentLineNumber + 1, counter = blockState.counter + 1 }

                    Text level ->
                        if blockState.scanning == BeginScan then
                            -- make new block
                            let
                                k =
                                    blockState.currentLineNumber

                                --_ =
                                --    Debug.log "NEW BLOCK (line, blockType, str)"
                                --        ( k, Paragraph level, Array.get k blockState.array )
                                --( k, Paragraph level, blockState.array )
                            in
                            Loop
                                { blockState
                                    | scanning = InParagraph level
                                    , currentLineNumber = blockState.currentLineNumber + 1
                                    , blockType = C.Paragraph level
                                    , counter = blockState.counter + 1
                                }

                        else if blockState.scanning == InParagraph level then
                            --let
                            --    _ =
                            --        Debug.log "(Text level, scanning)" ( level, blockState.scanning )
                            --in
                            Loop
                                { blockState
                                    | currentLineNumber = blockState.currentLineNumber + 1
                                    , counter = blockState.counter + 1
                                }

                        else if blockState.scanning == InTightBlock level then
                            Loop
                                { blockState
                                    | scanning = BeginScan
                                    , counter = blockState.counter + 1
                                }

                        else if blockState.scanning == InLooseBlock level then
                            Loop { blockState | scanning = BeginScan, counter = blockState.counter + 1 }

                        else
                            --let
                            --    _ =
                            --        Debug.log "BeginScan at" blockState.counter
                            --in
                            Loop { blockState | scanning = EndScan, counter = blockState.counter + 1 }

                    BlockHeading level args ->
                        if blockState.scanning == BeginScan then
                            let
                                blockKind =
                                    C.getBlockKind args

                                scanning =
                                    case blockKind of
                                        Loose _ ->
                                            InLooseBlock level

                                        Tight _ ->
                                            InTightBlock level

                                        Unclassified ->
                                            -- error instead?
                                            EndScan
                            in
                            Loop
                                { blockState
                                    | scanning = scanning
                                    , blockKind = blockKind
                                    , currentLineNumber = blockState.currentLineNumber + 1
                                    , blockType = C.blockType level args
                                    , counter = blockState.counter + 1
                                }

                        else
                            Loop { blockState | scanning = EndScan, counter = blockState.counter + 1 }

                    BlockEnd name ->
                        Loop { blockState | scanning = EndScan, counter = blockState.counter + 1 }



-- HELPERS


init : Int -> Int -> Source -> Block C.BlockType
init version nodeId source =
    Block
        { id = Just (Id.init version nodeId)
        , blockType = C.None
        , source = source
        , blockStart = 0
        , blockEnd = Source.length source
        }


gte : Block C.BlockType -> Block C.BlockType -> Bool
gte a b =
    case order a b of
        GT ->
            True

        EQ ->
            True

        LT ->
            False


order : Block C.BlockType -> Block C.BlockType -> Order
order (Block data1) (Block data2) =
    C.order data1.blockType data2.blockType
