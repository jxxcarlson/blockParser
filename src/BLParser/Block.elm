module BLParser.Block exposing
    ( Block
    , arrayOf
    , blockEnd
    , blockStart
    , equal
    , get
    , gte
    , idOf
    , init
    , order
    , root
    , setId
    , ss
    , stringOf
    , tt
    , typeOf
    )

import Array exposing (Array)
import BLParser.Id as Id exposing (Id)
import BLParser.Line as Line exposing (LineType(..))
import BLParser.Source as Source exposing (Source)
import BlockType.LanguageC as BlockType exposing (BlockKind(..), BlockType(..))
import Loop exposing (Step(..), loop)
import Stack exposing (Stack)
import Tree.Zipper as Zipper exposing (Zipper)


ss =
    """A
    B"""


tt =
    """A
    B
    C
D"""


uu =
    """| section A

| subsection B

C

    X

        | subsection Y

| subsection D

E

F

| section G"""


type Block
    = Block
        { id : Maybe Id
        , blockType : BlockType
        , blockStart : Int
        , blockEnd : Int
        , source : Source
        }


type alias BlockState =
    { currentLineNumber : Int
    , array : Array String
    , blockStart : Int
    , blockEnd : Int
    , arrayLength : Int
    , scanning : BlockScanState
    , blockType : BlockType
    , blockKind : BlockKind
    , counter : Int
    }


type alias BlockZipperState =
    { zipper : Zipper Block, stack : Stack BlockType }


type alias Position =
    { line : Int, column : Int }


get : Int -> Source -> Block
get blockStart_ source =
    loop (initMachine blockStart_ source) nextBlockState


type BlockScanState
    = BeginScan
    | InTightBlock Int
    | InLooseBlock Int
    | InParagraph Int
    | EndScan


root : Block
root =
    Block
        { blockStart = 0
        , blockEnd = 0
        , blockType = Root
        , source = Source.fromList []
        , id = Just (Id.init 0 0)
        }


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
    , counter = 0
    }


nextBlockState : BlockState -> Step BlockState Block
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
                        , blockType = Paragraph 0
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
                                    , blockType = Paragraph level
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
                                    BlockType.getBlockKind args

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
                                    , blockType = BlockType.blockType level args
                                    , counter = blockState.counter + 1
                                }

                        else
                            Loop { blockState | scanning = EndScan, counter = blockState.counter + 1 }

                    BlockEnd name ->
                        Loop { blockState | scanning = EndScan, counter = blockState.counter + 1 }



-- HELPERS


init : Int -> Int -> Source -> Block
init version nodeId source =
    Block
        { id = Just (Id.init version nodeId)
        , blockType = None
        , source = source
        , blockStart = 0
        , blockEnd = Source.length source
        }


equal : Block -> Block -> Bool
equal a b =
    idOf a == idOf b


arrayOf : Block -> Array String
arrayOf (Block data) =
    Source.toArray data.source


stringOf : Block -> String
stringOf (Block data) =
    Source.toArray data.source
        |> Array.toList
        |> String.join "\n"


blockStart : Block -> Int
blockStart (Block data) =
    data.blockStart


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
