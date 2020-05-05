module BLParser.Parse exposing
    ( ParserState
    , deleteRangeInSource
    , getId
    , getSource
    , getSourceMap
    , getZipper
    , parse
    , parseSource
    , parseString
    , setBzs
    , setSource
    , toTree
    )

-- import BLParser.Block as Block exposing (Block)

import BLParser.Id as Id exposing (Id)
import BLParser.Source as Source exposing (Source)
import BLParser.SourceMap as SourceMap exposing (SourceMap)
import Language.Block as Block exposing (Block(..))
import Language.C.Block as C
import Language.C.BlockType as BlockType exposing (BlockType)
import Loop exposing (Step(..), loop)
import Stack exposing (Stack)
import Tree as Tree exposing (Tree)
import Tree.Zipper as Zipper exposing (Zipper)


type ParserState a
    = ParserState
        { source : Source
        , sourceMap : SourceMap
        , cursor : Int
        , bzs : BlockZipperState a
        , arrayLength : Int
        , counter : Int
        , version : Int
        , id : Maybe Id
        }


initParserState : Id -> Source -> ParserState a
initParserState id source =
    ParserState
        { source = source
        , sourceMap = SourceMap.empty source
        , cursor = 0
        , bzs = initState
        , arrayLength = Source.length source
        , counter = Id.nodeId id
        , version = Id.version id
        , id = Just id
        }


parseSource : Id -> Source -> ParserState a
parseSource id source =
    parse (initParserState id source)


parseString : Id -> String -> ParserState a
parseString id str =
    parseSource id (Source.fromString str)


initState : BlockZipperState a
initState =
    { zipper = Zipper.fromTree (s C.root), stack = Stack.init |> Stack.push (Block.typeOf C.root) }


parse : ParserState a -> ParserState a
parse parserState =
    loop parserState nextState
        |> updateSourceMap


toTree : ParserState a -> Tree (Block a)
toTree state =
    getBZS state
        |> .zipper
        |> Zipper.toTree


sourceLength : ParserState a -> Int
sourceLength (ParserState data) =
    data.arrayLength


updateSourceMap : ParserState a -> ParserState a
updateSourceMap parserState =
    let
        newSourceMap =
            parserState
                |> toTree
                |> SourceMap.fromTree
    in
    setSourceMap newSourceMap parserState



-- |> updateSourceMap


nextState : ParserState a -> Step (ParserState a) (ParserState a)
nextState parserState =
    --let
    --    _ =
    --        Debug.log "n" (getCounter parserState)
    --
    --    _ =
    --        Debug.log "STACK" <|
    --            getStack parserState
    --
    --    _ =
    --        Debug.log "TREE" (getZipper parserState |> Zipper.toTree |> Tree.map (\b -> ( Block.stringOf b, Block.idOf b )))
    --in
    case getCursor parserState < sourceLength parserState of
        False ->
            Done parserState

        True ->
            let
                newBlock =
                    Block.get (getCursor parserState) (getSource parserState)
            in
            case Stack.top (getStack parserState) of
                Nothing ->
                    Done parserState

                Just btAtStackTop ->
                    --let
                    --    _ =
                    --        Debug.log "(NB, TS)" ( Block.typeOf newBlock, btAtStackTop )
                    --in
                    if BlockType.gte (Block.typeOf newBlock) btAtStackTop then
                        --let
                        --    _ =
                        --        Debug.log "Pop" btAtStackTop
                        --in
                        Loop (map par parserState |> incrementCounter)

                    else
                        Loop
                            (let
                                newId =
                                    Maybe.map Id.incrementNodeId (getId parserState)

                                updatedBlock =
                                    Block.setId newId newBlock

                                --_ =
                                --    Debug.log "Push" updatedBlock
                             in
                             map (ap updatedBlock) parserState
                                |> map lc
                                |> updateCursor (Block.blockEnd newBlock)
                                |> incrementCounter
                                |> updateId newId
                            )



-- GETTERS


getBZS : ParserState a -> BlockZipperState a
getBZS (ParserState data) =
    data.bzs


getZipper : ParserState a -> Zipper (Block a)
getZipper (ParserState data) =
    data.bzs.zipper


getStack : ParserState a -> Stack BlockType
getStack (ParserState data) =
    data.bzs.stack


getId : ParserState a -> Maybe Id
getId (ParserState data) =
    data.id


getCounter : ParserState a -> Int
getCounter (ParserState data) =
    data.counter


getSource : ParserState a -> Source
getSource (ParserState data) =
    data.source


getCursor : ParserState a -> Int
getCursor (ParserState data) =
    data.cursor


getSourceMap : ParserState a -> SourceMap
getSourceMap (ParserState data) =
    data.sourceMap



-- SETTERS, MODIFIERS


setSource : Source -> ParserState a -> ParserState a
setSource newSource (ParserState data) =
    ParserState { data | source = newSource }


setBzs : Tree (Block a) -> ParserState a -> ParserState a
setBzs ast (ParserState data) =
    let
        newBzs =
            { zipper = Zipper.fromTree ast, stack = Stack.init |> Stack.push (Block.typeOf Block.root) }
    in
    ParserState { data | bzs = newBzs }


replaceRangeInSource : Int -> Int -> Source -> ParserState a -> ParserState a
replaceRangeInSource from to source (ParserState data) =
    ParserState { data | source = Source.replaceRange from to source data.source }


insertBeforeIndex : Int -> Source -> ParserState a -> ParserState a
insertBeforeIndex k source (ParserState data) =
    ParserState { data | source = Source.insertBeforeIndex k source data.source }


deleteRangeInSource : Int -> Int -> ParserState a -> ParserState a
deleteRangeInSource from to parserState =
    setSource (Source.deleteRange from to (getSource parserState)) parserState


setSourceMap : SourceMap -> ParserState a -> ParserState a
setSourceMap sourceMap (ParserState data) =
    ParserState { data | sourceMap = sourceMap }


type alias BlockZipperState a =
    { zipper : Zipper (Block a), stack : Stack BlockType }


type alias Position =
    { line : Int, column : Int }



-- XXXX


updateCursor : Int -> ParserState a -> ParserState a
updateCursor k (ParserState ps) =
    ParserState { ps | cursor = k }


updateId : Maybe Id -> ParserState a -> ParserState a
updateId id (ParserState ps) =
    ParserState { ps | id = id }


incrementCounter : ParserState a -> ParserState a
incrementCounter (ParserState ps) =
    ParserState { ps | counter = ps.counter + 1 }



--- XXX


s =
    Tree.singleton


at =
    appendTreeToFocus


ap : Block a -> BlockZipperState a -> BlockZipperState a
ap b state =
    { state | zipper = at (s b) state.zipper }


map : (BlockZipperState a -> BlockZipperState a) -> ParserState a -> ParserState a
map f (ParserState ps) =
    let
        oldBzs =
            ps.bzs
    in
    ParserState { ps | bzs = f oldBzs }


par : BlockZipperState a -> BlockZipperState a
par state =
    case Zipper.parent state.zipper of
        Nothing ->
            state

        Just z ->
            let
                newStack =
                    state.stack |> Stack.pop
            in
            { state | stack = newStack, zipper = z }


lc : BlockZipperState a -> BlockZipperState a
lc state =
    case Zipper.lastChild state.zipper of
        Nothing ->
            state

        Just z ->
            { state | stack = Stack.push (Block.typeOf (Zipper.label z)) state.stack, zipper = z }


appendTreeToFocus : Tree a -> Zipper a -> Zipper a
appendTreeToFocus t_ z =
    let
        newTree =
            Tree.appendChild t_ (Zipper.tree z)
    in
    Zipper.replaceTree newTree z
