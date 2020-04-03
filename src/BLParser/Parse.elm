module BLParser.Parse exposing
    ( ParserState
    , deleteRangeInSource
    , getSource
    , getSourceMap
    , parse
    , parseSource
    , replaceSource
    , toTree
    )

import BLParser.Block as Block exposing (Block)
import BLParser.BlockType as BlockType exposing (BlockType)
import BLParser.Id as Id exposing (Id)
import BLParser.Source as Source exposing (Source)
import BLParser.SourceMap as SourceMap exposing (SourceMap)
import Loop exposing (Step(..), loop)
import Stack exposing (Stack)
import Tree as Tree exposing (Tree)
import Tree.Zipper as Zipper exposing (Zipper)


type ParserState
    = ParserState
        { source : Source
        , sourceMap : SourceMap
        , cursor : Int
        , bzs : BlockZipperState
        , arrayLength : Int
        , counter : Int
        , version : Int
        , id : Maybe Id
        }


initParserState : Source -> ParserState
initParserState source =
    ParserState
        { source = source
        , sourceMap = SourceMap.empty source
        , cursor = 0
        , bzs = initState
        , arrayLength = Source.length source
        , counter = 0
        , version = 0
        , id = Just (Id.init 0 0)
        }


parseSource : Source -> ParserState
parseSource source =
    parse (initParserState source)


initState : BlockZipperState
initState =
    { zipper = Zipper.fromTree (s Block.root), stack = Stack.init |> Stack.push (Block.typeOf Block.root) }


parse : ParserState -> ParserState
parse parserState =
    loop parserState nextState
        |> updateSourceMap


toTree : ParserState -> Tree Block
toTree state =
    getBZS state
        |> .zipper
        |> Zipper.toTree


sourceLength : ParserState -> Int
sourceLength (ParserState data) =
    data.arrayLength



-- GETTERS


getBZS : ParserState -> BlockZipperState
getBZS (ParserState data) =
    data.bzs


getZipper : ParserState -> Zipper Block
getZipper (ParserState data) =
    data.bzs.zipper


getStack : ParserState -> Stack BlockType
getStack (ParserState data) =
    data.bzs.stack


getId : ParserState -> Maybe Id
getId (ParserState data) =
    data.id


getCounter : ParserState -> Int
getCounter (ParserState data) =
    data.counter


getSource : ParserState -> Source
getSource (ParserState data) =
    data.source


getCursor : ParserState -> Int
getCursor (ParserState data) =
    data.cursor


getSourceMap : ParserState -> SourceMap
getSourceMap (ParserState data) =
    data.sourceMap



-- SETTERS, MODIFIERS


replaceSource : Source -> ParserState -> ParserState
replaceSource newSource (ParserState data) =
    ParserState { data | source = newSource }


replaceRangeInSource : Int -> Int -> Source -> ParserState -> ParserState
replaceRangeInSource from to source (ParserState data) =
    ParserState { data | source = Source.replaceRange from to source data.source }


insertBeforeIndex : Int -> Source -> ParserState -> ParserState
insertBeforeIndex k source (ParserState data) =
    ParserState { data | source = Source.insertBeforeIndex k source data.source }


deleteRangeInSource : Int -> Int -> ParserState -> ParserState
deleteRangeInSource from to parserState =
    replaceSource (Source.deleteRange from to (getSource parserState)) parserState


setSourceMap : SourceMap -> ParserState -> ParserState
setSourceMap sourceMap (ParserState data) =
    ParserState { data | sourceMap = sourceMap }


type alias BlockZipperState =
    { zipper : Zipper Block, stack : Stack BlockType }


type alias Position =
    { line : Int, column : Int }


updateSourceMap : ParserState -> ParserState
updateSourceMap parserState =
    let
        newSourceMap =
            parserState
                |> toTree
                |> SourceMap.fromTree
    in
    setSourceMap newSourceMap parserState



-- |> updateSourceMap


nextState : ParserState -> Step ParserState ParserState
nextState parserState =
    --let
    --    _ =
    --        Debug.log "n" (getCounter parserState)
    --
    --    _ =
    --        Debug.log "(STACK, FOCUS, TREE)"
    --            ( getStack parserState
    --            , Block.blockTypeOf (Zipper.label (getZipper parserState))
    --            , getZipper parserState |> Zipper.toTree |> BlockTree.toStringTree
    --            )
    --in
    case getCursor parserState < sourceLength parserState of
        False ->
            Done parserState

        True ->
            let
                newBlock =
                    Block.get (getCursor parserState) (getSource parserState)

                --_ =
                --    Debug.log "(NB, TS, >=)"
                --        ( Block.blockTypeOf newBlock
                --        , Stack.top (getStack parserState)
                --        , Maybe.map2 BlockType.gte (Just (Block.blockTypeOf newBlock)) (Stack.top (getStack parserState))
                --        )
            in
            case Stack.top (getStack parserState) of
                Nothing ->
                    --let
                    --    _ =
                    --        Debug.log "branch" Nothing
                    --in
                    Done parserState

                Just btAtStackTop ->
                    --let
                    --    _ =
                    --        Debug.log "(NB, TS)" ( Block.blockTypeOf newBlock, btAtStackTop )
                    --in
                    if BlockType.gte (Block.typeOf newBlock) btAtStackTop then
                        --let
                        --    _ =
                        --        Debug.log "action" "Pop"
                        --in
                        Loop (map par parserState |> incrementCounter)

                    else
                        --let
                        --    _ =
                        --        Debug.log "action" "Push"
                        --in
                        Loop
                            (let
                                newId =
                                    Maybe.map Id.incrementNodeId (getId parserState)

                                updatedBlock =
                                    Block.setId newId newBlock
                             in
                             map (ap updatedBlock) parserState
                                |> map lc
                                |> updateCursor (Block.blockEnd newBlock)
                                |> incrementCounter
                                |> updateId newId
                            )



-- XXXX


updateCursor : Int -> ParserState -> ParserState
updateCursor k (ParserState ps) =
    ParserState { ps | cursor = k }


updateId : Maybe Id -> ParserState -> ParserState
updateId id (ParserState ps) =
    ParserState { ps | id = id }


incrementCounter : ParserState -> ParserState
incrementCounter (ParserState ps) =
    ParserState { ps | counter = ps.counter + 1 }



--- XXX


s =
    Tree.singleton


at =
    appendTreeToFocus


ap : Block -> BlockZipperState -> BlockZipperState
ap b state =
    { state | zipper = at (s b) state.zipper }


map : (BlockZipperState -> BlockZipperState) -> ParserState -> ParserState
map f (ParserState ps) =
    let
        oldBzs =
            ps.bzs
    in
    ParserState { ps | bzs = f oldBzs }


par : BlockZipperState -> BlockZipperState
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


lc : BlockZipperState -> BlockZipperState
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
