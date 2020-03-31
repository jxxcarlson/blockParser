module Edit.Parse exposing
    ( ParserState
    , getSourceMap
    , parse
    , parseSource
    , toTree
    )

import Edit.Block as Block exposing (Block)
import Edit.BlockType as BlockType exposing (BlockType)
import Edit.Id as Id exposing (Id)
import Edit.Source as Source exposing (Source)
import Edit.SourceMap as SourceMap exposing (SourceMap)
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


getBZS : ParserState -> BlockZipperState
getBZS (ParserState data) =
    data.bzs


getStack : ParserState -> Stack BlockType
getStack (ParserState data) =
    data.bzs.stack


getId : ParserState -> Maybe Id
getId (ParserState data) =
    data.id


getSource : ParserState -> Source
getSource (ParserState data) =
    data.source


getCursor : ParserState -> Int
getCursor (ParserState data) =
    data.cursor


setSourceMap : SourceMap -> ParserState -> ParserState
setSourceMap sourceMap (ParserState data) =
    ParserState { data | sourceMap = sourceMap }


getSourceMap : ParserState -> SourceMap
getSourceMap (ParserState data) =
    data.sourceMap


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
    --        Debug.log "n" parserState.counter
    --
    --    _ =
    --        Debug.log "(STACK, FOCUS, TREE)"
    --            ( parserState.bzs.stack
    --            , (Zipper.label parserState.bzs.zipper).blockType
    --            , parserState.bzs.zipper |> Zipper.toTree |> toStringTree
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
                --    Debug.log "(NB, TS, >=)" ( newBlock.blockType, Stack.top parserState.bzs.stack, Maybe.map2 Block.greaterThanOrEqual (Just newBlock.blockType) (Stack.top parserState.bzs.stack) )
            in
            case Stack.top (getStack parserState) of
                Nothing ->
                    let
                        _ =
                            Debug.log "branch" Nothing
                    in
                    Done parserState

                Just btAtStackTop ->
                    --let
                    --    _ =
                    --        Debug.log "(NB, TS)" ( newBlock.blockType, btAtStackTop )
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
            { state | stack = Stack.push (Block.blockTypeOf (Zipper.label z)) state.stack, zipper = z }


appendTreeToFocus : Tree a -> Zipper a -> Zipper a
appendTreeToFocus t_ z =
    let
        newTree =
            Tree.appendChild t_ (Zipper.tree z)
    in
    Zipper.replaceTree newTree z
