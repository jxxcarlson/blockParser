module Edit.Parse exposing (ParserState, parse, parseSource)

import Edit.Block as Block exposing (Block)
import Edit.BlockType as BlockType exposing (BlockType)
import Edit.Id as Id exposing (Id)
import Edit.Source as Source exposing (Source)
import Edit.SourceMap as SourceMap exposing (SourceMap)
import Loop exposing (Step(..), loop)
import Stack exposing (Stack)
import Tree as Tree exposing (Tree)
import Tree.Zipper as Zipper exposing (Zipper)


type alias ParserState =
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
    { source = source
    , sourceMap = SourceMap.empty source
    , cursor = 0
    , bzs = initState
    , arrayLength = Source.length source
    , counter = 0
    , version = 0
    , id = Just (Id.init 0 0)
    }


type alias BlockZipperState =
    { zipper : Zipper Block, stack : Stack BlockType }


type alias Position =
    { line : Int, column : Int }


parseSource : Source -> ParserState
parseSource source =
    parse (initParserState source)


initState : BlockZipperState
initState =
    { zipper = Zipper.fromTree (s Block.root), stack = Stack.init |> Stack.push (Block.typeOf Block.root) }


parse : ParserState -> ParserState
parse parserState =
    loop parserState nextState



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
    case parserState.cursor < parserState.arrayLength of
        False ->
            Done parserState

        True ->
            let
                newBlock =
                    Block.get parserState.cursor parserState.source

                --_ =
                --    Debug.log "(NB, TS, >=)" ( newBlock.blockType, Stack.top parserState.bzs.stack, Maybe.map2 Block.greaterThanOrEqual (Just newBlock.blockType) (Stack.top parserState.bzs.stack) )
            in
            case Stack.top parserState.bzs.stack of
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
                                    Maybe.map Id.incrementVersion parserState.id

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
updateCursor k ps =
    { ps | cursor = k }


updateId : Maybe Id -> ParserState -> ParserState
updateId id ps =
    { ps | id = id }


incrementCounter : ParserState -> ParserState
incrementCounter ps =
    { ps | counter = ps.counter + 1 }



--- XXX


s =
    Tree.singleton


at =
    appendTreeToFocus


ap : Block -> BlockZipperState -> BlockZipperState
ap b state =
    { state | zipper = at (s b) state.zipper }


map : (BlockZipperState -> BlockZipperState) -> ParserState -> ParserState
map f parserState =
    let
        oldBzs =
            parserState.bzs
    in
    { parserState | bzs = f oldBzs }


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
