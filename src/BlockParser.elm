module BlockParser exposing
    ( getNodeAtLine
    , initParserState
    , parse
    , parseString
    , parseStringArrayWithVersion
    , parseStringWithVersion
    , toTree
    )

{-|

    The blockParser function reads an Array of lines and produces a tree of BlockData.

-}

import Array exposing (Array)
import ArrayUtil
import Block exposing (Block, BlockType, Id)
import BlockTree
import Loop exposing (Step(..), loop)
import Stack exposing (Stack)
import Tree exposing (Tree)
import Tree.Zipper as Zipper exposing (Zipper)



-- TYPES


type alias ParserState =
    { bzs : BlockZipperState
    , source : Array String
    , sourceMap : Array (Maybe Id)
    , cursor : Int
    , arrayLength : Int
    , counter : Int
    , id : Maybe Id
    }


type alias BlockZipperState =
    { zipper : Zipper Block, stack : Stack BlockType }


type alias Position =
    { line : Int, column : Int }


parse : ParserState -> ParserState
parse parserState =
    loop parserState nextState


initParserState : Int -> Array String -> ParserState
initParserState version source =
    { source = source
    , sourceMap = Array.fromList (List.repeat (Array.length source) Nothing)
    , cursor = 0
    , bzs = initState
    , arrayLength = Array.length source
    , counter = 0
    , id = Just ( version, 0 )
    }



-- PARSER OPERATIONS


toTree : ParserState -> Tree Block
toTree state =
    state
        |> .bzs
        |> .zipper
        |> Zipper.toTree


updateSourceMap : ParserState -> ParserState
updateSourceMap parserState =
    { parserState | sourceMap = parserState |> toTree |> BlockTree.sourceMapFromTree }


getNode : Id -> ParserState -> Maybe Block
getNode id parserState =
    parserState |> toTree |> BlockTree.getNodeFromTree id


getArraySegment : Id -> ParserState -> Maybe ( Int, Int )
getArraySegment id parserState =
    parserState |> toTree |> BlockTree.getArraySegmentFromTree id


getNodeAtLine : Int -> ParserState -> Maybe Block
getNodeAtLine index parserState =
    BlockTree.getNodeAtLine (parserState |> toTree) parserState.sourceMap index


insertString : Position -> String -> ParserState -> ParserState
insertString pos str ps =
    { ps | source = ArrayUtil.insert pos str ps.source }



-- PARSER


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
                    if Block.greaterThanOrEqual newBlock.blockType btAtStackTop then
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
                                    case parserState.id of
                                        Nothing ->
                                            Nothing

                                        Just ( version, id ) ->
                                            Just ( version, id + 1 )

                                updatedBlock =
                                    { newBlock | id = newId }
                             in
                             map (ap updatedBlock) parserState
                                |> map lc
                                |> updateCursor newBlock.blockEnd
                                |> incrementCounter
                                |> updateId newId
                            )



-- PARSER OPERATIONS


updateId : Maybe Id -> ParserState -> ParserState
updateId id ps =
    { ps | id = id }


incrementCounter : ParserState -> ParserState
incrementCounter ps =
    { ps | counter = ps.counter + 1 }


push : BlockType -> BlockZipperState -> BlockZipperState
push bt bzs =
    { bzs | stack = Stack.push bt bzs.stack }


pop : BlockType -> BlockZipperState -> BlockZipperState
pop bt bzs =
    { bzs | stack = Stack.pop bzs.stack }


updateCursor : Int -> ParserState -> ParserState
updateCursor k ps =
    { ps | cursor = k }


initState : BlockZipperState
initState =
    { zipper = Zipper.fromTree (s Block.rootData), stack = Stack.init |> Stack.push Block.rootData.blockType }



-- TREE OPERATIONS


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
            { state | stack = Stack.push (Zipper.label z).blockType state.stack, zipper = z }


appendTreeToFocus : Tree a -> Zipper a -> Zipper a
appendTreeToFocus t_ z =
    let
        newTree =
            Tree.appendChild t_ (Zipper.tree z)
    in
    Zipper.replaceTree newTree z



-- CONVENIENCE PARSER FUNCTIONS


parseString : String -> Tree Block
parseString str =
    parseStringWithVersion 0 str


parseStringWithVersion : Int -> String -> Tree Block
parseStringWithVersion version str =
    let
        array =
            Block.arrayFromString str
    in
    parseStringArrayWithVersion version array


parseStringArrayWithVersion : Int -> Array String -> Tree Block
parseStringArrayWithVersion version array =
    parse (initParserState version array) |> toTree
