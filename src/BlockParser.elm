module BlockParser exposing
    ( blockTreeFromArray
    , blockTreeFromString
    , getNodeAtLine
    , initParserState
    , moveSubTree
    , parse
    , parseArray
    , parseString
    , replaceLine
    , setFocus
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
import String.Extra
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
    , version : Int
    , id : Maybe Id
    }


type alias BlockZipperState =
    { zipper : Zipper Block, stack : Stack BlockType }


type alias Position =
    { line : Int, column : Int }


parse : ParserState -> ParserState
parse parserState =
    loop parserState nextState |> updateSourceMap


parseArray : Array String -> ParserState
parseArray source =
    parse (initParserState source)


parseString : String -> ParserState
parseString source =
    parseArray (Block.arrayFromString source)


initParserState : Array String -> ParserState
initParserState source =
    { source = source
    , sourceMap = Array.fromList (List.repeat (Array.length source) Nothing)
    , cursor = 0
    , bzs = initState
    , arrayLength = Array.length source
    , counter = 0
    , version = 0
    , id = Just ( 0, 0 )
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
    case getNodeAtLine pos.line ps of
        Nothing ->
            ps

        Just block ->
            let
                offset =
                    pos.line - block.blockStart

                newArray =
                    ArrayUtil.insert (Position offset pos.column) str block.array

                newTree =
                    blockTreeFromArray newArray
            in
            case setFocus block.id ps.bzs.zipper of
                Nothing ->
                    ps

                Just refocusedZipper ->
                    let
                        newZipper =
                            Zipper.replaceTree newTree refocusedZipper

                        oldBzs =
                            ps.bzs

                        newBzs =
                            { oldBzs | zipper = newZipper }
                    in
                    { ps | source = ArrayUtil.insert pos str ps.source, bzs = newBzs }


replaceLine : Int -> String -> ParserState -> ParserState
replaceLine line str ps =
    case getNodeAtLine line ps of
        Nothing ->
            let
                _ =
                    Debug.log "Node not found" line
            in
            ps

        Just block ->
            let
                offset =
                    Debug.log "offset"
                        (line - block.blockStart)

                newArray =
                    Debug.log "newArray" <|
                        Array.set offset str (Debug.log "old Array" block.array)

                newLabel : Block
                newLabel =
                    Debug.log "newLabel" <|
                        (parse (initParserState newArray)
                            |> .bzs
                            |> .zipper
                            |> Zipper.label
                        )

                newSubTree : Maybe (Tree Block)
                newSubTree =
                    setFocus block.id ps.bzs.zipper
                        |> Maybe.map Zipper.tree
                        |> Maybe.map (Tree.replaceLabel newLabel)
                        |> Debug.log "newSubTree"
            in
            case ( setFocus block.id ps.bzs.zipper, newSubTree ) of
                ( Nothing, _ ) ->
                    ps

                ( _, Nothing ) ->
                    ps

                ( Just refocusedZipper, Just newSubTree_ ) ->
                    let
                        newZipper =
                            Zipper.replaceTree newSubTree_ refocusedZipper

                        oldBzs =
                            ps.bzs

                        newBzs =
                            { oldBzs | zipper = newZipper }
                    in
                    { ps | source = Array.set line str ps.source, bzs = newBzs }


{-| Set the focus of the zipper to the subtree
whose root has the given id
-}
setFocus : Maybe Id -> Zipper Block -> Maybe (Zipper Block)
setFocus id zipper =
    Zipper.findFromRoot (\label -> label.id == id) zipper



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


moveSubTree : Id -> Id -> Zipper Block -> Maybe (Zipper Block)
moveSubTree from to zipper =
    let
        refocusedZipper : Maybe (Zipper Block)
        refocusedZipper =
            setFocus (Just from) zipper

        subTree : Maybe (Tree Block)
        subTree =
            refocusedZipper
                |> Maybe.map Zipper.tree
                |> Debug.log "subTree"

        prunedZipper : Maybe (Zipper Block)
        prunedZipper =
            refocusedZipper
                |> Maybe.andThen Zipper.removeTree
                |> Maybe.andThen (setFocus (Just to))
                |> Debug.log "prunedZipper"

        _ =
            prunedZipper |> Maybe.map Zipper.toTree |> Debug.log "prunedTree"
    in
    Maybe.map2 appendTreeToFocus subTree prunedZipper



-- CONVENIENCE PARSER FUNCTIONS


blockTreeFromString : String -> Tree Block
blockTreeFromString str =
    let
        array =
            Block.arrayFromString str
    in
    blockTreeFromArray array


blockTreeFromArray : Array String -> Tree Block
blockTreeFromArray array =
    parse (initParserState array) |> toTree
