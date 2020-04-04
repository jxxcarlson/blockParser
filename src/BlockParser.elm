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
import Maybe.Extra
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
                    blockTreeFromArray newArray |> Debug.log "newTree"
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


updateBlock : Array String -> Block -> Block
updateBlock source block =
    parse (initParserState source)
        |> .bzs
        |> .zipper
        |> Zipper.label
        |> (\x -> { x | id = block.id })


updateSubTreeAtRoot : Array String -> Block -> ParserState -> Maybe (Tree Block)
updateSubTreeAtRoot source block ps =
    let
        newBlock =
            updateBlock source block
    in
    setFocus block.id ps.bzs.zipper
        |> Maybe.map Zipper.tree
        |> Maybe.map (Tree.replaceLabel newBlock)


replaceSubTreeAtId : Maybe Id -> Tree Block -> Zipper Block -> Zipper Block
replaceSubTreeAtId maybeId subTree zipper =
    case setFocus maybeId zipper of
        Nothing ->
            zipper

        Just refocusedZipper ->
            let
                newZipper =
                    Zipper.replaceTree subTree refocusedZipper

                fromNode =
                    Zipper.label newZipper

                toNode =
                    findValidParent fromNode.blockType (Zipper.root zipper)

                zipperAfterMove : Zipper Block
                zipperAfterMove =
                    case Maybe.map3 moveSubTree fromNode.id toNode.id (Just newZipper) |> Maybe.Extra.join of
                        Nothing ->
                            newZipper

                        Just z ->
                            z
            in
            zipperAfterMove


replaceLine : Int -> String -> ParserState -> ParserState
replaceLine line str ps =
    case getNodeAtLine line ps of
        Nothing ->
            ps

        Just block ->
            let
                newArray =
                    Array.set (line - block.blockStart) str block.array

                newSubTree : Maybe (Tree Block)
                newSubTree =
                    updateSubTreeAtRoot newArray block ps

                zipperAfterMove : Zipper Block
                zipperAfterMove =
                    case newSubTree of
                        Nothing ->
                            ps.bzs.zipper

                        Just subTree_ ->
                            replaceSubTreeAtId block.id subTree_ ps.bzs.zipper
            in
            { ps
                | source = Array.set line str ps.source
                , bzs = mapBZS (\z -> zipperAfterMove) ps.bzs
            }


mapBZS : (Zipper Block -> Zipper Block) -> BlockZipperState -> BlockZipperState
mapBZS f blockZipperState =
    let
        oldZipper =
            blockZipperState.zipper
    in
    { blockZipperState | zipper = f oldZipper }


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



-- TREE AND ZIPPER OPERATIONS


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
        refocusedZipper =
            setFocus (Just from) zipper

        subTree =
            refocusedZipper
                |> Maybe.map Zipper.tree

        prunedZipper =
            refocusedZipper
                |> Maybe.andThen Zipper.removeTree
                |> Maybe.andThen (setFocus (Just to))
    in
    Maybe.map2 appendTreeToFocus subTree prunedZipper


type alias ST =
    { blockType : BlockType, zipper : Zipper Block, count : Int }


{-| This will terminate if the root of the zipper has blockType Document,
which is greatest in the partial order

TODO: return a Maybe Block instead, with Nothing returned if the root does not satisfy the above assumption.

s

-}
findValidParent : BlockType -> Zipper Block -> Block
findValidParent blockType zipper =
    let
        ns : ST -> Step ST ST
        ns state =
            let
                _ =
                    Debug.log "(count, btGiven, btFocus)" ( state.count, blockType, (Zipper.label state.zipper).blockType )
            in
            if state.count > 3 then
                Done state

            else if Block.greaterThanOrEqual state.blockType (Zipper.label state.zipper).blockType then
                case Zipper.parent zipper of
                    Nothing ->
                        Done state

                    Just z ->
                        Loop { state | zipper = z, count = state.count + 1 }

            else
                Done state
    in
    loop { blockType = blockType, zipper = zipper, count = 0 } ns
        |> .zipper
        |> Zipper.label



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
