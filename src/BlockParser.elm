module BlockParser exposing
    ( isParserBijective
    , parse
    , toAnnotatedStringTree
    , toString
    , toStringTree
    , toTaggedStringTree
    )

{-|

    The blockParser function reads an Array of lines and produces a tree of BlockData.

-}

import Array exposing (Array)
import Block exposing (Block, BlockData, BlockType)
import HTree
import Loop exposing (Step(..), loop)
import Stack exposing (Stack)
import Tree exposing (Tree)
import Tree.Zipper as Zipper exposing (Zipper)


type alias ParserState =
    { bzs : BlockZipperState, array : Array String, cursor : Int, arrayLength : Int, counter : Int }


type alias BlockZipperState =
    { zipper : Zipper BlockData, stack : Stack BlockType }


parse : String -> Tree BlockData
parse str =
    loop (initParserState str) nextState


initParserState : String -> ParserState
initParserState str =
    let
        array =
            Block.arrayFromString str
    in
    { array = array
    , cursor = 0
    , bzs = initState
    , arrayLength = Array.length array
    , counter = 0
    }


nextState : ParserState -> Step ParserState (Tree BlockData)
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
            Done (parserState.bzs.zipper |> Zipper.toTree)

        True ->
            let
                newBlock =
                    Block.get parserState.cursor parserState.array

                --_ =
                --    Debug.log "(NB, TS, >=)" ( newBlock.blockType, Stack.top parserState.bzs.stack, Maybe.map2 gte (Just newBlock.blockType) (Stack.top parserState.bzs.stack) )
            in
            case Stack.top parserState.bzs.stack of
                Nothing ->
                    let
                        _ =
                            Debug.log "branch" Nothing
                    in
                    Done (parserState.bzs.zipper |> Zipper.toTree)

                Just btAtStackTop ->
                    --let
                    --    _ =
                    --        Debug.log "(NB, TS)" ( newBlock.blockType, btAtStackTop )
                    --in
                    if gte newBlock.blockType btAtStackTop then
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
                            (map (ap newBlock) parserState
                                |> map lc
                                |> updateCursor newBlock.blockEnd
                                |> incrementCounter
                            )


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


gte a b =
    not (Block.lessThan a b)


gt a b =
    not (Block.lessThanOrEqual a b)


s =
    Tree.singleton


at =
    appendTreeToFocus


initState : BlockZipperState
initState =
    { zipper = Zipper.fromTree (s Block.rootData), stack = Stack.init |> Stack.push Block.rootData.blockType }


ap : BlockData -> BlockZipperState -> BlockZipperState
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



-- TESTS


isParserBijective : String -> ( Bool, Bool )
isParserBijective str =
    let
        str2 =
            parse str |> toString

        compress =
            String.replace "\n" ""
    in
    ( str2 == str, compress str2 == compress str )



-- CONVERSIONS


toString : Tree BlockData -> String
toString tree =
    Tree.foldl (\str acc -> acc ++ str) "" (toStringTree tree)


toStringTree : Tree BlockData -> Tree String
toStringTree tree =
    let
        mapper : BlockData -> String
        mapper bd =
            bd.array |> Array.toList |> String.join "\n"
    in
    Tree.map mapper tree


toTaggedStringTree : Tree BlockData -> Tree ( String, Int )
toTaggedStringTree tree =
    tree
        |> toStringTree
        |> HTree.tagWithDepth


toAnnotatedStringTree : Tree BlockData -> Tree ( String, String )
toAnnotatedStringTree tree =
    let
        mapper : BlockData -> ( String, String )
        mapper bd =
            ( Debug.toString bd.blockType, bd.array |> Array.toList |> String.join "\n" )
    in
    Tree.map mapper tree
