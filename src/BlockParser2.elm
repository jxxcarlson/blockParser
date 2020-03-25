module BlockParser2 exposing
    ( BlockZipperState
    , ap
    , appendTreeToFocus
    , initParserState
    , initState
    , isParserBijective
    , lab
    , lc
    , nextState
    , par
    , parse
    , toAnnotatedStringTree
    , toString
    , toStringTree
    , tt
    )

{-|

    The blockParser function reads an Array of lines and produces a tree of Blocks,

-}

import Array exposing (Array)
import Block exposing (Block, BlockData, BlockType)
import Loop exposing (Step(..), loop)
import Parser.Advanced
import Stack exposing (Stack)
import Tree exposing (Tree)
import Tree.Zipper as Zipper exposing (Zipper)


type alias ParserState =
    { bzs : BlockZipperState, array : Array String, cursor : Int, scanning : ScanType, arrayLength : Int }


type ScanType
    = Scanning
    | EndScan


parse : String -> Tree BlockData
parse str =
    loop (initParserState str) nextState


toStringTree : Tree BlockData -> Tree String
toStringTree tree =
    let
        mapper : BlockData -> String
        mapper bd =
            bd.array |> Array.toList |> String.join "\n"
    in
    Tree.map mapper tree


isParserBijective : String -> ( Bool, Bool )
isParserBijective str =
    let
        str2 =
            parse str |> toString

        compress =
            String.replace "\n" ""
    in
    ( str2 == str, compress str2 == compress str )


toString : Tree BlockData -> String
toString tree =
    Tree.foldl (\str acc -> acc ++ str) "" (toStringTree tree)


toAnnotatedStringTree : Tree BlockData -> Tree ( String, String )
toAnnotatedStringTree tree =
    let
        mapper : BlockData -> ( String, String )
        mapper bd =
            ( Debug.toString bd.blockType, bd.array |> Array.toList |> String.join "\n" )
    in
    Tree.map mapper tree


initParserState : String -> ParserState
initParserState str =
    let
        array =
            Block.arrayFromString str
    in
    { array = array
    , cursor = 0
    , bzs = initState
    , scanning = Scanning
    , arrayLength = Array.length array
    }


nextState : ParserState -> Step ParserState (Tree BlockData)
nextState parserState =
    --let
    --    _ =
    --        Debug.log "(STACK, FOCUS, TREE)"
    --            ( parserState.bzs.stack
    --            , (Zipper.label parserState.bzs.zipper).blockType
    --            , parserState.bzs.zipper |> Zipper.toTree |> toStringTree
    --            )
    --in
    case parserState.cursor < parserState.arrayLength && parserState.scanning == Scanning of
        False ->
            Done (parserState.bzs.zipper |> Zipper.toTree)

        True ->
            let
                newBlock =
                    Block.getBlock parserState.cursor parserState.array

                --_ =
                --    Debug.log "(NB, TS)" ( newBlock.blockType, Stack.top parserState.bzs.stack )
            in
            case Stack.top parserState.bzs.stack of
                Nothing ->
                    --let
                    --    _ =
                    --        Debug.log "branch" Nothing
                    --in
                    Done (parserState.bzs.zipper |> Zipper.toTree)

                Just btAtStackTop ->
                    --let
                    --    _ =
                    --        Debug.log "(NB, TS)" ( newBlock.blockType, btAtStackTop )
                    --in
                    if gte newBlock.blockType btAtStackTop then
                        --let
                        --    _ =
                        --        Debug.log "action" "Parent"
                        --in
                        Loop (map par parserState)

                    else
                        --let
                        --    _ =
                        --        Debug.log "action" "Append, push"
                        --in
                        Loop
                            (map (ap newBlock) parserState
                                |> map lc
                                |> updateCursor newBlock.blockEnd
                            )


push : BlockType -> BlockZipperState -> BlockZipperState
push bt bzs =
    { bzs | stack = Stack.push bt bzs.stack }


popped : BlockType -> BlockZipperState -> BlockZipperState
popped bt bzs =
    { bzs | stack = Stack.popped bzs.stack }


updateCursor : Int -> ParserState -> ParserState
updateCursor k ps =
    { ps | cursor = k }


gte a b =
    not (Block.lessThan a b)


type alias BlockZipperState =
    { zipper : Zipper BlockData, stack : Stack BlockType }


s =
    Tree.singleton


t =
    Tree.tree


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
                    state.stack |> Stack.popped
            in
            { state | stack = newStack, zipper = z }


lab : BlockZipperState -> BlockData
lab state =
    Zipper.label state.zipper


lc : BlockZipperState -> BlockZipperState
lc state =
    case Zipper.lastChild state.zipper of
        Nothing ->
            state

        Just z ->
            { state | stack = Stack.push (Zipper.label z).blockType state.stack, zipper = z }


tt : BlockZipperState -> Tree BlockData
tt state =
    state.zipper |> Zipper.toTree



--
--addTreeToFocus : Tree a -> Zipper a -> Zipper a
--addTreeToFocus t z =
--    let
--        newTree =
--            Tree.appendChild t (Tree.singleton (Zipper.label z))
--    in
--    Zipper.replaceTree newTree z


appendTreeToFocus : Tree a -> Zipper a -> Zipper a
appendTreeToFocus t_ z =
    let
        newTree =
            Tree.appendChild t_ (Zipper.tree z)
    in
    Zipper.replaceTree newTree z


type DocTree
    = Tree Block



{-

   Blocks can be either *tight* or *loose*.  A tight
   block is terminated by a blank line.  A loose
   block is terminated by EOF or a line defining
   a block of the same kind.  For example, the block that
   begins with

       | section 1 Intro

   can be terminated by

       | section 2 Atoms

   There is one other way of ending a loose block, illustrated
   by this example:

       | quote (Abraham Lincoln)

       ---
       ---

       .quote
-}
