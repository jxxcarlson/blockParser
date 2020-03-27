module BlockParser exposing
    ( isInjective
    , parseString
    , parseStringArray
    , toBlockTypeTree
    , toString
    , toStringArray
    , toStringTree
    , toTaggedStringTree
    )

{-|

    The blockParser function reads an Array of lines and produces a tree of BlockData.

-}

import Array exposing (Array)
import Block exposing (Block, BlockData, BlockType, Id)
import Diff
import HTree
import Loop exposing (Step(..), loop)
import Stack exposing (Stack)
import Tree exposing (Tree)
import Tree.Zipper as Zipper exposing (Zipper)


parseString : Int -> String -> Tree BlockData
parseString version str =
    let
        array =
            Block.arrayFromString str
    in
    parseStringArray version array


parseStringArray : Int -> Array String -> Tree BlockData
parseStringArray version array =
    loop (initParserState version array) nextState



-- TYPES


type alias ParserState =
    { bzs : BlockZipperState
    , array : Array String
    , cursor : Int
    , arrayLength : Int
    , counter : Int
    , id : Id
    }


type alias BlockZipperState =
    { zipper : Zipper BlockData, stack : Stack BlockType }



-- PARSER


initParserState : Int -> Array String -> ParserState
initParserState version array =
    { array = array
    , cursor = 0
    , bzs = initState
    , arrayLength = Array.length array
    , counter = 0
    , id = ( version, 0 )
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
                --    Debug.log "(NB, TS, >=)" ( newBlock.blockType, Stack.top parserState.bzs.stack, Maybe.map2 Block.greaterThanOrEqual (Just newBlock.blockType) (Stack.top parserState.bzs.stack) )
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
                            (map (ap newBlock) parserState
                                |> map lc
                                |> updateCursor newBlock.blockEnd
                                |> incrementCounter
                            )



-- PARSER OPERATIONS


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


isInjective : String -> Bool
isInjective str =
    let
        array =
            Block.arrayFromString str

        qIdentity =
            toStringArray << parseStringArray 0

        array2 =
            qIdentity array
    in
    array2 == array



-- CONVERSIONS


toString : Tree BlockData -> String
toString tree =
    Tree.foldl (\str acc -> acc ++ str) "" (toStringTree tree)
        |> String.dropLeft (String.length "Document\n\n\n")


toStringArray : Tree BlockData -> Array String
toStringArray tree =
    Tree.foldl (\block list -> (block.array |> Array.toList |> List.reverse) ++ list) [] tree
        |> List.reverse
        |> List.drop 1
        |> Array.fromList


toStringTree : Tree BlockData -> Tree String
toStringTree tree =
    let
        mapper : BlockData -> String
        mapper bd =
            bd.array |> Array.toList |> String.join "\n" |> (\x -> "\n" ++ x)
    in
    Tree.map mapper tree


toTaggedStringTree : Tree BlockData -> Tree ( String, Int )
toTaggedStringTree tree =
    tree
        |> toStringTree
        |> HTree.tagWithDepth


{-| Return a tree representing (BlockType, depth of node)
-}
toBlockTypeTree : Tree BlockData -> Tree ( String, Int )
toBlockTypeTree tree =
    let
        mapper : BlockData -> String
        mapper bd =
            Debug.toString bd.blockType
    in
    Tree.map mapper tree
        |> HTree.tagWithDepth
