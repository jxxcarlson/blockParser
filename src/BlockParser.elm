module BlockParser exposing
    ( annotatedLines
    , getArraySegment
    , getNode
    , getNodeAtLine
    , isInjective
    , parseString
    , parseStringArrayWithVersion
    , parseStringWithVersion
    , sourceMapFromTree
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
import Block exposing (Block, BlockType, Id)
import Diff
import HTree
import Loop exposing (Step(..), loop)
import Maybe.Extra
import Stack exposing (Stack)
import Tree exposing (Tree)
import Tree.Zipper as Zipper exposing (Zipper)



-- TYPES


type alias ParserState =
    { bzs : BlockZipperState
    , array : Array String
    , sourceMap : Array (Maybe Id)
    , cursor : Int
    , arrayLength : Int
    , counter : Int
    , id : Maybe Id
    }


type alias BlockZipperState =
    { zipper : Zipper Block, stack : Stack BlockType }


initParserState : Int -> Array String -> ParserState
initParserState version array =
    { array = array
    , sourceMap = Array.fromList (List.repeat (Array.length array) Nothing)
    , cursor = 0
    , bzs = initState
    , arrayLength = Array.length array
    , counter = 0
    , id = Just ( version, 0 )
    }


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
    parse (initParserState version array)
        |> .bzs
        |> .zipper
        |> Zipper.toTree


parse : ParserState -> ParserState
parse parserState =
    loop parserState nextState


{-|

    > getArraySegment (0,9) bt
    Just (20,22)

-}
getArraySegment : Id -> Tree Block -> Maybe ( Int, Int )
getArraySegment id tree =
    let
        getData : Block -> ( Int, Int )
        getData b =
            ( b.blockStart, b.blockEnd )
    in
    getNode id tree
        |> Maybe.map getData


{-|

    > bt = parseString text4
    > > getNode (0,9) bt
      [{ array = Array.fromList ["","One proton"], blockEnd = 22, blockStart = 20, blockType = Paragraph, id = Just (0,9) }]

-}
getNode : Id -> Tree Block -> Maybe Block
getNode id tree =
    let
        f : Block -> List Block -> List Block
        f block list =
            case Just id == block.id of
                True ->
                    block :: list

                False ->
                    list
    in
    Tree.foldl f [] tree
        |> List.head


{-|

    > getNodeAtLine sourceMap 5 bt
    Just { array = Array.fromList ["","Roses are red,","violets are blue"], blockEnd = 7, blockStart = 4, blockType = Paragraph, id = Just (0,3) }

-}
getNodeAtLine : Array (Maybe Id) -> Int -> Tree Block -> Maybe Block
getNodeAtLine sourceMap index tree =
    let
        maybeIndex =
            Array.get index sourceMap |> Maybe.Extra.join
    in
    case maybeIndex of
        Nothing ->
            Nothing

        Just id ->
            getNode id tree



-- |> Maybe.andThen (getNode tree)


annotatedLines : Tree Block -> Array ( String, Maybe Id )
annotatedLines tree =
    let
        annotateLines : Block -> Array ( String, Maybe Id )
        annotateLines bd =
            let
                id =
                    bd.id
            in
            Array.map (\line -> ( line, id )) bd.array
    in
    Tree.foldl (\bd acc -> Array.append acc (annotateLines bd)) Array.empty tree


sourceMapFromTree : Tree Block -> Array (Maybe Id)
sourceMapFromTree tree =
    let
        annotateLines : Block -> Array (Maybe Id)
        annotateLines bd =
            let
                id =
                    bd.id
            in
            Array.map (\line -> id) bd.array
    in
    Tree.foldl (\bd acc -> Array.append acc (annotateLines bd)) Array.empty tree



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



-- TESTS


isInjective : String -> Bool
isInjective str =
    let
        array =
            Block.arrayFromString str

        qIdentity =
            toStringArray << parseStringArrayWithVersion 0

        array2 =
            qIdentity array
    in
    array2 == array



-- CONVERSIONS


toString : Tree Block -> String
toString tree =
    Tree.foldl (\str acc -> acc ++ str) "" (toStringTree tree)
        |> String.dropLeft (String.length "Document\n\n\n")


toStringArray : Tree Block -> Array String
toStringArray tree =
    Tree.foldl (\block list -> (block.array |> Array.toList |> List.reverse) ++ list) [] tree
        |> List.reverse
        |> List.drop 1
        |> Array.fromList


toStringTree : Tree Block -> Tree String
toStringTree tree =
    let
        mapper : Block -> String
        mapper bd =
            bd.array |> Array.toList |> String.join "\n" |> (\x -> "\n" ++ x)
    in
    Tree.map mapper tree


toStringTreeWithId : Tree Block -> Tree ( String, Maybe Id )
toStringTreeWithId tree =
    let
        stringValue : Block -> String
        stringValue bd =
            bd.array |> Array.toList |> String.join "\n" |> (\x -> "\n" ++ x)

        mapper : Block -> ( String, Maybe Id )
        mapper bd =
            ( stringValue bd, bd.id )
    in
    Tree.map mapper tree


toTaggedStringTree : Tree Block -> Tree ( ( String, Maybe Id ), Int )
toTaggedStringTree tree =
    tree
        |> toStringTreeWithId
        |> HTree.tagWithDepth


{-| Return a tree representing (BlockType, depth of node)
-}
toBlockTypeTree : Tree Block -> Tree ( ( String, Maybe Id ), Int )
toBlockTypeTree tree =
    let
        mapper : Block -> ( String, Maybe Id )
        mapper bd =
            ( Debug.toString bd.blockType, bd.id )
    in
    Tree.map mapper tree
        |> HTree.tagWithDepth
