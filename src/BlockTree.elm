module BlockTree exposing
    ( annotatedLines
    , getArraySegmentFromTree
    , getNodeAtLine
    , getNodeFromTree
    , sourceMapFromTree
    , toBlockTypeTree
    , toString
    , toStringArray
    , toStringTree
    , toTaggedStringTree
    )

import Array exposing (Array)
import Block exposing (Block, BlockType, Id)
import HTree
import Maybe.Extra
import Tree exposing (Tree)


{-|

    > getArraySegment (0,9) bt
    Just (20,22)

-}
getArraySegmentFromTree : Id -> Tree Block -> Maybe ( Int, Int )
getArraySegmentFromTree id tree =
    let
        getData : Block -> ( Int, Int )
        getData b =
            ( b.blockStart, b.blockEnd )
    in
    getNodeFromTree id tree
        |> Maybe.map getData


{-|

    > bt = parseString text4
    > > getNode (0,9) bt
      [{ array = Array.fromList ["","One proton"], blockEnd = 22, blockStart = 20, blockType = Paragraph, id = Just (0,9) }]

-}
getNodeFromTree : Id -> Tree Block -> Maybe Block
getNodeFromTree id tree =
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
getNodeAtLine : Tree Block -> Array (Maybe Id) -> Int -> Maybe Block
getNodeAtLine tree sourceMap index =
    let
        maybeIndex =
            Array.get index sourceMap |> Maybe.Extra.join
    in
    case maybeIndex of
        Nothing ->
            Nothing

        Just id ->
            getNodeFromTree id tree



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
