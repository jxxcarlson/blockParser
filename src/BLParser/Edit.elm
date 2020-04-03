module BLParser.Edit exposing (after_, before_, between_, check, edit, getParts, separate, spanningTreeOfSourceRange)

import Array exposing (Array)
import BLParser.Block as Block exposing (Block)
import BLParser.BlockTree as BlockTree
import BLParser.Id as Id exposing (Id)
import BLParser.Parse as Parse exposing (ParserState)
import BLParser.Source as Source exposing (Source)
import BLParser.SourceMap as SourceMap
import Maybe.Extra
import Tree exposing (Tree)
import Tree.Extra



-- edit : Int -> Int -> Source -> ParserState -> ParserState
-- edit : Int -> Int -> Source -> ParserState -> ( Maybe (Tree Block), Maybe (Tree Block) )


edit from to newSource parserState =
    let
        theSource =
            Parse.getSource parserState

        before =
            Source.slice 0 from theSource

        ( spanningTreeOfAffectedSource, prunedTree ) =
            separate from to parserState

        spanningSource_ =
            spanningTreeOfAffectedSource |> Maybe.map BlockTree.toStringArrayFromParseTree
    in
    case spanningSource_ of
        Nothing ->
            Nothing

        Just spanningSource ->
            let
                k =
                    Source.length before + Array.length spanningSource

                after =
                    Source.slice k (Source.length theSource) theSource
            in
            Just ( before, spanningSource, after )


type alias EditParts =
    { spanningTreeOfAffectedSource : Maybe (Tree Block)
    , prunedTree : Maybe (Tree Block)
    , before : Source
    , between : Array String
    , after : Source
    }


before_ : Maybe EditParts -> Maybe (Array String)
before_ maybeEditParts =
    case maybeEditParts of
        Nothing ->
            Nothing

        Just ep ->
            Just <| Source.toArray ep.before


between_ : Maybe EditParts -> Maybe (Array String)
between_ maybeEditParts =
    case maybeEditParts of
        Nothing ->
            Nothing

        Just ep ->
            Just <| ep.between


after_ : Maybe EditParts -> Maybe (Array String)
after_ maybeEditParts =
    case maybeEditParts of
        Nothing ->
            Nothing

        Just ep ->
            Just <| Source.toArray ep.after


check : Maybe EditParts -> Maybe (Array String)
check maybeEditParts =
    case maybeEditParts of
        Nothing ->
            Nothing

        Just ep ->
            Just <| Array.append (Array.append (Source.toArray ep.before) ep.between) (Source.toArray ep.after)


getParts : Int -> Int -> ParserState -> Maybe EditParts
getParts from to parserState =
    let
        theSource =
            Parse.getSource parserState

        before =
            Source.slice 0 from theSource

        ( spanningTreeOfAffectedSource, prunedTree ) =
            separate from to parserState

        spanningSource_ =
            spanningTreeOfAffectedSource |> Maybe.map BlockTree.toStringArray
    in
    case spanningSource_ of
        Nothing ->
            Nothing

        Just spanningSource ->
            let
                k =
                    Source.length before + Array.length spanningSource

                after =
                    Source.slice k (Source.length theSource) theSource
            in
            Just
                { spanningTreeOfAffectedSource = spanningTreeOfAffectedSource
                , prunedTree = prunedTree
                , before = before
                , between = spanningSource
                , after = after
                }



-- Maybe.map BlockTree.toStringArray


separate : Int -> Int -> ParserState -> ( Maybe (Tree Block), Maybe (Tree Block) )
separate from to parserState =
    let
        ast =
            Parse.toTree parserState

        spanningTreeOfAffectedSource =
            spanningTreeOfSourceRange (Debug.log "FROM" from) to parserState

        spanningTreeRoot =
            Maybe.map Tree.label spanningTreeOfAffectedSource

        prunedTree =
            Maybe.map2 Tree.Extra.removeSubtree spanningTreeRoot (Just ast)
                |> Maybe.Extra.join
    in
    ( spanningTreeOfAffectedSource, prunedTree )


{-| Given two integers that define a range of lines in the source map
of the parser state, return the spanning tree
-}
spanningTreeOfSourceRange : Int -> Int -> ParserState -> Maybe (Tree Block)
spanningTreeOfSourceRange from to parserState =
    let
        ast : Tree Block
        ast =
            Parse.toTree parserState

        affectedIds : List Id
        affectedIds =
            Debug.log "affectedIds"
                (SourceMap.range from to (Parse.getSourceMap parserState)
                    |> SourceMap.idList
                )

        affectedNodes : List Block
        affectedNodes =
            List.map (getNodeFromTree ast) affectedIds
                |> Maybe.Extra.values

        spanningTree_ : Maybe (Tree Block)
        spanningTree_ =
            Tree.Extra.spanningTree affectedNodes ast
    in
    spanningTree_


getNodeFromTree : Tree Block -> Id -> Maybe Block
getNodeFromTree tree id =
    let
        f : Block -> List Block -> List Block
        f block list =
            case Just id == Block.idOf block of
                True ->
                    block :: list

                False ->
                    list
    in
    Tree.foldl f [] tree
        |> List.head
