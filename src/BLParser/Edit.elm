module BLParser.Edit exposing (edit, separate, spanningTreeOfSourceRange)

import Array exposing (Array)
import BLParser.Block as Block exposing (Block)
import BLParser.Id as Id exposing (Id)
import BLParser.Parse as Parse exposing (ParserState)
import BLParser.Source as Source exposing (Source)
import BLParser.SourceMap as SourceMap
import Maybe.Extra
import Tree exposing (Tree)
import Tree.Extra



-- edit : Int -> Int -> Source -> ParserState -> ParserState


edit : Int -> Int -> Source -> ParserState -> ( Maybe (Tree Block), Maybe (Tree Block) )
edit from to newSource parserState =
    let
        ast =
            Parse.toTree parserState

        spanningTreeOfAffectedSource =
            spanningTreeOfSourceRange from to parserState

        spanningTreeRoot =
            Maybe.map Tree.label spanningTreeOfAffectedSource

        prunedTree =
            Maybe.map2 Tree.Extra.removeSubtree spanningTreeRoot (Just ast)
                |> Maybe.Extra.join
    in
    ( spanningTreeOfAffectedSource, prunedTree )


separate : Int -> Int -> ParserState -> ( Maybe (Tree Block), Maybe (Tree Block) )
separate from to parserState =
    let
        ast =
            Parse.toTree parserState

        spanningTreeOfAffectedSource =
            spanningTreeOfSourceRange from to parserState

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
            SourceMap.range from to (Parse.getSourceMap parserState)
                |> SourceMap.idList

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
