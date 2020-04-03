module BLParser.Edit exposing
    ( after_
    , before_
    , between_
    , check
    , edit
    , getParts
    , prepareEditParts
    , separate
    , spanningTreeOfSourceRange
    )

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
-- edit : Int -> Int -> c -> ParserState -> Maybe EditParts


edit from to insertionText parserState =
    case prepareEditParts from to insertionText parserState of
        Nothing ->
            Nothing

        Just ep ->
            let
                foo =
                    1
            in
            Nothing


type alias EditParts =
    { newSource : Source, textToParse : Source, prunedTree : Tree Block }


{-|

    > prepareEditParts 5 6 s2 ps1 |> Maybe.map .prunedTree  |> Maybe.map (Tree.map Block.stringOf)
    FROM: 5
    affectedIds: [Id 0 4]
    HEAD: Array.fromList ["","| subsection D"]
    TAIL: Array.fromList ["","E","","F"]
    Just (Tree "" [Tree ("| section A") [Tree ("\n| subsection B") [Tree "\nC" []]],Tree ("\n| section G") [Tree "" []]])

-}
prepareEditParts : Int -> Int -> Source -> ParserState -> Maybe EditParts
prepareEditParts from to insertionText parserState =
    case getParts from to parserState of
        Nothing ->
            Nothing

        Just ep ->
            let
                firstIndexOfUnchangedSource =
                    to - from + 1

                n =
                    Array.length ep.between

                head =
                    Debug.log "HEAD" <|
                        Array.slice 0 firstIndexOfUnchangedSource ep.between

                tail =
                    Debug.log "TAIL" <|
                        Array.slice firstIndexOfUnchangedSource n ep.between

                textToParse =
                    Source.fromArray (Array.append (Source.toArray insertionText) tail)
            in
            case ep.prunedTree of
                Nothing ->
                    Nothing

                Just prunedTree ->
                    Just
                        { newSource = Source.merge ep.before textToParse ep.after
                        , textToParse = textToParse
                        , prunedTree = prunedTree
                        }


type alias PreliminaryEditParts =
    { spanningTreeOfAffectedSource : Maybe (Tree Block)
    , prunedTree : Maybe (Tree Block)
    , before : Source
    , between : Array String
    , after : Source
    }


getParts : Int -> Int -> ParserState -> Maybe PreliminaryEditParts
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
        ast =
            Parse.toTree parserState

        affectedIds =
            Debug.log "affectedIds"
                (SourceMap.range from to (Parse.getSourceMap parserState)
                    |> SourceMap.idList
                )

        affectedNodes =
            List.map (getNodeFromTree ast) affectedIds
                |> Maybe.Extra.values

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



-- TESTING


check : Maybe PreliminaryEditParts -> Maybe (Array String)
check maybeEditParts =
    case maybeEditParts of
        Nothing ->
            Nothing

        Just ep ->
            Just <| Array.append (Array.append (Source.toArray ep.before) ep.between) (Source.toArray ep.after)


before_ : Maybe PreliminaryEditParts -> Maybe (Array String)
before_ maybeEditParts =
    case maybeEditParts of
        Nothing ->
            Nothing

        Just ep ->
            Just <| Source.toArray ep.before


between_ : Maybe PreliminaryEditParts -> Maybe (Array String)
between_ maybeEditParts =
    case maybeEditParts of
        Nothing ->
            Nothing

        Just ep ->
            Just <| ep.between


after_ : Maybe PreliminaryEditParts -> Maybe (Array String)
after_ maybeEditParts =
    case maybeEditParts of
        Nothing ->
            Nothing

        Just ep ->
            Just <| Source.toArray ep.after
