module BLParser.Edit exposing
    (  --after_
       --, before_
       --, between_
       --, check
       edit

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
import BLParser.SourceMap as SourceMap exposing (SourceMap)
import Maybe.Extra
import Tree exposing (Tree)
import Tree.Extra
import Tree.Zipper as Zipper exposing (Zipper)


{-|

    > edit 5 6 s2 ps1 |> Maybe.map (Tree.map Block.stringOf) |> Maybe.map Tree.Extra.tagWithDepth
    FROM: 5
    affectedIds: [Id 0 4]
    TAIL: Array.fromList ["","E","","F"]
    ID: Id 1 8
    attachmentNode: Block { blockEnd = 0, blockStart = 0, blockType = Root, id = Just (Id 0 0), source = Source (Array.fromList []) }
    Just (Tree ("",0) [Tree ("| section A",1) [Tree ("\n| subsection B",2) [Tree ("\nC",3) []]],Tree ("\n| section G",1) [Tree ("",2) []],Tree ("\n| section X",1) [Tree ("\nE",2) [],Tree ("\nF",2) []]])

-}
edit : Int -> Int -> Source -> ParserState -> Maybe ParserState
edit from to insertionText parserState =
    case ( prepareEditParts from to insertionText parserState, Parse.getId parserState ) of
        ( Nothing, _ ) ->
            Nothing

        ( _, Nothing ) ->
            Nothing

        ( Just ep, Just id ) ->
            let
                newId =
                    Id.incrementVersion id

                newSubTree_ =
                    Parse.parseSource newId ep.textToParse
                        |> Parse.toTree
                        |> Tree.children
                        |> List.head

                newParserState =
                    parserState
                        |> Parse.setSource (Debug.log "ep.newSource" ep.newSource)

                _ =
                    Debug.log "source of newParserState " (Parse.getSource newParserState)
            in
            case newSubTree_ of
                Nothing ->
                    Nothing

                Just subTree ->
                    let
                        newParseTree =
                            Tree.Extra.attachSubtreeInOrder Block.gte ep.attachmentNode subTree ep.prunedTree
                    in
                    Just newParserState
                        |> Maybe.map2 Parse.setBzs newParseTree



-- |> Maybe.map2 Parse.setSource (Just ep.newSource)


type alias ExpansionData =
    { from : Int
    , to : Int
    , insertionText : Source
    }


expand : SourceMap -> Source -> Source -> Int -> Int -> ExpansionData
expand sourceMap source insertionText from to =
    let
        foo =
            9
    in
    { from = from, to = to, insertionText = insertionText }


type alias EditParts =
    { newSource : Source, textToParse : Source, prunedTree : Tree Block, attachmentNode : Block }


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
                textToParse =
                    Source.merge ep.uuu insertionText ep.www
            in
            Just
                { newSource = Source.merge ep.before textToParse ep.after
                , textToParse = textToParse
                , prunedTree = ep.prunedTree
                , attachmentNode = ep.attachmentNode
                }


type alias PreliminaryEditParts =
    { spanningTree : Tree Block
    , prunedTree : Tree Block
    , attachmentNode : Block
    , uuu : Source
    , www : Source
    , before : Source
    , after : Source
    }


getParts : Int -> Int -> ParserState -> Maybe PreliminaryEditParts
getParts from to parserState =
    case separate from to parserState of
        Nothing ->
            Nothing

        Just separationData ->
            let
                theSource =
                    Parse.getSource parserState

                before =
                    Source.slice 0 separationData.startOfSpan theSource

                spanningSource =
                    separationData.spanningTree |> BlockTree.toStringArray

                uuu =
                    Source.slice separationData.startOfSpan from theSource |> Debug.log "U"

                vvv =
                    Source.slice from (to + 1) theSource |> Debug.log "V"

                www =
                    Source.slice (to + 1) (separationData.endOfSpan + 0) theSource |> Debug.log "W"

                k =
                    Source.length before + Array.length spanningSource

                after =
                    Source.slice separationData.endOfSpan (Source.length theSource) theSource
            in
            Just
                { spanningTree = separationData.spanningTree
                , prunedTree = separationData.prunedTree
                , attachmentNode = separationData.attachmentNode
                , uuu = uuu
                , www = www
                , before = before
                , after = after
                }


setFocus : a -> Zipper a -> Maybe (Zipper a)
setFocus node zipper =
    Zipper.findFromRoot (\label -> label == node) zipper


type alias SeparationData =
    { spanningTree : Tree Block
    , startOfSpan : Int
    , endOfSpan : Int
    , prunedTree : Tree Block
    , attachmentNode : Block
    }


separate : Int -> Int -> ParserState -> Maybe SeparationData
separate from to parserState =
    case spanningTreeOfSourceRange from to parserState of
        Nothing ->
            Nothing

        Just spanningData ->
            let
                ast =
                    Parse.toTree parserState

                spanningTreeRoot =
                    Tree.label spanningData.spanningTree

                attachmentNode_ =
                    Parse.getZipper parserState
                        |> setFocus spanningTreeRoot
                        |> Maybe.andThen Zipper.parent
                        |> Maybe.map Zipper.label

                prunedTree_ =
                    Tree.Extra.removeSubtree spanningTreeRoot ast
            in
            case ( prunedTree_, attachmentNode_ ) of
                ( Nothing, _ ) ->
                    Nothing

                ( _, Nothing ) ->
                    Nothing

                ( Just prunedTree, Just attachmentNode ) ->
                    Just
                        { spanningTree = spanningData.spanningTree
                        , startOfSpan = spanningData.startOfSpan
                        , endOfSpan = spanningData.endOfSpan
                        , prunedTree = prunedTree
                        , attachmentNode = attachmentNode
                        }


type alias SpanningData =
    { spanningTree : Tree Block
    , startOfSpan : Int
    , endOfSpan : Int
    }


{-| Given two integers that define a range of lines in the source map
of the parser state, return the spanning tree
-}
spanningTreeOfSourceRange : Int -> Int -> ParserState -> Maybe SpanningData
spanningTreeOfSourceRange from to parserState =
    let
        ast =
            Parse.toTree parserState

        affectedIds =
            SourceMap.range from (to + 1) (Parse.getSourceMap parserState)
                |> SourceMap.idList

        affectedNodes : List Block
        affectedNodes =
            List.map (getNodeFromTree ast) affectedIds
                |> Maybe.Extra.values

        startOfSpan : Maybe Int
        startOfSpan =
            List.head affectedNodes
                |> Maybe.map (\b -> Block.blockStart b)
                |> Debug.log "startOfSpan"

        endOfSpan : Maybe Int
        endOfSpan =
            List.head (List.reverse affectedNodes)
                |> Maybe.map (\b -> Block.blockEnd b)
                |> Debug.log "endOfSpan"

        spanningTree_ : Maybe (Tree Block)
        spanningTree_ =
            Tree.Extra.spanningTree affectedNodes ast
    in
    case ( spanningTree_, startOfSpan, endOfSpan ) of
        ( Just tree_, Just start_, Just end_ ) ->
            Just { spanningTree = tree_, startOfSpan = start_, endOfSpan = end_ }

        _ ->
            Nothing


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
--
--
--check : Maybe PreliminaryEditParts -> Maybe (Array String)
--check maybeEditParts =
--    case maybeEditParts of
--        Nothing ->
--            Nothing
--
--        Just ep ->
--            Just <| Array.append (Array.append (Source.toArray ep.before) ep.between) (Source.toArray ep.after)
--
--
--before_ : Maybe PreliminaryEditParts -> Maybe (Array String)
--before_ maybeEditParts =
--    case maybeEditParts of
--        Nothing ->
--            Nothing
--
--        Just ep ->
--            Just <| Source.toArray ep.before
--
--
--between_ : Maybe PreliminaryEditParts -> Maybe (Array String)
--between_ maybeEditParts =
--    case maybeEditParts of
--        Nothing ->
--            Nothing
--
--        Just ep ->
--            Just <| ep.between
--
--
--after_ : Maybe PreliminaryEditParts -> Maybe (Array String)
--after_ maybeEditParts =
--    case maybeEditParts of
--        Nothing ->
--            Nothing
--
--        Just ep ->
--            Just <| Source.toArray ep.after
