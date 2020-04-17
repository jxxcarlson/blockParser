module NewBlockParserTests exposing (suite)

import Array exposing (Array)
import BLParser.Block as Block
import BLParser.BlockTree as BlockTree
import BLParser.Edit as Edit
import BLParser.TestCode as TC
import Expect exposing (Expectation)
import Test exposing (..)
import Tree
import Tree.Extra


stringFromArray : Array String -> String
stringFromArray array =
    array
        |> Array.toList
        |> String.join "\n"


t =
    Tree.tree


s =
    Tree.singleton


tt =
    """
| section A

B

| section C

D

"""


xx =
    """| section A

| subsection B

C

| subsection D

E

F

| section G"""


suite : Test
suite =
    describe "The BlockParser module"
        [ describe "BlockParser.parse"
            [ test "Parse.parse using blockStringOfString" <|
                \_ ->
                    TC.blockTreeOfString tt
                        |> Tree.map Block.stringOf
                        |> Tree.Extra.tagWithDepth
                        |> Expect.equal (t ( "", 0 ) [ t ( "\n| section A", 1 ) [ t ( "\nB", 2 ) [] ], t ( "\n| section C", 1 ) [ t ( "\nD", 2 ) [ t ( "\n", 3 ) [] ] ] ])
            , test "Injectivity" <|
                let
                    bt =
                        TC.blockTreeOfString tt

                    tt2 =
                        BlockTree.parseTreeToString bt
                in
                \_ ->
                    Expect.equal tt tt2
            , test "Edit.separate" <|
                let
                    parserState =
                        TC.parserStateOfString xx

                    separatedTree =
                        Edit.separate 5 6 parserState

                    spanningTree =
                        separatedTree |> Maybe.map .spanningTree |> Maybe.map (Tree.map Block.stringOf)

                    prunedTree =
                        separatedTree |> Maybe.map .prunedTree |> Maybe.map (Tree.map Block.stringOf)

                    expectedSpanningTree =
                        Just (t "\n| subsection D" [ s "\nE", s "\nF" ])

                    expectedPrunedTree =
                        Just (t "" [ t "| section A" [ t "\n| subsection B" [ t "\nC" [] ] ], s "\n| section G" ])
                in
                \_ ->
                    Expect.equal ( spanningTree, prunedTree ) ( expectedSpanningTree, expectedPrunedTree )
            ]
        ]
