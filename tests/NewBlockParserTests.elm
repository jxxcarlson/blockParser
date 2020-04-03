module NewBlockParserTests exposing (suite)

import BLParser.Block as Block
import BLParser.Edit as Edit
import BLParser.TestCode as TC
import Expect exposing (Expectation)
import Test exposing (..)
import Tree
import Tree.Extra


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
"""


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
            , test "Edit.separate" <|
                let
                    parserState =
                        TC.parserStateOfString xx

                    separatedTree =
                        Edit.separate 5 6 parserState

                    spanningTree =
                        Tuple.first separatedTree |> Maybe.map (Tree.map Block.stringOf)

                    prunedTree =
                        Tuple.second separatedTree |> Maybe.map (Tree.map Block.stringOf)

                    expectedSpanningTree =
                        Just (t "\n| subsection D" [ t "\nE" [], t "\nF" [ t "" [] ] ])

                    expectedPrunedTree =
                        Just (t "" [ t "| section A" [ t "\n| subsection B" [ t "\nC" [] ] ] ])
                in
                \_ ->
                    Expect.equal ( spanningTree, prunedTree ) ( expectedSpanningTree, expectedPrunedTree )
            ]
        ]
