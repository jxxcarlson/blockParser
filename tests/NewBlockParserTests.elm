module NewBlockParserTests exposing (suite)

import BLParser.Block as Block
import BLParser.TestCode as TC
import Expect exposing (Expectation)
import Test exposing (..)
import Tree
import Tree.Extra


t =
    Tree.tree


t1 =
    """
| section A

B

| section C

D

"""


suite : Test
suite =
    describe "The BlockParser module"
        [ describe "BlockParser.parse"
            [ test "parse a document" <|
                \_ ->
                    TC.blockTreeOfString t1
                        |> Tree.map Block.stringOf
                        |> Tree.Extra.tagWithDepth
                        |> Expect.equal (t ( "", 0 ) [ t ( "\n| section A", 1 ) [ t ( "\nB", 2 ) [] ], t ( "\n| section C", 1 ) [ t ( "\nD", 2 ) [ t ( "\n", 3 ) [] ] ] ])
            ]
        ]
