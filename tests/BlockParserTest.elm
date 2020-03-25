module BlockParserTest exposing (suite)

import BlockParser exposing (parse, toStringTree)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Tree exposing (tree)


suite : Test
suite =
    describe "The BlockParser module"
        [ describe "BlockParser.parse"
            [ test "parse a document with two nodes" <|
                \_ ->
                    "| section Intro\n\nA"
                        |> parse
                        |> toStringTree
                        |> Expect.equal (tree "Document" [ tree "| section Intro" [ tree "\nA" [] ] ])
            , test "parse a document of depth two with three nodes" <|
                \_ ->
                    "| section Intro\n\n| subsection A\n\n| subsection B"
                        |> parse
                        |> toStringTree
                        |> Expect.equal (tree "Document" [ tree "| section Intro" [ tree "\n| subsection A" [], tree "\n| subsection B" [] ] ])
            ]
        ]
