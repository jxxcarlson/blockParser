module BlockParserTest exposing (suite)

import BlockParser exposing (parse, toStringTree)
import Expect exposing (Expectation)
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
            , test "parse a document of depth two with two text nodes and a math node" <|
                \_ ->
                    "| section Intro\n\nFee, fie fo fum\n\nRoses are red,\nviolets are blue\n\n| math\na^2 + b^2 = c^2"
                        |> parse
                        |> toStringTree
                        |> Expect.equal (tree "Document" [ tree "| section Intro" [ tree "\nFee, fie fo fum" [], tree "\nRoses are red,\nviolets are blue" [], tree "\n| math\na^2 + b^2 = c^2" [] ] ])
            , test "parse a document of depth two with two text nodes and a math node followed by a text node" <|
                \_ ->
                    "| section Intro\n\nFee, fie fo fum\n\nRoses are red,\nviolets are blue\n\n| math\na^2 + b^2 = c^2\n\nho ho ho!"
                        |> parse
                        |> toStringTree
                        |> Expect.equal (tree "Document" [ tree "| section Intro" [ tree "\nFee, fie fo fum" [], tree "\nRoses are red,\nviolets are blue" [], tree "\n| math\na^2 + b^2 = c^2" [], tree "\nho ho ho!" [] ] ])
            ]
        ]
