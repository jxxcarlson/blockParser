module BlockParserTest exposing (suite)

import BlockParser exposing (parseString)
import BlockTree exposing (toStringTree)
import Expect exposing (Expectation)
import HTree exposing (tagWithDepth)
import Test exposing (..)
import Tree exposing (tree)


suite : Test
suite =
    describe "The BlockParser module"
        [ describe "BlockParser.parse"
            [ test "parse a document with two nodes" <|
                \_ ->
                    "| section Intro\n\nA"
                        |> parseString
                        |> toStringTree
                        |> Expect.equal (tree "\nDocument" [ tree "\n| section Intro" [ tree "\n\nA" [] ] ])
            , test "parse a document of depth two with three nodes" <|
                \_ ->
                    "| section Intro\n\n| subsection A\n\n| subsection B"
                        |> parseString
                        |> toStringTree
                        |> Expect.equal (tree "\nDocument" [ tree "\n| section Intro" [ tree "\n\n| subsection A" [], tree "\n\n| subsection B" [] ] ])
            , test "parse a document of depth two with three nodes, test depths" <|
                \_ ->
                    "| section Intro\n\n| subsection A\n\n| subsection B"
                        |> parseString
                        |> toStringTree
                        |> tagWithDepth
                        |> Tree.map Tuple.second
                        |> Expect.equal (tree 0 [ tree 1 [ tree 2 [], tree 2 [] ] ])
            , test "parse a document of depth two with two text nodes and a math node" <|
                \_ ->
                    "| section Intro\n\nFee, fie fo fum\n\nRoses are red,\nviolets are blue\n\n| math\na^2 + b^2 = c^2"
                        |> parseString
                        |> toStringTree
                        |> Expect.equal (tree "\nDocument" [ tree "\n| section Intro" [ tree "\n\nFee, fie fo fum" [], tree "\n\nRoses are red,\nviolets are blue" [], tree "\n\n| math\na^2 + b^2 = c^2" [] ] ])
            , test "parse a document of depth two with two text nodes and a math node followed by a text node" <|
                \_ ->
                    "| section Intro\n\nFee, fie fo fum\n\nRoses are red,\nviolets are blue\n\n| math\na^2 + b^2 = c^2\n\nho ho ho!"
                        |> parseString
                        |> toStringTree
                        |> Expect.equal (tree "\nDocument" [ tree "\n| section Intro" [ tree "\n\nFee, fie fo fum" [], tree "\n\nRoses are red,\nviolets are blue" [], tree "\n\n| math\na^2 + b^2 = c^2" [], tree "\n\nho ho ho!" [] ] ])
            ]
        ]
