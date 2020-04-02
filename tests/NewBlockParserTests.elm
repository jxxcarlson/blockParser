module NewBlockParserTests exposing (suite)

import Edit.Parse as Parse
import Expect exposing (Expectation)
import HTree exposing (tagWithDepth)
import Test exposing (..)
import Text exposing (..)
import Tree exposing (tree)


suite : Test
suite =
    describe "The BlockParser module"
        [ describe "BlockParser.parse"
            [ test "parse a document with two nodes" <|
                \_ ->
                    "| section Intro\n\nA"
                        |> blockTreeFromString
                        |> toStringTree
                        |> Expect.equal (tree "\nDocument" [ tree "\n| section Intro" [ tree "\n\nA" [] ] ])
            , test "parse a document of depth two with three nodes" <|
                \_ ->
                    "| section Intro\n\n| subsection A\n\n| subsection B"
                        |> blockTreeFromString
                        |> toStringTree
                        |> Expect.equal (tree "\nDocument" [ tree "\n| section Intro" [ tree "\n\n| subsection A" [], tree "\n\n| subsection B" [] ] ])
            , test "parse a document of depth two with three nodes, test depths" <|
                \_ ->
                    "| section Intro\n\n| subsection A\n\n| subsection B"
                        |> blockTreeFromString
                        |> toStringTree
                        |> tagWithDepth
                        |> Tree.map Tuple.second
                        |> Expect.equal (tree 0 [ tree 1 [ tree 2 [], tree 2 [] ] ])
            , test "parse a document of depth two with two text nodes and a math node" <|
                \_ ->
                    "| section Intro\n\nFee, fie fo fum\n\nRoses are red,\nviolets are blue\n\n| math\na^2 + b^2 = c^2"
                        |> blockTreeFromString
                        |> toStringTree
                        |> Expect.equal (tree "\nDocument" [ tree "\n| section Intro" [ tree "\n\nFee, fie fo fum" [], tree "\n\nRoses are red,\nviolets are blue" [], tree "\n\n| math\na^2 + b^2 = c^2" [] ] ])
            , test "parse a document of depth two with two text nodes and a math node followed by a text node" <|
                \_ ->
                    "| section Intro\n\nFee, fie fo fum\n\nRoses are red,\nviolets are blue\n\n| math\na^2 + b^2 = c^2\n\nho ho ho!"
                        |> blockTreeFromString
                        |> toStringTree
                        |> Expect.equal (tree "\nDocument" [ tree "\n| section Intro" [ tree "\n\nFee, fie fo fum" [], tree "\n\nRoses are red,\nviolets are blue" [], tree "\n\n| math\na^2 + b^2 = c^2" [], tree "\n\nho ho ho!" [] ] ])
            ]
        , describe
            "Editing"
            [ test "ed1" <|
                \_ ->
                    let
                        ps1 =
                            BlockParser.parseString ed1

                        a =
                            BlockParser.replaceLine 5 "| section C" ps1
                                |> BlockParser.toTree
                                |> BlockTree.toBlockTypeTree

                        b =
                            BlockParser.parseString ed2
                                |> BlockParser.toTree
                                |> BlockTree.toBlockTypeTree
                    in
                    Expect.equal a b
            ]
        ]
