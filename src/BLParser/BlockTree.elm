module BLParser.BlockTree exposing (toBlockTypeTree, toString, toStringArray, toStringTree)

import Array exposing (Array)
import BLParser.Block as Block exposing (Block)
import BLParser.Id exposing (Id)
import HTree
import Tree exposing (Tree)


toString : Tree Block -> String
toString tree =
    Tree.foldl (\str acc -> acc ++ str) "" (toStringTree tree)
        |> String.dropLeft (String.length "\n\n")


toStringTree : Tree Block -> Tree String
toStringTree tree =
    let
        mapper : Block -> String
        mapper b =
            Block.arrayOf b |> Array.toList |> String.join "\n" |> (\x -> "\n" ++ x)
    in
    Tree.map mapper tree


toStringArray : Tree Block -> Array String
toStringArray tree =
    Tree.foldl (\block list -> (Block.arrayOf block |> Array.toList |> List.reverse) ++ list) [] tree
        |> List.reverse
        |> List.drop 1
        |> Array.fromList


{-|

    parseSource source |> toTree |> BlockTree.toBlockTypeTree

-}
toBlockTypeTree : Tree Block -> Tree ( ( String, Maybe Id ), Int )
toBlockTypeTree tree =
    let
        mapper : Block -> ( String, Maybe Id )
        mapper bd =
            ( Debug.toString (Block.typeOf bd), Block.idOf bd )
    in
    Tree.map mapper tree
        |> HTree.tagWithDepth
