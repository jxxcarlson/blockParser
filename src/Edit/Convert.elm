module Edit.Convert exposing (blockTreeToBlockTypeTree, blockTreeToStringTree, toTree)

import Array
import Edit.Block as Block exposing (Block)
import Edit.Id exposing (Id)
import Edit.Parse exposing (ParserState)
import HTree
import Tree exposing (Tree)
import Tree.Zipper as Zipper


toTree : ParserState -> Tree Block
toTree state =
    state
        |> .bzs
        |> .zipper
        |> Zipper.toTree


blockTreeToStringTree : Tree Block -> Tree String
blockTreeToStringTree tree =
    let
        mapper : Block -> String
        mapper b =
            Block.arrayOf b |> Array.toList |> String.join "\n" |> (\x -> "\n" ++ x)
    in
    Tree.map mapper tree


blockTreeToBlockTypeTree : Tree Block -> Tree ( ( String, Maybe Id ), Int )
blockTreeToBlockTypeTree tree =
    let
        mapper : Block -> ( String, Maybe Id )
        mapper bd =
            ( Debug.toString (Block.typeOf bd), Block.idOf bd )
    in
    Tree.map mapper tree
        |> HTree.tagWithDepth
