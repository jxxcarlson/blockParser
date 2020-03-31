module Edit.BlockTree exposing (toBlockTypeTree, toStringTree)

import Array
import Edit.Block as Block exposing (Block)
import Edit.Id exposing (Id)
import HTree
import Tree exposing (Tree)


toStringTree : Tree Block -> Tree String
toStringTree tree =
    let
        mapper : Block -> String
        mapper b =
            Block.arrayOf b |> Array.toList |> String.join "\n" |> (\x -> "\n" ++ x)
    in
    Tree.map mapper tree


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
