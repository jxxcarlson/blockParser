module MU.BlockTree exposing
    ( blockTreeOfString
    , parseTreeToString
    , toBlockTypeTree
    , toString
    , toStringArray
    , toStringArrayFromParseTree
    , toStringTree
    )

import Array exposing (Array)
import HTree
import MU.Block as Block exposing (Block)
import MU.Id as Id exposing (Id)
import MU.Language as Language exposing (Language)
import MU.Parse as Parse
import MU.Source as Source
import Tree exposing (Tree)


blockTreeOfString : Language blockType -> String -> Tree (Block blockType)
blockTreeOfString lang =
    Source.fromString >> Parse.parseSource lang Id.initial >> Parse.toTree


toString : Tree (Block blockType) -> String
toString tree =
    Tree.foldl (\str acc -> acc ++ str) "" (toStringTree tree)


parseTreeToString : Tree (Block blockType) -> String
parseTreeToString tree =
    Tree.foldl (\str acc -> acc ++ str) "" (toStringTree tree)
        |> String.dropLeft (String.length "\n\n")


toStringTree : Tree (Block blockType) -> Tree String
toStringTree tree =
    let
        mapper : Block blockType -> String
        mapper b =
            Block.arrayOf b |> Array.toList |> String.join "\n" |> (\x -> "\n" ++ x)
    in
    Tree.map mapper tree


toStringArrayFromParseTree : Tree (Block blockType) -> Array String
toStringArrayFromParseTree tree =
    Tree.foldl (\block list -> (Block.arrayOf block |> Array.toList |> List.reverse) ++ list) [] tree
        |> List.reverse
        |> List.drop 1
        |> Array.fromList


toStringArray : Tree (Block blockType) -> Array String
toStringArray tree =
    Tree.foldl (\block list -> (Block.arrayOf block |> Array.toList |> List.reverse) ++ list) [] tree
        |> List.reverse
        |> Array.fromList


{-|

    parseSource source |> toTree |> BlockTree.toBlockTypeTree

-}
toBlockTypeTree : Tree (Block blockType) -> Tree ( ( String, Maybe Id ), Int )
toBlockTypeTree tree =
    let
        mapper : Block blockType -> ( String, Maybe Id )
        mapper bd =
            ( Block.stringOf bd, Block.idOf bd )
    in
    Tree.map mapper tree
        |> HTree.tagWithDepth
