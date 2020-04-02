module BLParser.TestCode exposing (blockTreeOfString)

import BLParser.Block as Block exposing (Block)
import BLParser.BlockTree as BlockTree
import BLParser.Parse as Parse
import BLParser.Source as Source
import Tree exposing (Tree)


{-|

    > blockTreeOfString t1 |> Tree.map Block.stringOf |> HTree.tagWithDepth
    Tree ("",0) [Tree ("\n| section A",1) [Tree ("\nB",2) []],Tree ("\n| section C",1) [Tree ("\nD",2) [Tree ("\n",3) []]]]

-}
blockTreeOfString : String -> Tree Block
blockTreeOfString =
    Source.fromString >> Parse.parseSource >> Parse.toTree
