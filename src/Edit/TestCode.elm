module Edit.TestCode exposing (blockTreeOfString)

import Edit.Block as Block exposing (Block)
import Edit.BlockTree as BlockTree
import Edit.Parse as Parse
import Edit.Source as Source
import Tree exposing (Tree)


{-|

    > blockTreeOfString t1 |> Tree.map Block.stringOf |> HTree.tagWithDepth
    Tree ("",0) [Tree ("\n| section A",1) [Tree ("\nB",2) []],Tree ("\n| section C",1) [Tree ("\nD",2) [Tree ("\n",3) []]]]

-}
blockTreeOfString : String -> Tree Block
blockTreeOfString =
    Source.fromString >> Parse.parseSource >> Parse.toTree
