module BLParser.TestCode exposing (arrayFromString, blockTreeOfString, parserStateOfString, ps1, pt1, s1, s2, sEmpty, x1, x2, xx)

import Array exposing (Array)
import BLParser.Block as Block exposing (Block)
import BLParser.BlockTree as BlockTree
import BLParser.Edit as Edit
import BLParser.Id as Id
import BLParser.Parse as Parse exposing (ParserState)
import BLParser.Source as Source
import Tree exposing (Tree)
import Tree.Extra


arrayFromString : String -> Array String
arrayFromString str =
    str
        |> String.lines
        |> Array.fromList


{-|

    > blockTreeOfString t1 |> Tree.map Block.stringOf |> HTree.tagWithDepth
    Tree ("",0) [Tree ("\n| section A",1) [Tree ("\nB",2) []],Tree ("\n| section C",1) [Tree ("\nD",2) [Tree ("\n",3) []]]]

-}
blockTreeOfString : String -> Tree Block
blockTreeOfString =
    Source.fromString >> Parse.parseSource Id.initial >> Parse.toTree


parserStateOfString : String -> ParserState
parserStateOfString =
    Source.fromString >> Parse.parseSource Id.initial



--
--ss1 =
--    Edit.separate 5 6 ps1
--
--
--spt =
--    Tuple.first ss1
-- |> Maybe.map (Tree.map Block.stringOf)
--
--prt =
--    Tuple.second ss1 |> Maybe.map (Tree.map Block.stringOf)
--
--
--st1 =
--    Edit.spanningTreeOfSourceRange 5 6 ps1 |> Maybe.map (Tree.map Block.stringOf)


ps1 =
    parserStateOfString x1


s1 =
    Source.fromString x1


pt1 =
    ps1 |> Parse.toTree |> Tree.map Block.stringOf |> Tree.Extra.tagWithDepth


x1 =
    """| section A

| subsection B

C

| subsection D

E

F

| section G
"""


xx =
    """| section A

| subsection B

C

| subsection D

E

F

| section G"""


sEmpty =
    Source.fromString ""


s2 =
    Source.fromString x2


x2 =
    """
| section X"""
