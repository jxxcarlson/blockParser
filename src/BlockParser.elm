module BlockParser exposing (Block(..), Line(..), addTreeToFocus, appendTreeToFocus, classify)

{-|

    The blockParser function reads an Array of lines and produces a tree of Blocks,

-}

import Array exposing (Array)
import Parser.Advanced
import Stack exposing (Stack)
import Tree exposing (Tree)
import Tree.Zipper as Zipper exposing (Zipper)


addTreeToFocus : Tree a -> Zipper a -> Zipper a
addTreeToFocus t z =
    let
        newTree =
            Tree.appendChild t (Tree.singleton (Zipper.label z))
    in
    Zipper.replaceTree newTree z


appendTreeToFocus : Tree a -> Zipper a -> Zipper a
appendTreeToFocus t z =
    let
        newTree =
            Tree.appendChild t (Zipper.tree z)
    in
    Zipper.replaceTree newTree z


type DocTree
    = Tree Block



{-

   Blocks can be either *tight* or *loose*.  A tight
   block is terminated by a blank line.  A loose
   block is terminated by EOF or a line defining
   a block of the same kind.  For example, the block that
   begins with

       | section 1 Intro

   can be terminated by

       | section 2 Atoms

   There is one other way of ending a loose block, illustrated
   by this example:

       | quote (Abraham Lincoln)

       ---
       ---

       .quote
-}
