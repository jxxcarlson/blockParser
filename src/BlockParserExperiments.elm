module BlockParserExperiments exposing
    ( State
    , ap
    , appendTreeToFocus
    , initState
    , lab
    , lc
    , par
    , tt
    )

{-|

    The blockParser function reads an Array of lines and produces a tree of Blocks,

-}

import Array exposing (Array)
import Block exposing (Block)
import Parser.Advanced
import Stack exposing (Stack)
import Tree exposing (Tree)
import Tree.Zipper as Zipper exposing (Zipper)


type alias State =
    { zipper : Zipper Int, stack : Stack Int }


s =
    Tree.singleton


t =
    Tree.tree


at =
    appendTreeToFocus


initState : Int -> State
initState k =
    { zipper = Zipper.fromTree (s k), stack = Stack.init |> Stack.push k }


ap : Int -> State -> State
ap k state =
    { state | zipper = at (s k) state.zipper }


par : State -> State
par state =
    case Zipper.parent state.zipper of
        Nothing ->
            state

        Just z ->
            let
                newStack =
                    state.stack |> Stack.pop |> Tuple.second
            in
            { state | stack = newStack, zipper = z }


lab : State -> Int
lab state =
    Zipper.label state.zipper


lc : State -> State
lc state =
    case Zipper.lastChild state.zipper of
        Nothing ->
            state

        Just z ->
            { state | stack = Stack.push (Zipper.label z) state.stack, zipper = z }


tt : State -> Tree Int
tt state =
    state.zipper |> Zipper.toTree



--
--addTreeToFocus : Tree a -> Zipper a -> Zipper a
--addTreeToFocus t z =
--    let
--        newTree =
--            Tree.appendChild t (Tree.singleton (Zipper.label z))
--    in
--    Zipper.replaceTree newTree z


appendTreeToFocus : Tree a -> Zipper a -> Zipper a
appendTreeToFocus t_ z =
    let
        newTree =
            Tree.appendChild t_ (Zipper.tree z)
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
