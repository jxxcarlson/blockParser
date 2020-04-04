module ZTree exposing (moveSubTree, s, setFocus, t, t1, t2, z1, z2)

import Tree exposing (Tree)
import Tree.Zipper as Zipper exposing (Zipper)


s =
    Tree.singleton


t =
    Tree.tree


t1 : Tree Int
t1 =
    t 0 [ s 1, s 2 ]


z1 =
    Zipper.fromTree t1


t2 =
    t 0 [ t 1 [ s 2 ], s 3 ]


z2 =
    Zipper.fromTree t2


setFocus : Int -> Zipper Int -> Maybe (Zipper Int)
setFocus k zipper =
    Zipper.findFromRoot (\label -> label == k) zipper


moveSubTree : Int -> Int -> Zipper Int -> Maybe (Zipper Int)
moveSubTree from to zipper =
    let
        refocusedZipper =
            setFocus from zipper

        subTree =
            refocusedZipper
                |> Maybe.map Zipper.tree

        prunedZipper =
            refocusedZipper
                |> Maybe.andThen Zipper.removeTree
                |> Maybe.andThen (setFocus to)
    in
    Maybe.map2 appendTreeToFocus subTree prunedZipper


appendTreeToFocus : Tree a -> Zipper a -> Zipper a
appendTreeToFocus t_ z =
    let
        newTree =
            Tree.appendChild t_ (Zipper.tree z)
    in
    Zipper.replaceTree newTree z
