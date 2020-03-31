module Edit.Tree exposing (State, listDifference, subtractNodesOfTree)

import Edit.Block exposing (Block, equal)
import Loop exposing (Step(..), loop)
import Tree exposing (Tree)
import Tree.Zipper as Zipper exposing (Zipper)


spanningTree : List Block -> Tree Block -> Tree Block
spanningTree nodeList tree =
    let
        initialState =
            { nodeList = nodeList, tree = tree, zipper = Zipper.fromTree tree }
    in
    loop initialState nextState


type alias State a =
    { nodeList : List a
    , zipper : Zipper a
    , tree : Tree a
    }


nextState : State Block -> Step (State Block) (Tree Block)
nextState state =
    case List.head state.nodeList of
        Nothing ->
            Done (state.zipper |> Zipper.toTree)

        Just node ->
            case setFocus node state.zipper of
                Nothing ->
                    Done (state.zipper |> Zipper.toTree)

                Just refocusedZipper ->
                    let
                        subTree =
                            Zipper.tree refocusedZipper

                        nodeList1 =
                            state.nodeList
                                |> List.drop 1

                        nodeList2 =
                            subtractNodesOfTree subTree nodeList1

                        newZipper =
                            case nodeList2 of
                                [] ->
                                    refocusedZipper

                                _ ->
                                    Zipper.parent refocusedZipper
                    in
                    Loop { state | nodeList = nodeList2, zipper = newZipper }


{-|

    > s = Tree.singleton
    > t = Tree.tree

    > a = t 1 [ t 2 [t 3 [ s 4]]]
    -->  Tree 1 [Tree 2 [Tree 3 [Tree 4 []]]]

    > subtractNodesOfTree a [3, 4, 8, 9]
    [8,9] : List number

-}
subtractNodesOfTree : Tree a -> List a -> List a
subtractNodesOfTree tree nodeList =
    tree
        |> Tree.flatten
        |> listDifference nodeList


listDifference : List a -> List a -> List a
listDifference list1 list2 =
    let
        folder : List a -> a -> List a -> List a
        folder list1_ item list2_ =
            if not <| List.member item list1_ then
                item :: list2_

            else
                list2_
    in
    List.foldl (folder list2) [] list1
        |> List.reverse


setFocus : Block -> Zipper Block -> Maybe (Zipper Block)
setFocus node zipper =
    Zipper.findFromRoot (\label -> equal label node) zipper
