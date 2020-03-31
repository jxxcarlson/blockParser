module Edit.TreeOps exposing (spanningTree)

import Loop exposing (Step(..), loop)
import Maybe.Extra
import Tree exposing (Tree)
import Tree.Zipper as Zipper exposing (Zipper)


{-| Compute the smallest subtree of the given tree
which contains all the nodes of the nodeList.
-}
spanningTree : List a -> Tree a -> Maybe (Tree a)
spanningTree nodeList tree =
    let
        root =
            List.head nodeList

        nodeList1 =
            List.drop 1 nodeList

        zipper =
            Maybe.map2 setFocus root (Just (Zipper.fromTree tree)) |> Maybe.Extra.join

        subTree =
            Maybe.map Zipper.tree zipper

        nodeList2 : List a
        nodeList2 =
            Maybe.map2 subtractNodesOfTree subTree (Just nodeList1) |> Maybe.withDefault []

        initialState =
            { nodeList = nodeList2
            , tree = tree
            , zipper = zipper
            , root = root
            }
    in
    loop initialState nextState


type alias State a =
    { nodeList : List a
    , zipper : Maybe (Zipper a)
    , tree : Tree a
    , root : Maybe a
    }


nextState : State a -> Step (State a) (Maybe (Tree a))
nextState state =
    case ( state.root, state.nodeList ) of
        ( Nothing, _ ) ->
            Done Nothing

        ( Just root, [] ) ->
            let
                refocusedZipper : Maybe (Zipper a)
                refocusedZipper =
                    Maybe.map2 setFocus (Just root) state.zipper |> Maybe.Extra.join

                output : Maybe (Tree a)
                output =
                    Maybe.map Zipper.tree refocusedZipper
            in
            Done output

        ( Just _, nodeList ) ->
            let
                newZipper =
                    Maybe.andThen Zipper.parent state.zipper

                newRoot =
                    Maybe.map Zipper.label newZipper

                nodeList1 : List a
                nodeList1 =
                    List.drop 1 nodeList

                zipper =
                    Maybe.map2 setFocus newRoot state.zipper |> Maybe.Extra.join

                subTree =
                    Maybe.map Zipper.tree zipper

                nodeList2 : List a
                nodeList2 =
                    Maybe.map2 subtractNodesOfTree subTree (Just nodeList1) |> Maybe.withDefault []
            in
            Loop { state | nodeList = nodeList2, zipper = zipper, root = newRoot }


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


setFocus : a -> Zipper a -> Maybe (Zipper a)
setFocus node zipper =
    Zipper.findFromRoot (\label -> label == node) zipper
