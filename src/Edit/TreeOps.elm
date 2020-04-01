module Edit.TreeOps exposing
    ( attach
    , findAttachmentNode
    , removeSubTree
    , spanningTree
    )

import Loop exposing (Step(..), loop)
import Maybe.Extra
import Tree exposing (Tree)
import Tree.Zipper as Zipper exposing (Zipper)


{-|

    > c = t 1 [ t 2 [ s 3, t 4 [s 5, s 6]]]
    Tree 1 [Tree 2 [Tree 3 [],Tree 4 [Tree 5 [],Tree 6 []]]]

    > d = t 3 [ s 4, s 5]
    Tree 3 [Tree 4 [],Tree 5 []]

    > attach (<) 6 d c
    subTreeRoot: 3
    attachmentNode: 2
    Just (Tree 1 [Tree 2 [Tree 3 [],Tree 4 [Tree 5 [],Tree 6 []],Tree 3 [Tree 4 [],Tree 5 []]]])

-}
attach : (a -> a -> Bool) -> a -> Tree a -> Tree a -> Maybe (Tree a)
attach gte targetNode subTree tree =
    let
        subTreeRoot =
            Zipper.fromTree subTree
                |> Zipper.root
                |> Zipper.label
                |> Debug.log "subTreeRoot"
    in
    case findAttachmentNode gte subTreeRoot targetNode tree of
        Nothing ->
            Nothing

        Just attachmentNode ->
            let
                _ =
                    Debug.log "attachmentNode" attachmentNode

                zipper =
                    Zipper.fromTree tree |> setFocus attachmentNode

                zipper2 =
                    Maybe.map2 appendTreeToFocus (Just subTree) zipper
            in
            Maybe.map Zipper.toTree zipper2


appendTreeToFocus : Tree a -> Zipper a -> Zipper a
appendTreeToFocus t_ z =
    let
        newTree =
            Tree.appendChild t_ (Zipper.tree z)
    in
    Zipper.replaceTree newTree z


{-|

    > c = t 1 [ t 2 [ s 3, t 4 [s 5, s 6]]]
    Tree 1 [Tree 2 [Tree 3 [],Tree 4 [Tree 5 [],Tree 6 []]]]

    > findAttachmentNode (<) 3  6 c
    Just 2 : Maybe number

-}
findAttachmentNode : (a -> a -> Bool) -> a -> a -> Tree a -> Maybe a
findAttachmentNode gte_ node targetNode tree =
    let
        zipper =
            Zipper.fromTree tree |> setFocus targetNode

        gte a b =
            case b of
                Nothing ->
                    False

                Just b_ ->
                    gte_ a b_

        initialState =
            { zipper = zipper, target = Just targetNode, node = node, gte = gte }
    in
    loop initialState nextATState


type alias ATState a =
    { zipper : Maybe (Zipper a)
    , target : Maybe a
    , node : a
    , gte : a -> Maybe a -> Bool
    }


nextATState : ATState a -> Step (ATState a) (Maybe a)
nextATState state =
    case state.gte state.node state.target of
        False ->
            Done state.target

        True ->
            let
                newZipper =
                    Maybe.andThen Zipper.parent state.zipper

                newTarget =
                    Maybe.map Zipper.label newZipper
            in
            Loop { state | zipper = newZipper, target = newTarget }


{-|

    > c = t 1 [ t 2 [ s 3, t 4 [s 5, s 6]]]
    Tree 1 [Tree 2 [Tree 3 [],Tree 4 [Tree 5 [],Tree 6 []]]]

    > removeSubTree 3 c |> Maybe.andThen (removeSubTree 5)
    Just (Tree 1 [Tree 2 [Tree 4 [Tree 6 []]]])

-}
removeSubTree : a -> Tree a -> Maybe (Tree a)
removeSubTree a tree =
    let
        zipper =
            Zipper.fromTree tree
                |> setFocus a
    in
    Maybe.map Zipper.removeTree zipper
        |> Maybe.Extra.join
        |> Maybe.map Zipper.toTree


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
    loop initialState nextSPState


type alias SPState a =
    { nodeList : List a
    , zipper : Maybe (Zipper a)
    , tree : Tree a
    , root : Maybe a
    }


nextSPState : SPState a -> Step (SPState a) (Maybe (Tree a))
nextSPState state =
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
