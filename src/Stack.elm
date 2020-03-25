module Stack exposing (Stack, empty, init, pop, push, top)


type alias Stack a =
    List a


init : Stack a
init =
    []


empty : Stack a -> Bool
empty stack =
    stack == []


push : a -> Stack a -> Stack a
push item stack =
    item :: stack


pop : Stack a -> ( Maybe a, Stack a )
pop stack =
    ( List.head stack, List.drop 1 stack )


top : Stack a -> Maybe a
top stack =
    List.head stack
