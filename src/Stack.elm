module Stack exposing (Stack, empty, init, pop, push)


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


pop : Stack a -> Maybe a
pop stack =
    List.head stack
