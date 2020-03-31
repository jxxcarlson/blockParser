module Edit.Source exposing
    ( Source
    , delete
    , fromArray
    , fromList
    , fromString
    , insert
    , length
    , toArray
    )

import Array exposing (Array)


type Source
    = Source (Array String)


fromArray : Array String -> Source
fromArray array =
    Source array


fromList : List String -> Source
fromList list =
    Source (Array.fromList list)


fromString : String -> Source
fromString str =
    str |> String.lines |> Array.fromList |> Source


toArray : Source -> Array String
toArray (Source array) =
    array


length : Source -> Int
length (Source array) =
    Array.length array


{-|

    > a = Source.fromString "A\nB\nC\nD"
    --> Source (Array.fromList ["A","B","C","D"])

    > Source.delete 1 2 a
    --> Source (Array.fromList ["A","D"])

-}
delete : Int -> Int -> Source -> Source
delete from to (Source array) =
    let
        before =
            Array.slice 0 from array

        after =
            Array.slice (to + 1) (Array.length array) array
    in
    Source (Array.append before after)


{-|

    > a = Source.fromString "A\nB\nC\nD"
    --> Source (Array.fromList ["A","B","C","D"])

    > b = Source.fromString "X\nY\nZ"
    Source (Array.fromList ["X","Y","Z"])

    > Source.insert 1 2 b a
    Source (Array.fromList ["A","X","Y","Z","D"])

-}
insert : Int -> Int -> Source -> Source -> Source
insert from to (Source insertion) (Source target) =
    let
        before =
            Array.slice 0 from target

        after =
            Array.slice (to + 1) (Array.length target) target
    in
    Source (Array.append (Array.append before insertion) after)
