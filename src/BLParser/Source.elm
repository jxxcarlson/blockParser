module BLParser.Source exposing
    ( Source
    , append
    , deleteRange
    , empty
    , fromArray
    , fromList
    , fromString
    , insertBeforeIndex
    , length
    , lineAt
    , merge
    , replaceRange
    , slice
    , toArray
    )

import Array exposing (Array)


type Source
    = Source (Array String)


append : Source -> Source -> Source
append (Source array1) (Source array2) =
    Source (Array.append array1 array2)


merge : Source -> Source -> Source -> Source
merge s1 s2 s3 =
    append (append s1 s2) s3


empty : Source
empty =
    Source Array.empty


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


lineAt : Int -> Source -> Maybe String
lineAt k (Source array) =
    Array.get k array


slice : Int -> Int -> Source -> Source
slice from to (Source array) =
    Source (Array.slice from to array)


{-|

    > a = Source.fromString "A\nB\nC\nD"
    --> Source (Array.fromList ["A","B","C","D"])

    > Source.delete 1 2 a
    --> Source (Array.fromList ["A","D"])

-}
deleteRange : Int -> Int -> Source -> Source
deleteRange from to (Source array) =
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
replaceRange : Int -> Int -> Source -> Source -> Source
replaceRange from to (Source insertion) (Source target) =
    let
        before =
            Array.slice 0 from target

        after =
            Array.slice (to + 1) (Array.length target) target
    in
    Source (Array.append (Array.append before insertion) after)


{-|

    > a = Array.fromList [0, 1, 2, 3, 4, 5, 6]
    Array.fromList [0,1,2,3,4,5,6]

    > deleteRangeOfArray 3 4 a
    Array.fromList [0,1,2,5,6]

-}
deleteRangeOfArray : Int -> Int -> Array a -> Array a
deleteRangeOfArray from to array =
    let
        before =
            Array.slice 0 from array

        after =
            Array.slice (to + 1) (Array.length array) array
    in
    Array.append before after


{-|

    > b = Array.fromList [0, 1, 2, 3]

    > c = Array.fromList [8, 9]

    > insertInArray 2 c b
    Array.fromList [0,1,8,9,2,3]

-}
insertInArrayBeforeIndex : Int -> Array a -> Array a -> Array a
insertInArrayBeforeIndex k insertion target =
    let
        before =
            Array.slice 0 k target

        after =
            Array.slice k (Array.length target) target
    in
    Array.append (Array.append before insertion) after


{-|

    > a = Source.fromString "a\nb\nc\nd"
    Source (Array.fromList ["a","b","c","d"])
        : Source
    > b = Source.fromString "X\nY"
    Source (Array.fromList ["X","Y"])

    > insertBeforeIndex 2 b a
    Source (Array.fromList ["a","b","X","Y","c","d"])

-}
insertBeforeIndex : Int -> Source -> Source -> Source
insertBeforeIndex k (Source insertion) (Source target) =
    Source (insertInArrayBeforeIndex k insertion target)
