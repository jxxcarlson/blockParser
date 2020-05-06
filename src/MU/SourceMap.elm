module MU.SourceMap exposing
    ( SourceMap
    , empty
    , fromTree
    , idList
    , lineAt
    , range
    )

import Array exposing (Array)
import List.Extra
import MU.Block as Block exposing (Block(..))
import MU.Id as Id exposing (Id)
import MU.Source as Source exposing (Source)
import Maybe.Extra
import Tree exposing (Tree)


type SourceMap
    = SourceMap (Array (Maybe Id))


lineAt : Int -> SourceMap -> Maybe Id
lineAt k (SourceMap array) =
    Array.get k array |> Maybe.Extra.join


range : Int -> Int -> SourceMap -> SourceMap
range from to (SourceMap array) =
    SourceMap (Array.slice from to array)


{-| Given a sourceMap, return a list
of unique Ids.
-}
idList : SourceMap -> List Id
idList (SourceMap array) =
    array
        |> Array.toList
        |> Maybe.Extra.values
        |> List.Extra.uniqueBy Id.stringValue


empty : Source -> SourceMap
empty source =
    List.repeat (Source.length source) Nothing
        |> Array.fromList
        |> SourceMap


fromTree : Tree (Block a) -> SourceMap
fromTree tree =
    let
        annotateLines : Block a -> Array (Maybe Id)
        annotateLines b =
            let
                id =
                    Block.idOf b
            in
            Array.map (\line -> id) (Block.arrayOf b)
    in
    Tree.foldl (\bd acc -> Array.append acc (annotateLines bd)) Array.empty tree
        |> SourceMap
