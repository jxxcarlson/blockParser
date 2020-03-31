module Edit.SourceMap exposing (SourceMap, empty, fromTree)

import Array exposing (Array)
import Edit.Block as Block exposing (Block)
import Edit.Id as Id exposing (Id)
import Edit.Source as Source exposing (Source)
import Tree exposing (Tree)


type SourceMap
    = SourceMap (Array (Maybe Id))


empty : Source -> SourceMap
empty source =
    List.repeat (Source.length source) Nothing
        |> Array.fromList
        |> SourceMap


fromTree : Tree Block -> SourceMap
fromTree tree =
    let
        annotateLines : Block -> Array (Maybe Id)
        annotateLines b =
            let
                id =
                    Block.idOf b
            in
            Array.map (\line -> id) (Block.arrayOf b)
    in
    Tree.foldl (\bd acc -> Array.append acc (annotateLines bd)) Array.empty tree
        |> SourceMap
