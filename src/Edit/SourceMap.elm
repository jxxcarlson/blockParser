module Edit.SourceMap exposing (SourceMap, empty)

import Array exposing (Array)
import Edit.Id as Id exposing (Id)
import Edit.Source as Source exposing (Source)


type SourceMap
    = SourceMap (Array (Maybe Id))


empty : Source -> SourceMap
empty source =
    List.repeat (Source.length source) Nothing
        |> Array.fromList
        |> SourceMap
