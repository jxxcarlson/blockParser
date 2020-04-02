module BLParser.Edit exposing (deleteRangeOfArray, insertInArray)

import Array exposing (Array)
import BLParser.Parse exposing (ParserState)
import BLParser.Source as Source exposing (Source)


deleteRange : Int -> Int -> ParserState -> ParserState
deleteRange from to parserState =
    parserState
      |>
