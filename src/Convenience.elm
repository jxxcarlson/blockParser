module Convenience exposing (parseString, parseStringArrayWithVersion, parseStringWithVersion)

import Array exposing (Array)
import Block exposing (Block)
import BlockParser
import Tree exposing (Tree)



-- CONVENIENCE PARSER FUNCTIONS


parseString : String -> Tree Block
parseString str =
    parseStringWithVersion 0 str


parseStringWithVersion : Int -> String -> Tree Block
parseStringWithVersion version str =
    let
        array =
            Block.arrayFromString str
    in
    parseStringArrayWithVersion version array


parseStringArrayWithVersion : Int -> Array String -> Tree Block
parseStringArrayWithVersion version array =
    BlockParser.parse (BlockParser.initParserState version array) |> BlockParser.toTree
