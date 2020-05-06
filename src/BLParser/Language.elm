module BLParser.Language exposing (Language)

import BLParser.Block exposing (Block(..))
import BLParser.Source exposing (Source)


type alias Language blockType =
    { getBlock : Int -> Source -> Block blockType
    , root : Block blockType
    , gte : blockType -> blockType -> Bool
    , blockGte : Block blockType -> Block blockType -> Bool
    }
