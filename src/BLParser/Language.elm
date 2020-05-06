module BLParser.Language exposing (Language)

import BLParser.Source exposing (Source)
import Language.Block exposing (Block(..))


type alias Language blockType =
    { getBlock : Int -> Source -> Block blockType
    , root : Block blockType
    , gte : blockType -> blockType -> Bool
    , blockGte : Block blockType -> Block blockType -> Bool
    }
