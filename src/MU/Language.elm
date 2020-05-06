module MU.Language exposing (Language)

import MU.Block exposing (Block(..))
import MU.Source exposing (Source)


type alias Language blockType =
    { getBlock : Int -> Source -> Block blockType
    , root : Block blockType
    , gte : blockType -> blockType -> Bool
    , blockGte : Block blockType -> Block blockType -> Bool
    }
