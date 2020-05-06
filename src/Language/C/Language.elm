module Language.C.Language exposing (BlockType, lang)

import BLParser.Language exposing (Language)
import Language.C.Block as Block
import Language.C.BlockType


type alias BlockType =
    Language.C.BlockType.BlockType


lang : Language BlockType
lang =
    { getBlock = Block.get
    , root = Block.root
    , gte = Language.C.BlockType.gte
    , blockGte = Block.gte
    }
