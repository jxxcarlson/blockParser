module Language.C.Language exposing (languageC)

import BLParser.Language exposing (Language)
import Language.C.Block as Block
import Language.C.BlockType as BlockType exposing (BlockType)


languageC : Language BlockType
languageC =
    { getBlock = Block.get
    , root = Block.root
    , gte = BlockType.gte
    , blockGte = Block.gte
    }
