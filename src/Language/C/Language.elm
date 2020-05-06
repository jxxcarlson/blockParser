module Language.C.Language exposing (BlockType, lang)

{-| This module defines the language C.
-}

import Language.C.Block as Block
import Language.C.BlockType
import MU.Language exposing (Language)


type alias BlockType =
    Language.C.BlockType.BlockType


lang : Language BlockType
lang =
    { getBlock = Block.get
    , root = Block.root
    , gte = Language.C.BlockType.gte
    , blockGte = Block.gte
    }
