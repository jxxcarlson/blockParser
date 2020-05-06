module Language.B.Language exposing (BlockType, lang)

{-| This module defines the language B.
-}

import Language.B.Block as Block
import Language.B.BlockType
import MU.Language exposing (Language)


type alias BlockType =
    Language.B.BlockType.BlockType


lang : Language BlockType
lang =
    { getBlock = Block.get
    , root = Block.root
    , gte = Language.B.BlockType.gte
    , blockGte = Block.gte
    }
