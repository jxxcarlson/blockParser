module ParserTest exposing (isInjective)

import Block
import BlockTree
import Convenience


isInjective : String -> Bool
isInjective str =
    let
        array =
            Block.arrayFromString str

        qIdentity =
            BlockTree.toStringArray << Convenience.parseStringArrayWithVersion 0

        array2 =
            qIdentity array
    in
    array2 == array
