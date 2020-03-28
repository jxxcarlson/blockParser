module ParserTest exposing (isInjective)

import Block
import BlockParser
import BlockTree


isInjective : String -> Bool
isInjective str =
    let
        array =
            Block.arrayFromString str

        qIdentity =
            BlockTree.toStringArray << BlockParser.parseStringArrayWithVersion 0

        array2 =
            qIdentity array
    in
    array2 == array
