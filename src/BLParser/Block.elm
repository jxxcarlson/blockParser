module BLParser.Block exposing (..)

import Array exposing (Array)
import BLParser.Id as Id exposing (Id)
import BLParser.Source as Source exposing (Source)
import Stack exposing (Stack)
import Tree.Zipper as Zipper exposing (Zipper)


type Block a
    = Block
        { id : Maybe Id
        , blockType : a
        , blockStart : Int
        , blockEnd : Int
        , source : Source
        }


type BlockKind
    = Tight String
    | Loose String
    | Unclassified


type BlockScanState
    = BeginScan
    | InTightBlock Int
    | InLooseBlock Int
    | InParagraph Int
    | EndScan


type alias BlockState a =
    { currentLineNumber : Int
    , array : Array String
    , blockStart : Int
    , blockEnd : Int
    , arrayLength : Int
    , scanning : BlockScanState
    , blockType : a
    , blockKind : BlockKind
    , counter : Int
    }


type alias BlockZipperState a =
    { zipper : Zipper (Block a), stack : Stack a }


type alias Position =
    { line : Int, column : Int }



-- HELPERS


equal : Block a -> Block a -> Bool
equal a_ b_ =
    idOf a_ == idOf b_


arrayOf : Block a -> Array String
arrayOf (Block data) =
    Source.toArray data.source


stringOf : Block a -> String
stringOf (Block data) =
    Source.toArray data.source
        |> Array.toList
        |> String.join "\n"


blockStart : Block a -> Int
blockStart (Block data) =
    data.blockStart


blockEnd : Block a -> Int
blockEnd (Block data) =
    data.blockEnd


typeOf : Block a -> a
typeOf (Block data) =
    data.blockType


idOf : Block a -> Maybe Id
idOf (Block data) =
    data.id


setId : Maybe Id -> Block a -> Block a
setId id (Block data) =
    Block { data | id = id }
