module Edit.Main exposing (..)
import Array exposing Array
import Tree exposing(Tree)
import Edit.Block exposing(Block, Id, BlockType)


type Source = Source (Array String)

type ParseTree = Tree Block

type SourceMap = SourceMap (Array Id)
