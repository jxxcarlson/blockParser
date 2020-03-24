module BlockParser exposing (Block(..), BlockEnd(..), BlockHeading(..), Line(..), LineType(..))

{-|

    The blockParser function reads an Array of lines and produces a tree of Blocks,

-}

import Array exposing (Array)
import Tree


type DocTree
    = Tree Block


type Block
    = Block
        { id : String
        , content : Array String
        , kind : BlockType
        , args : List String
        }


type BlockType
    = Section Int
    | Environment String
    | Math



{-

   Blocks can be either *tight* or *loose*.  A tight
   block is terminated by a blank line.  A loose
   block is terminated by EOF or a line defining
   a block of the same kind.  For example, the block that
   begins with

       | section 1 Intro

   can be terminated by

       | section 2 Atoms

   There is one other way of ending a loose block, illustrated
   by this example:

       | quote (Abraham Lincoln)

       ---
       ---

       .quote
-}


{-| Think of the List String as the arguments of a BlockHeading

    Example: | section 1 Intro
    --> BlockHeading ["section", "1", "Intro"]

-}
type BlockHeading
    = BlockHeading (List String)


type BlockEnd
    = BlockEnd String


type LineType
    = Blank
    | Text
    | BlockHeading
    | BlockEnd


type Line
    = Line
        { lineNumber : Int
        , content : String
        , lineType : LineType
        }
