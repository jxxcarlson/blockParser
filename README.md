# BlockParser

BlockParser transforms source text for the SM markup
language into a tree of blocks.  There are three
kinds of blocks.

- Ordinary paragraphs.  These consist of contiguous
  lines with a blank line before and after 

- Tight blocks.  These are like ordinary paragraphs, 
  but where the first line, the *block header,* has
  a special meaning. Here is an example:
  
  ```text
  | math
  a^2 + b^2 = c^2
  ```
  
  The pipe character signals the beginning of the tight
  block.  Everything after the "signal" string is regarded
  as an argument or argument list to the block.  In this
  case, the argument list is empty.  The remaining lines
  in the block constitute the *body* of the block.  One 
  can configure SM so that leading space before the 
  signal string is significant, e.g., defines a "level."
  This is accomplished in the implementation of the 
  `classify` function described below.
  
- Loose blocks.  These may consist of more than one paragraph.
  The body of a loose block is terminated when (a) a block header 
  of higher level is scanned by the parser, or (b)
  a block terminator is encountered.  In the example
  below, the block terminator would be `.quotation`, 
  where this word begins in column 1.  Block level
  is a partial order on block types, to be discussed below.
  
  ```text
  | quotation (Abraham Lincoln)
  
  Four score and seven years ago ...
  
  Now we are engaged in a great civil war ...
  
  But, in a larger sense
  
  | subsection Comments
  ``` 
  
## Recognizing Blocks

Blocks are recognized by the function

```elm
Block.get : Int -> Array String -> BlockData
```
The first argument is the line number at which
to beginning scanning for a valid block in the
array given in the second argument, where the
return type is defined as follows:

```elm
type alias BlockData =
    { blockStart : Int -- index in source array
    , blockEnd : Int -- index in source array
    , array : Array String -- slice of source array
    , blockType : BlockType
    }
```
`BlockType` depends on the implementation of SM.  Here
is a simple example:

```elm
type BlockType
    = None
    | Paragraph
    | Section Int (List String)
    | Math
    | Quotation (List String)
    | Environment (List String)
    | Document
```

The operation of `Block.get` is controlled by 
the function
  
```elm
classify : Int -> String -> Line
```  

where
  
```elm
type alias Line =
    { lineNumber : Int
    , content : String
    , lineType : LineType
    }
```
  

and 

```elm
type LineType
    = Blank
    | Text
    | BlockHeading (List String)
    | BlockEnd String
```

## Arranging the blocks in a tree

The function `BlockParser.parser` takes a string
as argument and produces a `Tree BlockData` as output.
To do this, one sets up a stack of `BlockType` and 
a zipper of `BlocData`, where the latter holds the 
tree that is being built up.  New blocks, which are 
obtained by `Block.get` are always added as a child
of a node in the right-most subtree, which we shall
call the *spine* of the tree.  The stack is 
a representation of a segment of the spine.  When
a new block is acquired by `Block.get`, its insertion
point is the spine is determined by examining the
stack. Blocks are subject to a partial order defined
on their BlockTypes. A new block is inserted as the
child of smallest node "in" the stack which is 
greater than the new block.  Two operations, 
`push` and `pop` operate jointly on the stack + 
zipper structure to carry out the insertion.  The `pop`
operation is used when necessary to bring the 
correct node into the focus of the zipper.  The 
`push` operation does the actual insertion.  As the
terminology indicates, these operations have the 
conventional means insofar as they affect the stack.

## The partial order

