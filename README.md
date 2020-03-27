# Parsing a Family of Block-structured Markup Languages

Below we describe a family of block-structured
markup languages and a system for parsing
their source text to a tree of blocks.  The system
parses source text to a tree of blocks. Its structure is
 modular, with the block part of
the markup language defined in the 
module `Block` and the parser defined in module
`BlockParser`.  Thus, to parse a different 
language, it suffices to use a different `Block` module.
Note that a complete parser requires an additional step
which transforms blocks, taking into account whatever
inline elements there are in the markup language.  For this one
must define a suitable type `AugmentedBlockData` 
and a function

```elm
parseInline : Block -> AugmentedBlockData
```

The main function in each of the two modules is implemented
as a finite-state machine operating on lines.
As discussed in section 6, the parser is *injective*,
so that the source can be recovered verbatim from the
parse tree.  The implementation language is 
[Elm](https:elm-lang.org), a statically typed
language of pure functions.




## Contents

1. BlockParser 
2. Recognizing Blocks
3. Arranging the blocks in a tree
4. The partial order
5. Finite State Machines
6. Annotated String Arrays and Source Maps
7. Injectivity
8. Interactive use (Elm repl)
9. Tests and Benchmarks

## 1 BlockParser

The function `parseStringArray` transforms an array of lines of 
source text for a block markup
language into a tree of blocks.  There are three
kinds of blocks.

- Ordinary paragraphs.  These consist of contiguous
  non-blank lines with a blank line before and after 

- Tight blocks.  These are like ordinary paragraphs, 
  but where the first line, the *block header,* has
  a special meaning. Here is an example:
  
  ```text
  | math
  a^2 + b^2 = c^2
  ```
  
  The pipe character signals the beginning of the tight
  block.  Everything after this "signal" string is regarded
  as an argument or argument list to the block header.  In this
  case, the argument list is empty.  The remaining lines
  in the block constitute the *body* of the block.  One 
  can configure things so that leading space before the 
  signal string is significant, e.g., defines a "level,"
  as in Markup.
  This is accomplished in the implementation of the 
  `classify` function described below.
  
- Loose blocks.  These may consist of more than one paragraph.
  The body of a loose block is terminated when (i) a block header 
  of higher level is scanned by the parser, or (ii)
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

In the examples above, blocks are signaled by the
pipe character.  However, any leading string
can in principle could be used, as can be 
a mix of such.  Thus Markdown-style blocks such 
as the below can be accommodated.

```text

# Section 
## Subsection 
- List element 
    - Next level list element

```

## 2. Recognizing Blocks

Blocks are recognized by the function

```elm
Block.get : Int -> Array String -> Block
```
The first argument is the line number at which
to beginning scanning for a valid block in the
array given in the second argument, where the
return type is defined as follows:

```elm
type alias Block =
    { blockStart : Int -- index in source array
    , blockEnd : Int -- index in source array
    , array : Array String -- the body of the block, from a slice of the source array
    , id : Maybe Id
    , blockType : BlockType  -- derived from the block header
    }
```
The `Id` is a tuple `(Int, Int)`, which should 
be thought of as `(version, blockId)`, where the first
component is given to the parser to conrol edits efficiently,
and where the second is a unique identifier for the block.
`BlockType` depends on the definition of the 
markup language.   Below is the type used for 
a language with source text which looks like this:

```text
| section Intro

Let's talk about matter 

| subsection Atoms 

The are tiny!

| subsubsection Hydrogen

It has one proton.

| subsubsection Helium

It has two protons.
People use it to fill baloons
at birthday parties.


| section Quantum Mechanics

We need QM to understand atoms. Here is the
Heisenberg Uncertainty Principle:

| math 
[ \\hat x, \\hat p ] = i \\hbar
```

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

## 3. Arranging the blocks in a tree

The function `BlockParser.parseStringArray` takes an
array of strings
as argument and produces a `Tree Block` as output.
To do this, one sets up a  `Stack BlockType` and 
a `Tree.Zipper Block`, where the latter holds the 
tree that is being built up.  New blocks, which are 
obtained by `Block.get` are always added as a child
of a node in the right-most subtree, which we shall
call the *spine* of the tree.  The spine
is a connected tree where each node has
at most one child. The stack is 
a representation of the spine.  When
a new block is acquired by `Block.get`, its insertion
point is the spine is determined by examining the
stack. Blocks are subject to a partial order defined
on their BlockTypes. A new block is inserted as the
child of the smallest node "in" the stack which is 
greater than the new block.  Two operations, 
`push` and `pop`, operate jointly on the stack + 
zipper structure to carry out insertion of a new block.  The `pop`
operation is used when necessary to bring the 
correct node into the focus of the zipper.  The 
`push` operation does the actual insertion.  At then
end of a `push`, the block type of the new 
node is on the top of the stack and the new node
is the focus of the zipper. A `pop` operation removes the top 
of the stack and moves the focus of the zipper to its parent
 so that its
block type is on the top of the stack.

## 4. The partial order

As noted above, the manner in which blocks are arranged
in a tree depends on a partial order of block types.
In the type described above, `None` is the least element
and `Document` is the greatest. Any `Section` (section, subsection,
etc.) dominates
`Paragraph`, `Math`, `Quotation`, and `Envirnoment`, while
the latter not comparable.  Different choices of partial
order give different results: the same blocks, but 
arranged in a different tree.

## 5. Finite State Machines

Both `Block.get` and `BlockParser.parserStringArray` are
implemented as finite-state machines using the general
construct 

```elm
type Step state a
    = Loop state
    | Done a


loop : state -> (state -> Step state a) -> a
loop s nextState =
    case nextState s of
        Loop s_ ->
            loop s_ nextState

        Done b ->
            b
```
In the case of `Block.get`, the type of the state is
given by


```elm
type alias BlockState =
    { currentLineNumber : Int
    , array : Array String
    , blockStart : Int
    , blockEnd : Int
    , arrayLength : Int
    , scanning : BlockScanState
    , blockType : BlockType
    , blockKind : BlockKind
    }
```

In the case of `BlockParser.parserStringArray`, it is
given by 

```elm
type alias ParserState =
    { bzs : BlockZipperState
    , array : Array String
    , cursor : Int
    , arrayLength : Int
    , counter : Int
    , id : Maybe Id
    }
```

These machines operate on the level of lines rather
than the level of characters.  The 
function `parseString` is implmented as 
`parseStringWithVersion 0`, where

**Note.** 

````elm
parseStringWithVersion : Int -> String -> Tree Block
````

permits one to version `Id`, something that is needed
in implmenting interactivef editors.


## 6. Annotated String Arrays and Source Maps

To make effective use of a markup language parser
in a rich editor or IDE, one needs a way of relating
lines of source text to nodes in the parse tree and 
*vice versa.*   To this end, use
 
```elm
sourceMapFromTree : Tree Block -> Array (Maybe Id)
```
to compute 

```elm
sourceMap : Array (Maybe Id)
```

The sourceMap has the property that the line at 
index `k` in the source is a line of the block with `id = sourceMap[k]`.
We can look up the block corresponding to a given line using

```elm
getNodeAtLine : Array (Maybe Id) -> Int -> Tree Block -> Maybe Block
```

To go in the other direction, we can find the beginning and ending indices
of the source array corresponding to a given `Id` using

```elm
getArraySegment : Id -> Tree Block -> Maybe ( Int, Int )
```

For more info, one can use

```elm
getNode : Id -> Tree Block -> Maybe Block
``` 

## 7. Injectivity

An *injective* parser 

```elm
parseStringArray : Array String -> Tree Block
```

is one for which there is a left inverse

```elm
toStringArray: Tree Block -> Array String
```

That is, 

```elm
toStringArray << parseStringArray = identity
```
where `<<` is Elm's composition operator and
`identity` means the identity on `Array String`

Injective parsers have the nice property that the 
array of source text can be recovered from the parse tree.
To test injectivty, define 

```elm
isInjective : String -> Bool
isInjective str =
    let
        array =
            Block.arrayFromString str

        qIdentity =
            toStringArray << parseStringArray

        array2 =
            qIdentity array
    in
    array2 == array
```

Then one has, for example

```text
> List.map isInjective [text1, text2, text3, text4]
[True,True,True,True]
```

## 8. Interactive use (Elm repl)

Good for experimenting ...

```elm
$ elm repl
> import BlockParser exposing(..)
> import Text exposing(..)
> t2 -- See what this text looks like
> parseString t2 -- Parse it. Lot's of stuff
> parseString t2 |> toStringTree -- easier to read
> parseString t2 |> toTaggedStringTree -- gives depth info
> parseString text4 |> toBlockTypeTree  -- Return a tree representing (BlockType, depth of node)
```

## 9. Tests and Benchmarks

There is  small test suite in `./tests`.  The 
results in `./benchmakrs` are as follows:

```text
   text4:    3568 runs/sec @98%     : 0.3 ms/run
   text4X10: 344 runs/sec @99.83%   : 3 ms/run

   text4 has 33 lines               : 9.0 microseconds/line
   text4X10 has 339 lines           : 8.7 microseconds/line
```