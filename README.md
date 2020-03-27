# Parsing a Family of Block Languages

Below we describe a family of block-structured
markup languages and a system for parsing
source text to a tree of block data.  The system
is modular, with block language defined in the 
module `Block` and the parser defined in module
`BlockParser`.  Thus, to parse a different 
language, just use a different `Block` module.


## BlockParser

BlockParser transforms an array of lines of 
source text for a block markup
language into a tree of blocks.  There are three
kinds of blocks in such a language.

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
  can configure things so that leading space before the 
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

In the examples above, blocks are signaled by the
piper character.  However, any leading string
can in principle could be used, as can be 
a mix of such.  Thus Markdown-style blocks such 
as the below are legitimate.

```text

# Section 
## Subsection 
- List element 
    - Next level list element

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
`BlockType` depends on the definition of the 
markup language.  Below is the type used for 
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

As noted above, the manner in which blocks are arranged
in a tree depends on a partial order of block types.
In the type describe above, `None` is the least element
and `Docoument` is the greatest. Any `Section` dominates
`Paragraph`, `Math`, `Quotation`, and `Envirnoment`, while
the latter not comparable.  Different choices of partial
order give different results: the same blocks, but 
arranged in a different tree.

## Interactive use (Elm repl)

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

## Injectivity

An *injective* parser 

```elm
parseStringArray : Array String -> Tree BlockData
```

is one for which there is a left inverse

```elm
toStringArray: Tree BlockData -> Array String
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
