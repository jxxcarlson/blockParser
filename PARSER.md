# An Iterative Parser

This document describes the design of an incremental parser for the block-structured
markup languages discussed in the README.  By *incremental*, we mean that if 
if part of the source text is changed one does not have to re-parse the entire document
to obtain a valid parse tree.  Such a capability makes possible interactive editing
of documents which which are parsed, then rendered in real time. The idea is as follows.
The source text is given as an array of lines, that is, strings.  A first parsing of the
document produces an AST where the nodes have the following structure:

```elm
type Block
    = Block
        { id : Maybe Id
        , blockType : BlockType
        , blockStart : Int
        , blockEnd : Int
        , source : Source
        }
```
