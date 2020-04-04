# An Iterative Parser

This document describes the design of an incremental parser for the block-structured
markup languages discussed in the README.  By *incremental*, we mean that if 
when the source text is edited, one does not have to re-parse the entire document
to obtain a valid parse tree.  Such a capability makes possible interactive editing
of documents which which are parsed, then rendered in real time. The idea is as follows.
The source text is given as an array of lines.  A first parsing of the
document produces an abstract syntax tree (AST) where the nodes are of type `Block`:

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
Here `Source` is basically and array of strings.  Each block has a uniqe `Id` which serves
to identify it in the parse tree.  When the text is parsed, various other structures
are computed, among which is a `sourceMap`, which is an array of `Id`.  The source map provides
a mapping from the source to the AST.  It also gives a skeletal view of the source, eg., 

```
sourceMap:
0 A
1 A
2 B
3 B
4 B
5 C
etc.
```

Thus lines 0 and 1 belong to the node with id `A`, while lines 2, 3, and 4 belong to the node with id `B`, 
etc. Below is a slightly more detailed skeletal view.  In the first column is the line number.  It is followed
by the identifier of the block to which it belongs and shorthand for the block type: s for section, ss for subsection,
and t for text.  

```
0 A s
1 A s
2 B ss
3 B ss
4 C t
5 C t
6 D ss
7 D ss
8 E t
9 E t
10 F t
11 F t
12 G s
13 G 2
```

The parse tree for this text is as illustrated below.

<img src="parseTree1.jpg" alt="drawing" width="200" />

## Edits

We take simplified view of edits, which are assumed to operate on lines. 
Lines can be deleted, replaced, or inserted, and these changes can 
be contiguous or not.  Normal human editing is carried on by a sequence 
of contiguous edits.  A search-and-replace operation can result in
non-contguous edits.  Despite these apparent complexities, the editing
model is quite simple.  While we describe it in the contiguous case,
it holds general.   Thus changes to the text occur in a region of
of the source, as illustrated below.  The changed text may lie
entirely within a node, or it may span several.  Expand this region
if necessary so that if part of a node lies in it, then the full node
lies in it. Call this region U.  In the example below, the region where the change occours
extends to the text corresponding to node D.  This text is replaced
by as new text which fills a node X.

The editing algorithm operates as follows.  Let T be the 
source text.

1. Find the spanning tree of the region where the change occurs.

2. Let T' = T - { spanning tree}

2. Let V be the text corresponding to the spanning tree.  It
   consists of two parts, U, whih is defined above, and W = V - U.
   The new source text is T' is T with U cut out and replaced by W.
   If W is empty, this operation is a pure deletion.
   
3. Parse the text  XW.  Let S be the corresponding ast.
   Attach S to T as the least  node greater than the root
   of S in the given partial order.

<img src="parse2.jpg" alt="drawing" width="300" />

Tree surgery:

<img src="parse3.jpg" alt="drawing" width="450" />


