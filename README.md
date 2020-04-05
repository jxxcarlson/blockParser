# The BlockParser Project


The aim of this project is to develop a modular
parser for a family of block-structured
markup languages. e.g., Markdown, or the language
illustrated below. Such 
languages are parsed by a pair of functions 

```elm
    blockParse : Array String -> Tree Block
```

and 

```elm
   inlineLineParse : Block -> AugmentedBlock
```

The first discovers the block structure of the language,
while the second handles inline elments such as
bold, italic, inline math (`$ ... $`) etc.  Thus
a complete parser is given by the composition

```elm
  parse : Array String -> Tree AugmentedBlock
  parse = blockParse >> Tree.map inlineParse
```


As discussed in `./docs/PARSE.md`, the parser is *injective*,
meaning that the source can be recovered verbatim from the
parse tree.  There is also a module `Edit` exposing
a function which implements incremental parsing.
By *incremental*, we mean that when the source text is edited, one does not have to re-parse the entire document
to obtain a valid parse tree.  Indeed, it is often only a small amount of text. Such a capability makes possible interactive editing of documents which which are parsed, then rendered in real time. The strategy for doing this
is explained in section 10 of `.docs/PARSE.md` 

The implementation language used in this project is 
[Elm](https:elm-lang.org), a statically typed
language of pure functions.

**NOTE.** This project is an experiment, and in a great state of flux,
so it is not a good idea to rely on it for anything at the moment.
Also, there is a good deal of cruft that needs to be removed.
I wil do this shortly.



## Example Language

Below is a short piece of source text in
the kind of language we have in mind.
It is discussed at greater length in 
`./docs/PARSER.md`.


```text
| section Intro

Let's talk about matter 

| subsection Atoms 

They are tiny!

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


### Configuring a Language

An instance of the kind of language we consider 
is defined entirely by a module `BlockType`.  
To conifigure the parser for a given
language, suppose that we have modules

```elm
BlockType.LanguageA
BlockType.LanguageB
```

which define a `BlockType` and a partial order on block types.
To use LanguageA, add the line

```elm
import BlockType.LanguageB as BlockType exposing (BlockType(..), BlockKind(..))
```

to the files

```elm
BLParser.Parse
BLParser.Block
```





