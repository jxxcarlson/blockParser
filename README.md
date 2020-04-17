# The BlockParser Project

**Contents**



1. Introduction 
2. Example Language
3. A Property of Family **P**
4. Configuring a Language
5. Tests and tools

## 1. Introduction

The aim of this project is to develop a modular
parser for a family of block-structured
markup languages which we shall call **P**. 
Examples are  Markdown and the language
illustrated below. Not all markup languages
are of type **P**, as we note in section 3.

Languages in **P** are parsed by a pair of functions 

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
The system described here is configurable, with the language
definition contained in a single moodule.  Changing it changes the language (section 4).

As discussed in `./docs/PARSE.md`, the parser is *injective*,
meaning that the source can be recovered verbatim from the
parse tree.  In more technical terms, the functions `blockParse` 
and `parse` have left inverses.  There is also a module `Edit` exposing
a function which implements incremental parsing.
By *incremental*, we mean that when the source text is edited, one does not have to re-parse the entire document
to obtain a valid parse tree.  Many edits require one to re-parse a small fraction of the source text. Such a capability makes possible interactive editing of documents which which are parsed, then rendered in real time. The strategy for doing this
is explained in section 10 of `.docs/PARSE.md` 

The implementation language used in this project is 
[Elm](https:elm-lang.org), a statically typed
language of pure functions.

**NOTES** 

1. This project is an experiment, and in a great state of flux.
At the moment it is not a good idea to rely on it for anything.
Also, there is a good deal of cruft that needs to be removed.
I will do this shortly.

2. An end-to-end pipeline for using a markup language in **P** requires functions `blockParse`, `parseInline`, and 

    ```elm
    render : Tree AugmentedBlock -> OutputLanguage
    ```
    As an example, HTML can be the output language.

3. For now, we treat only the first component of this pipeline.

4. As the project progresses, I run little experiments to see
   to what extent the family **P** is adequate in the sense
   that it permits one to parse languages that one might 
   be interested in practice.



## 2. Example Language

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

## 3. A Property of Family **P**

A language **L** in **P** has given a partial order on block types.  
Let **T** = `parseBlock` **S** for some source text **S**. Thus **T**
has type `Tree Block`.  From it one derives a **T'** : `Tree BlockType`.
Let **B'** be the set of block types of **T'**. The tree structure 
imposes a partial order on **B'**.  A language **L** in **P** has
the property that the partial order imposed by parse trees
is the same as the given partial order. 

Not all markup languages have this property in the sense
that there is no partial order on block types which satisfies
the above condition.  Suppose, for example that one has blocks
of type *A* and *B* in a languagle **L'**, and that blocks of
type *A* can appear inside blocks of type **B** and *vice versa*.
Such a language is not in **P**.

The point of this project is not to treat the most
general class of markup languages, but rather to treat
an interesting an useful subset. That said, injectivity and incremental parsing (which are not new) can easily
be implemented for languages not in **P** using the ideas (and variants of the code) presented in this project.



## 4. Configuring a Language

An instance of the kind of language we consider 
is defined entirely by a module `BlockType`.  To configure the parser for a given
language, suppose that we have modules

```elm
BlockType.LanguageA
BlockType.LanguageB
```

which define a `BlockType` and a partial order on block types.
To use LanguageA, add the line

```elm
import BlockType.LanguageA as BlockType exposing (BlockType(..), BlockKind(..))
```

to the files

```elm
BLParser.Parse
BLParser.Block
```

## 5. Tests and Tools


In addition to the code in `./tests` and `./benchmarks`, there is the folder `./repl`.  It provides a command-line tool in the form of a repl for experimenting with the the parser. It is currently configured to parse text in `LanguageB` and to display a simplified version of the parse tree. Each node of the tree is displayed
on single line, indented according to the depth of the node. This tools is easily configurable.  Simply change the `transform` function in `./tools/src/Command.elm`

```
$ cd tools
$ run build # first time only
$ npm run repl
```

This folder provides tool which is useful for experimenting 
with the parser.  Below is a sample session.  

```text
> b               # display contents of register B
register B:
0: xxx
1: yyy

> edit 5 7 b a
Edited text > M   # replace line 5-7 of A by the lines of B
                  # the result is placed in register M
```
  
Next, we compute the parse tree of the text in M. The symbol
‡ stands for a newline.
               
```                  
> p m
register M parse tree:
0 (0,0):
1 (0,1): | section A
  2 (0,2):  ‡ | subsection B
    3 (0,3):  ‡ C ‡ xxx ‡ yyy
  2 (0,4):  ‡ | subsection D
    3 (0,5):  ‡ E
    3 (0,6):  ‡ F
1 (0,7):  ‡ | section G
```




