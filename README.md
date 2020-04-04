# The BlockParser Project


The aim of this project is to develop a modular
parser for a family of block-structured
markup languages. e.g., Markdown, or the language
described in section 1 of `./docs/PARSER.md`. Such 
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





