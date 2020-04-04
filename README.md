# The BlockParser Project


The aim of this project is to develop a modular
parser for a family of block-structured
markup languages. e.g., Markdown, or the language
describes in `./docs/PARSER.md` .  An instance of such a 
language is defined entirely by a module
`BlockType`. See section 1 of `./docs/PARSER.md` for an example

### Configuring a Language

Suppose, for example that we have modules

```elm
BlockType.LanguageA
BlockType.LanguageB
```

Which define `BlockType` and a partial order on block types.
To use, say, LanguageA, one adds the line

```elm
import BlockType.LanguageB as BlockType exposing (BlockType(..))
```

to the files

```elm
BLParser.Parse
BLParser.Block
```

Note that a complete parser requires an additional step
which transforms blocks, taking into account whatever
inline elements there are in the markup language.  For this one
must define a suitable type `AugmentedBlock` 
and a function

```elm
parseInline : Block -> AugmentedBlock
```

which is mapped over `Tree Block`.  The main function in 
each of the two modules is implemented
as a state machine operating on lines.
As discussed in section 6, the parser is *injective*,
so that the source can be recovered verbatim from the
parse tree.  There is also a module `Edit` exposing
a function which implements incremental parsing.
By *incremental*, we mean that when the source text is edited, one does not have to re-parse the entire document
to obtain a valid parse tree.  Generally only a small amount of text must be re-parsed. Such a capability makes possible interactive editing of documents which which are parsed, then rendered in real time. This is explained in section 10. The implementation language is 
[Elm](https:elm-lang.org), a statically typed
language of pure functions.

The document `./docs/PARSER.md` describes the principles of operation 
of the parser and gives much additional detail.




