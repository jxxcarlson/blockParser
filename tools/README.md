## How to use the tool

This folder provides tool which is useful for experimenting
with the parser.  Below is a sample session.
A source file `t1` is loaded and displayed on the 
terminal.  The source is then parsed and a representation 
of the parse tree is displayed, giving for each node
its depth in the tree.  The symbol `@` is an alias for
a newline.


```
> .load source/t1
| section A

| subsection B

C

| subsection D 

E

F

| section G

'0: \n' +
  '1: | section A\n' +
  '  2: @| subsection B\n' +
  '    3: @C\n' +
  '  2: @| subsection D\n' +
  '    3: @E\n' +
  '    3: @F\n' +
  '1: @| section G'
```
