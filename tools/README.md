## How to use the tool

This folder provides tool which is useful for experimenting
with the parser.  Below is a sample session.
A source file `t1` is loaded and displayed on the 
terminal.  The source is then parsed and a representation 
of the parse tree is displayed, giving for each node
its depth in the tree.  The symbol `•` is an alias for
a newline.

**Load source text:**

```
> .load source/t1
| section A

| subsection B

C

| subsection D

E

F

| section G

loaded into register M
```
Note that the text is loaded into register M by default. The tool
has registers A, B, C, D, E, F and M like the classic HP
calculator.  

**Parse the text in register M:**

```
> p
register M parse tree:
0:
1: | section A
  2: •| subsection B
    3: •C
  2: •| subsection D
    3: •E
    3: •F
1: •| section G'
```

**View the contents of register M:**

```text
> d
register M:
0: section A
1:
2: | subsection B
3:
4: C
5:
6: | subsection D
7:
8: E
9:
10: F
11:
12: | section G
```

Type `h` at the command prompt for more information.