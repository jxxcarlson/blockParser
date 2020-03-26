module Text exposing (lineCount, t1, t1a, t2, text1, text2, text3, text4, text4b)


lineCount : String -> Int
lineCount str =
    str |> String.lines |> List.length


t1 =
    """
| section A

X

| section B
"""


t1a =
    """
| section A


X

| section B
"""


t2 =
    """
| section A

| subsection B

| section C

"""


text1 =
    """

one

two

three
"""


text2 =
    """

one
two

three
four
five

"""


text3 =
    """
| section Intro

Fee, fie fo fum

Roses are red, 
violets are blue

| section Periodic Table

Mendeleyev started it.

| subsection Hydrogen

One proton 

| subsection Helium 

Two protons.
Used to fill baloons.

| section Molecules

To be continued ...

"""


text4 =
    """
| section Intro

Fee, fie fo fum

Roses are red,
violets are blue

| math
a^2 + b^2 = c^2

| quotation Mother Jones
A stitch in time
saves nine.

| section Periodic Table

Mendeleyev started it.

| subsection Hydrogen

One proton

| subsection Helium

Two protons.
Used to fill baloons.

| section Molecules

To be continued ...

"""


text4b =
    """
| section Intro
          
Fee, fie fo fum

Roses are red,
violets are blue

| math
a^2 + b^2 = c^2

| quotation Mother Jones
A stitch in time
saves nine.

| section Periodic Table

Mendeleyev started it.

| subsection Hydrogen

One proton

| subsection Helium

Two protons.
Used to fill baloons.

| section Molecules

To be continued ...

"""
