# Colored brackets

Add color to brackets. Brackets will be coloured consecutively with the
colors provided in `color_seq` by scope.

## Usage

``` r
color_brackets(x, color_seq = list(yellow, blue, cyan))
```

## Arguments

- x:

  a character vector of brackets consisting of a valid sequence of any
  of the following: `'[[', '[', ']', '(', ')', '{', '}'`

- color_seq:

  a list of functions that take and return a character scalar. The
  ordering defines the sequence of color functions to apply to a given
  scope level. Color functions are recycled when the scope level exceeds
  the length of `color_seq`

## Details

Meant for coloring brackets encountered within `highlight`. Note that
occurrences of 'orphan' brackets are not taken into account mainly due
to the fact that cases such as

`` foo <- function(x){ `[[`(x, 1) } ``

will either be converted to

`foo <- function(x){ x[[1]] }`

before the brackets are coloured if passed in as
`highlight(deparse(foo))` or will be identified as a
'SYMBOL_FUNCTION_CALL' token instead of 'LBB' if passed in as

`` highlight("foo <- function(x){ `[[`(x, 1) }") ``

Similarly, invalid code that would lead to orphaned brackets is not
taken into account as this would be caught before hand in `highlight`.
