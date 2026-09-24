# Changelog

## prettycode 1.1.0

CRAN release: 2019-12-16

- Use the colors of the current theme in RStudio.

- Color matching brackets to help distinguish between scopes
  ([\#8](https://github.com/r-lib/prettycode/issues/8),
  [@mdequeljoe](https://github.com/mdequeljoe)).

- The withr package is now a development dependency, it is not needed
  for regular use.

## prettycode 1.0.2

CRAN release: 2018-09-11

- Add
  [`prettycode::prettycode()`](https://r-lib.github.io/prettycode/dev/reference/prettycode.md).
  Call this function to turn on pretty-printing of function objects.
  This is needed to work around the new S3 method search limits in R
  3.5.x.

- If prettycode fails to highlight a function, it falls back to
  [`base::print.function()`](https://rdrr.io/r/base/print.html) now
  ([\#3](https://github.com/r-lib/prettycode/issues/3)).

## prettycode 1.0.1

CRAN release: 2017-12-12

- Avoid registering the `print.function` S3 method. This is needed to
  avoid a new `R CMD check` check

## prettycode 1.0.0

CRAN release: 2017-01-27

First public release.
