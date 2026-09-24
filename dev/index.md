# prettycode

> Pretty Print R Code in the Terminal

Replace the standard print method for functions with one that performs
syntax highlighting, using ANSI colors, if the terminal supports them.

## Installation

Stable version:

``` r

install.packages("prettycode")
```

Development version:

``` r

pak::pak("r-lib/prettycode")
```

## Usage

Just call
[`prettycode::prettycode()`](https://r-lib.github.io/prettycode/dev/reference/prettycode.md)
and start printing functions to the screen. Long functions are
automatically paged using the default pager.

![](/screenshot.png)

### Options

- `prettycode.should_page` controls paging. Use `FALSE` to disable
  paging for long functions.

## License

MIT © Gábor Csárdi
