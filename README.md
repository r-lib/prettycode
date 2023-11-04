
# prettycode

> Pretty Print R Code in the Terminal

<!-- badges: start -->
[![R-CMD-check](https://github.com/r-lib/prettycode/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/r-lib/prettycode/actions/workflows/R-CMD-check.yaml)
[![](https://www.r-pkg.org/badges/version/prettycode)](https://www.r-pkg.org/pkg/prettycode)
[![CRAN RStudio mirror downloads](https://cranlogs.r-pkg.org/badges/prettycode)](https://www.r-pkg.org/pkg/prettycode)
[![Codecov test coverage](https://codecov.io/gh/r-lib/prettycode/branch/main/graph/badge.svg)](https://app.codecov.io/gh/r-lib/prettycode?branch=main)
<!-- badges: end -->

Replace the standard print method for functions with one that performs
syntax highlighting, using ANSI colors, if the terminal supports them.

## Installation

Stable version:

```r
install.packages("prettycode")
```

Development version:

```r
pak::pak("r-lib/prettycode")
```

## Usage

Just call `prettycode::prettycode()` and start printing functions to the
screen. Long functions are automatically paged using the default pager.

![](/screenshot.png)

### Options

  - `prettycode.should_page` controls paging. Use `FALSE` to disable paging for long functions.

## License

MIT © Gábor Csárdi
