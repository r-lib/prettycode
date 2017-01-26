
# prettycode

> Pretty Print R Code in the Terminal

[![Linux Build Status](https://travis-ci.org/gaborcsardi/prettycode.svg?branch=master)](https://travis-ci.org/gaborcsardi/prettycode)
[![Windows Build status](https://ci.appveyor.com/api/projects/status/github/gaborcsardi/prettycode?svg=true)](https://ci.appveyor.com/project/gaborcsardi/prettycode)
[![](http://www.r-pkg.org/badges/version/prettycode)](http://www.r-pkg.org/pkg/prettycode)
[![CRAN RStudio mirror downloads](http://cranlogs.r-pkg.org/badges/prettycode)](http://www.r-pkg.org/pkg/prettycode)
[![Coverage Status](https://img.shields.io/codecov/c/github/gaborcsardi/prettycode/master.svg)](https://codecov.io/github/gaborcsardi/prettycode?branch=master)

Replace the standard print method for functions with one that performs
syntax highlighting, using ANSI colors, if the terminal supports them.

## Installation

```r
source("https://install-github.me/gaborcsardi/prettycode")
```

## Usage

Just load the package and start printing functions to the screen.
Long functions are automatically paged using the default pager.

![](/inst/screenshot.png)

## License

MIT © Gábor Csárdi
