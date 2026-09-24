# Print a function with syntax highlighting

To turn on pretty printing of functions, you need to call
[`prettycode::prettycode()`](https://r-lib.github.io/prettycode/dev/reference/prettycode.md).
It might be a good idea to call it from your `.Rprofile`.

## Usage

``` r
# S3 method for class '`function`'
print(x, useSource = TRUE, style = default_style(), ...)
```

## Arguments

- x:

  Function to print.

- useSource:

  Whether to use the stored source code, if available.

- style:

  The highlight style to use, see
  [`default_style()`](https://r-lib.github.io/prettycode/dev/reference/default_style.md).

- ...:

  Not used currently, for compatibility with the `print` generic.

## Value

The function, invisibly.
