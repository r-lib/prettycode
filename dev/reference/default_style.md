# Default `prettycode` syntax highlighting style

A style function must return a named list of functions. Possible
entries:

- `reserved`: reserved words

- `number`: numeric literals

- `null`: the `NULL` constant

- `operator`: operators, including assignment

- `call`: function calls

- `string`: character literals

- `comment`: comments

- `bracket`: brackets: `(){}[]`

## Usage

``` r
default_style()
```

## Details

Each entry in a list must be a function that takes a character scalar,
and returns a character scalar with the exception of `bracket` which
should be a list of these type of functions defining a color sequence.
The default style adds ANSI formatting to the code.

Note that you can also change the code if you like, e.g. to include a
unicode arrow character instead of the two-character assignment
operator.

## Examples

``` r
highlight(deparse(get), style = default_style())
#> [1] "\033[31mfunction\033[39m \033[33m(\033[39mx, pos \033[32m=\033[39m \033[32m-\033[39m\033[34m1L\033[39m, envir \033[32m=\033[39m \033[36mas.environment\033[39m\033[34m(\033[39mpos\033[34m)\033[39m, mode \033[32m=\033[39m \033[33m\"any\"\033[39m, "
#> [2] "    inherits \033[32m=\033[39m \033[34mTRUE\033[39m\033[33m)\033[39m "                                                                                                                                                                                
#> [3] "\033[36m.Internal\033[39m\033[33m(\033[39m\033[36mget\033[39m\033[34m(\033[39mx, envir, mode, inherits\033[34m)\033[39m\033[33m)\033[39m"                                                                                                             
```
