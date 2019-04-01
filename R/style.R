
#' Default `prettycode` syntax highlighting style
#'
#' A style function must return a named list of functions. Possible
#' entries:
#' * `reserved`: reserved words
#' * `number`: numeric literals
#' * `null`: the `NULL` constant
#' * `operator`: operators, including assignment
#' * `call`: function calls
#' * `string`: character literals
#' * `comment`: comments
#' * `bracket`: brackets: \code{(){}[]} 
#'
#' Each entry in a list must be a function that takes a character
#' scalar, and returns a character scalar with the exception of `bracket`
#' which should be a list of these type of functions defining a color sequence. 
#' The default style adds ANSI formatting to the code.
#'
#' Note that you can also change the code if you like, e.g. to include
#' a unicode arrow character instead of the two-character assignment
#' operator.
#'
#' @importFrom crayon red cyan combine_styles make_style magenta bold
#'   blue green yellow italic
#' @export
#' @examples
#' highlight(deparse(get), style = default_style())

default_style <- function() {
  list(
    reserved = red,
    number   = blue,
    null     = combine_styles(blue, bold),
    operator = green,
    call     = cyan,
    string   = yellow,
    comment  = combine_styles(make_style("darkgrey"), italic),
    bracket  = c(yellow, blue, cyan)
  )
}
