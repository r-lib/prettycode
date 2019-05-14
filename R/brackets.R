open_brackets <- function() {
  c("(", "{", "[")
}

close_brackets <- function(){
  c(")", "}", "]")
}

bracket_tokens <- function() {
  s <- c(open_brackets(), close_brackets())
  c(paste0("'", s, "'"), "LBB")
}

apply_color <- function(x, lvl, l){
  k <- (lvl - 1) %% length(l) + 1
  l[[k]](x)
}

#' Colored brackets
#'
#' Add color to brackets. Brackets will be coloured consecutively with the
#' colors provided in \code{color_seq} by scope.
#'
#' @param x a character vector of brackets consisting of a valid sequence of any
#'   of the following: \code{'[[', '[', ']', '(', ')', '{', '}'}
#' @param color_seq a list of functions that take and return a character scalar. The 
#' ordering defines the sequence of color functions to apply to a given scope level.
#' Color functions are recycled when the scope level exceeds the length of \code{color_seq}
#'
#' @details Meant for coloring brackets encountered within \code{highlight}.
#'   Note that occurrences of 'orphan' brackets are not taken into account
#'   mainly due to the fact that cases such as
#'
#'   \code{foo <- function(x){ `[[`(x, 1) }}
#'
#'   will either be converted to
#'
#'   \code{foo <- function(x){ x[[1]] }}
#'
#'   before the brackets are coloured if passed in as
#'   \code{highlight(deparse(foo))} or will be identified as a
#'   'SYMBOL_FUNCTION_CALL' token instead of 'LBB' if passed in as
#'
#'   \code{highlight("foo <- function(x){ `[[`(x, 1) }")}
#'
#'   Similarly, invalid code that would lead to orphaned brackets is not taken
#'   into account as this would be caught before hand in \code{highlight}.
#'
#' @keywords internal
color_brackets <- function(x, color_seq = list(yellow, blue, cyan)) {
  stopifnot(vapply(color_seq, is.function, logical(1)))
  open <- c(open_brackets(), "[[")
  o <- character() 
  lvl <- 0 
  i <- 1
  while (i <= length(x)) {
    
    if (x[i] %in% open) {
      o[length(o) + 1] <- x[i]
      lvl <- lvl + 1
      x[i] <- apply_color(x[i], lvl, color_seq)
      i <- i + 1
      next
    } 
    
    j <- nchar(o[length(o)])
    x[i:(i + j - 1)] <- 
      apply_color(x[i:(i + j - 1)], lvl, color_seq)
    i <- i + j
    lvl <- lvl - 1
    o <- o[-length(o)] 
  }
  x
}
