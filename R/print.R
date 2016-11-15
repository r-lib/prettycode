
#' Pretty Print R Code in the Terminal
#'
#' Replace the standard print method for functions with one that performs
#' syntax highlighting, using ANSI colors, if the terminal supports them.
#'
#' @docType package
#' @name prettycode
NULL

#' Print a function with syntax highlighting
#'
#' @param x Function to print.
#' @param useSource Whether to use the stored source code, if available.
#' @param style The highlight style to use, see [default_style()].
#' @param ... Not used currently, for compatibility with the `print`
#'   generic.
#' @return The function, invisibly.
#'
#' @export
#' @importFrom withr with_envvar

print.function <- function(x, useSource = TRUE,
                           style = default_style(), ...) {

  if (!can_pretty_print()) return(base::print.function(x, useSource))

  srcref <- getSrcref(x)
  src <- if (useSource && ! is.null(srcref)) {
    as.character(srcref)
  } else {
    deparse(x)
  }

  hisrc <- highlight(src, style = style)

  if (!should_page(hisrc)) {
    cat(hisrc, sep = "\n")

  } else {
    cat(hisrc, sep = "\n", file = tmp <- tempfile())
    on.exit(unlink(tmp), add = TRUE)
    with_envvar(
      c("LESS" = "-R", action = "prefix"),
      file.show(tmp)
    )
  }

  invisible(x)
}

#' @importFrom crayon has_color

can_pretty_print <- function() {
  has_color()
}

num_lines <- function() {
  tryCatch(
    as.numeric(system("tput lines", intern = TRUE)),
    error = function(e) NA_integer_
  )
}

should_page <- function(src) {
  is_interactive() && is_terminal() && length(src) > num_lines()
}