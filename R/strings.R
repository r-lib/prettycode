
replace_in_place <- function(str, start, end, replacement) {

  stopifnot(
    length(str) == 1,
    length(start) == length(end),
    length(end) == length(replacement)
  )

  keep <- substring(str, c(1, end + 1), c(start - 1, nchar(str)))

  pieces <- character(length(replacement) * 2 + 1)

  even <- seq_along(replacement) * 2
  odd <- c(1, even + 1)
  pieces[even] <- replacement
  pieces[odd] <- keep

  paste0(pieces, collapse = "")
}
