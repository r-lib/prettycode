
operator_tokens <- function() {
  c(
    "'-'", "'+'", "'!'", "'~'", "'?'", "':'", "'*'", "'/'", "'^'",
    "SPECIAL", "LT", "GT", "EQ", "GE", "LE", "AND", "AND2", "OR", "OR2",
    "LEFT_ASSIGN", "RIGHT_ASSIGN", "'$'", "'@'", "EQ_ASSIGN"
  )
}

reserved_words <- function() {
  c("FUNCTION", "IF", "ELSE",
    "REPEAT", "WHILE", "FOR", "IN", "NEXT", "BREAK")
}

# same as `getParseData(, includeText = NA)` but making sure strings and symbols are not trimmed
get_parse_data <- function(x) {
  # include text so we don't lose long strings and symbols
  data <- getParseData(x, includeText = TRUE)
  # fetch indices of potentially trimmed text
  row_number <- which(data$token %in% c("STR_CONST", "SYMBOL") & startsWith(data$text, "["))
  ids <- data$id[row_number]
  parent_ids <- data$parent[row_number]
  parent_row_number <- match(parent_ids, data$id)
  # consider only if parent is expr and parent has a single child
  eligible_parent <-
    data$token[parent_row_number] == "expr" &
    vapply(parent_ids, function(x) sum(data$parent == x) == 1, logical(1))
  # replace with untrimmed
  data$text[row_number[eligible_parent]] <- data$text[parent_row_number[eligible_parent]]
  # remove text for non terminal tokens, as `getParseData(, includeText = NA)` would
  data$text[!data$terminal] <- ""
  data
}

#' Syntax highlight R code
#'
#' @param code Character vector, each element is one line of code.
#' @param style Style functions, see [default_style()].
#' @return Character vector, the highlighted code.
#'
#' @importFrom utils getSrcref getParseData
#' @export
#' @examples
#' highlight(deparse(ls))
#' cat(highlight(deparse(ls)), sep = "\n")

highlight <- function(code, style = default_style()) {

  parsed <- parse(text = code, keep.source = TRUE)
  data <- get_parse_data(parsed)

  hitext <- data$text

  ## Reserved words if else repeat while function for in next break
  if (!is.null(style$reserved)) {
    reserved <- data$token %in% reserved_words()
    hitext[reserved] <- style$reserved(data$text[reserved])
  }

  ## Numeric constants, including NAs, NaN and Inf
  if (!is.null(style$number)) {
    num_const <- data$token == "NUM_CONST"
    hitext[num_const] <- style$number(data$text[num_const])
  }

  ## NULL
  if (!is.null(style$null)) {
    null <- data$token == "NULL_CONST"
    hitext[null] <- style$null(data$text[null])
  }

  ## Operators
  if (!is.null(style$operator)) {
    operator <- data$token %in% operator_tokens()
    hitext[operator] <- style$operator(data$text[operator])
  }

  ## Function calls
  if (!is.null(style$call)) {
    fun_call <- data$token == "SYMBOL_FUNCTION_CALL"
    hitext[fun_call] <- style$call(data$text[fun_call])
  }

  ## Strings
  if (!is.null(style$string)) {
    string <- data$token == "STR_CONST"
    hitext[string] <- style$string(data$text[string])
  }

  ## Comments
  if (!is.null(style$comment)) {
    comment <- data$token == "COMMENT"
    hitext[comment] <- style$comment(data$text[comment])
  }

  ## Brackets
  if (!is.null(style$bracket)){
    bracket <- data$token %in% bracket_tokens()
    hitext[bracket] <- color_brackets(data$text[bracket], style$bracket)
  }

  do_subst(code, data, hitext)
}

do_subst <- function(code, pdata, hitext) {

  pdata$hitext <- hitext

  ## Need to do this line by line. TODO: multiline stuff might be broken
  vapply(seq_along(code), FUN.VALUE = character(1), function(no) {
    my <- pdata[pdata$line1 == no & pdata$line2 == no,, drop = FALSE]
    replace_in_place(code[no], my$col1, my$col2, my$hitext)
  })
}
