
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
  data <- getParseData(parsed, includeText = NA)

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
