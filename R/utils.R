
`%||%` <- function(l, r) if (is.null(l)) r else l

is_interactive <- function() interactive()

is_terminal <- function() {
  isatty(stdin()) &&
    Sys.getenv("RSTUDIO") != 1 &&
    Sys.getenv("R_GUI_APP_VERSION") == "" &&
    .Platform$GUI != "Rgui" &&
    !identical(getOption("STERM"), "iESS") &&
    Sys.getenv("EMACS") != "t"
}
