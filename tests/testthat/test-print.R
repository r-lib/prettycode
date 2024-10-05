test_that("print without color support", {

  local_mocked_bindings(should_page = function() FALSE)
  local_mocked_bindings(can_pretty_print = function() FALSE)

  f <- function() { }
  expect_output(
    withr::with_options(list(cli.num_colors = 256), print.function(f)),
    "function[(][)] { }"
  )
})


test_that("print without color support 2", {

  called <- FALSE
  local_mocked_bindings(should_page = function() FALSE)
  local_mocked_bindings(can_pretty_print = function() FALSE)
  local_mocked_bindings(print.function = function(...) called <<- TRUE)

  withr::with_options(list(crayon.enabled = TRUE), print.function(ls))

  expect_true(called)
})

test_that("print with color support", {

  args <- NULL
  local_mocked_bindings(should_page = function(...) FALSE)
  local_mocked_bindings(can_pretty_print = function() TRUE)
  local_mocked_bindings(cat = function(...) args <<- list(...))

  f <- function() { 1 + 2 }
  withr::with_options(
    list(cli.num_colors = 256),
    print.function(f)
  )
  expect_true(crayon::has_style(paste(args[[1]], collapse = "\n")))
})

test_that("print with color support 2", {

  args <- NULL
  local_mocked_bindings(should_page = function(...) FALSE)
  local_mocked_bindings(can_pretty_print = function() TRUE)
  local_mocked_bindings(cat = function(...) args <<- list(...))

  withr::with_options(
    list(cli.num_colors = 256),
    print.function(ls)
  )
  expect_true(crayon::has_style(paste(args[[1]], collapse = "\n")))
})

test_that("pager", {

  cn <- NULL
  local_mocked_bindings(should_page = function(...) TRUE)
  local_mocked_bindings(can_pretty_print = function() TRUE)
  local_mocked_bindings(file.show = function(f) cn <<- readLines(f))

  f <- function() { 1 + 2 }
  withr::with_options(
    list(cli.num_colors = 256),
    print.function(f)
  )

  expect_true(crayon::has_style(paste(cn, collapse = "\n")))
})

test_that("can_pretty_print", {
  expect_identical(can_pretty_print(), crayon::has_color())
})

test_that("num_lines", {
  local_mocked_bindings(system = function(...) "42")
  expect_equal(num_lines(), 42)
})

test_that("should_page 1", {
  local_mocked_bindings(is_interactive = function() FALSE)
  expect_false(should_page())
})

test_that("should_page 2", {
  local_mocked_bindings(is_interactive = function() FALSE)
  expect_false(should_page(1:100))
})

test_that("should_page 3", {
  local_mocked_bindings(is_interactive = function() TRUE)
  local_mocked_bindings(is_terminal = function() FALSE)
  expect_false(should_page(1:100))
})

test_that("should_page 4", {
  local_mocked_bindings(is_interactive = function() TRUE)
  local_mocked_bindings(is_terminal = function() TRUE)
  local_mocked_bindings(num_lines = function() 50)
  expect_false(should_page(1:10))
})

test_that("should_page 5", {
  local_mocked_bindings(is_interactive = function() TRUE)
  local_mocked_bindings(is_terminal = function() TRUE)
  local_mocked_bindings(num_lines = function() 50)
  expect_true(should_page(1:100))
})

test_that("fallback", {
  called <- FALSE
  local_mocked_bindings(print.function = function(...) called <<- TRUE)
  f <- new("function")
  body(f) <- substitute(.External(x), list(x = new("externalptr")))
  capture_output(print.function(f))
  expect_true(called)
})
