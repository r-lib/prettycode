
context("print")

test_that("print without color support", {

  mockery::stub(print.function, "should_page", FALSE)
  mockery::stub(print.function, "can_pretty_print", FALSE)

  f <- function() { }
  expect_output(
    withr::with_options(list(crayon.enabled = TRUE), print.function(f)),
    "function[(][)] { }"
  )
})


test_that("print without color support 2", {

  called <- FALSE
  mockery::stub(print.function, "should_page", FALSE)
  mockery::stub(print.function, "can_pretty_print", FALSE)
  mockery::stub(
    print.function,
    "base::print.function",
    function(...) called <<- TRUE
  )

  withr::with_options(list(crayon.enabled = TRUE), print.function(ls))

  expect_true(called)
})

test_that("print with color support", {

  args <- NULL
  mockery::stub(print.function, "should_page", FALSE)
  mockery::stub(print.function, "can_pretty_print", TRUE)
  mockery::stub(print.function, "cat", function(...) args <<- list(...))

  f <- function() { 1 + 2 }
  withr::with_options(
    list(crayon.enabled = TRUE),
    print.function(f)
  )
  expect_true(crayon::has_style(paste(args[[1]], collapse = "\n")))
})

test_that("print with color support 2", {

  args <- NULL
  mockery::stub(print.function, "should_page", FALSE)
  mockery::stub(print.function, "can_pretty_print", TRUE)
  mockery::stub(print.function, "cat", function(...) args <<- list(...))

  withr::with_options(
    list(crayon.enabled = TRUE),
    print.function(ls)
  )
  expect_true(crayon::has_style(paste(args[[1]], collapse = "\n")))
})

test_that("pager", {

  cn <- NULL
  mockery::stub(print.function, "should_page", TRUE)
  mockery::stub(print.function, "can_pretty_print", TRUE)
  mockery::stub(print.function, "file.show", function(f) cn <<- readLines(f))

  f <- function() { 1 + 2 }
  withr::with_options(
    list(crayon.enabled = TRUE),
    print.function(f)
  )

  expect_true(crayon::has_style(paste(cn, collapse = "\n")))
})

test_that("can_pretty_print", {
  expect_identical(can_pretty_print(), crayon::has_color())
})

test_that("num_lines", {
  mockery::stub(num_lines, "system", "42")
  expect_equal(num_lines(), 42)
})

test_that("should_page 1", {
  mockery::stub(should_page, "is_interactive", FALSE)
  expect_false(should_page())
})

test_that("should_page 2", {
  mockery::stub(should_page, "is_interactive", FALSE)
  expect_false(should_page(1:100))
})

test_that("should_page 3", {
  mockery::stub(should_page, "is_interactive", TRUE)
  mockery::stub(should_page, "is_terminal", FALSE)
  expect_false(should_page(1:100))
})

test_that("should_page 4", {
  mockery::stub(should_page, "is_interactive", TRUE)
  mockery::stub(should_page, "is_terminal", TRUE)
  mockery::stub(should_page, "num_lines", 50)
  expect_false(should_page(1:10))
})

test_that("should_page 5", {
  mockery::stub(should_page, "is_interactive", TRUE)
  mockery::stub(should_page, "is_terminal", TRUE)
  mockery::stub(should_page, "num_lines", 50)
  expect_true(should_page(1:100))
})

test_that("fallback", {
  called <- FALSE
  mockery::stub(print.function, "base::print.function",
                function(...) called <<- TRUE)

  f <- new("function")
  body(f) <- substitute(.External(x), list(x = new("externalptr")))
  capture_output(print.function(f))
  expect_true(called)
})
