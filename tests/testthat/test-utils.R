
context("utils")

test_that("is_interactive", {
  expect_identical(interactive(), is_interactive())
})

test_that("is_terminal, not a tty", {
  mockery::stub(is_terminal, "isatty", FALSE)
  expect_false(is_terminal())
})

test_that("is_terminal, rstudio", {
  withr::with_envvar(
    c("RSTUDIO" = "1"),
    expect_false(is_terminal())
  )
})

test_that("is_terminal, rgui", {
  withr::with_envvar(
    c("R_GUI_APP_VERSION" = "1.2.3"),
    expect_false(is_terminal())
  )
})

test_that("is_terminal, emacs", {
  withr::with_options(
    list("STERM" = "iESS"),
    expect_false(is_terminal())
  )
})
