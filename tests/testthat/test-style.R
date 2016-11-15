
context("style")

test_that("default_style", {
  def <- default_style()
  expect_true(is.list(def))
  expect_true(
    all(
      names(def) %in%
      c("reserved", "number", "null", "operator", "call", "string", "comment")
    )
  )
})
