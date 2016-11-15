
context("highlight")

test_that("reserved", {

  expect_equal(
    highlight("function () { }", list(reserved = function(x) "foo")),
    "foo () { }"
  )

  expect_equal(
    highlight("if (1) NULL else NULL", list(reserved = function(x) "foo")),
    "foo (1) NULL foo NULL"
  )

  expect_equal(
    highlight("repeat {}", list(reserved = function(x) "foo")),
    "foo {}"
  )

  expect_equal(
    highlight("while (1) {}", list(reserved = function(x) "foo")),
    "foo (1) {}"
  )

  expect_equal(
    highlight("for (i in x) next", list(reserved = function(x) "foo")),
    "foo (i foo x) foo"
  )

  expect_equal(
    highlight("for (i in x) break", list(reserved = function(x) "foo")),
    "foo (i foo x) foo"
  )
})

test_that("number", {

  expect_equal(
    highlight("1 + 1.0 + -1 + 2L + Inf", list(number = function(x) "N")),
    "N + N + -N + N + N"
  )

  expect_equal(
    highlight(
      "NA + NA_real_ + NA_integer_ + NA_character_",
      list(number = function(x) "N")
    ),
    "N + N + N + N"
  )

  expect_equal(
    highlight("TRUE + FALSE", list(number = function(x) "N")),
    "N + N"
  )
})

test_that("null", {
  expect_equal(highlight("NULL", list(null = function(x) "!!!")), "!!!")
})

test_that("operator", {

  expect_equal(
    highlight(
      "~ ! 1 - 2 + 3:4 * 5 / 6 ^ 7",
      list(operator = function(x) "OP")
    ),
    "OP OP 1 OP 2 OP 3OP4 OP 5 OP 6 OP 7"
  )

  expect_equal(
    highlight(
      "? 1 %% 2 %+% 2 < 3 & 4 > 5 && 6 == 7 | 8 <= 9 || 10 >= 11",
      list(operator = function(x) "OP")
    ),
    "OP 1 OP 2 OP 2 OP 3 OP 4 OP 5 OP 6 OP 7 OP 8 OP 9 OP 10 OP 11"
  )

  expect_equal(
    highlight(
      "a <- 10; 20 -> b; c = 30; a$b; a@b",
      list(operator = function(x) "OP")
    ),
    "a OP 10; 20 OP b; c OP 30; aOPb; aOPb"
  )
})

test_that("call", {
  expect_equal(highlight("ls(2)", list(call = function(x) "F")), "F(2)")
})

test_that("string", {
  expect_equal(
    highlight("'s' + \"s\"", list(string = function(x) "S")),
    "S + S"
  )
})

test_that("comment", {
  expect_equal(
    highlight(c("# COM", " ls() ## ANOT"), list(comment = function(x) "C")),
    c("C", " ls() C")
  )
})
