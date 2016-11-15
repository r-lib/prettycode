
context("replace_in_place")

test_that("replace_in_place", {

  expect_equal(
    replace_in_place("1234567890", c(2, 6), c(5, 8), c("foobar", "xxx")),
    "1foobarxxx90"
  )

  expect_equal(
    replace_in_place("1234567890", c(1, 5), c(6, 10), c("A", "B")),
    "AB"
  )
})

test_that("replace_in_place corner cases", {

  expect_equal(
    replace_in_place("foobar", integer(), integer(), character()),
    "foobar"
  )

  expect_equal(
    replace_in_place("12345", 1L, 5L, "no!"),
    "no!"
  )

  expect_equal(
    replace_in_place("12345", 1:5, 1:5, letters[1:5]),
    "abcde"
  )
})
