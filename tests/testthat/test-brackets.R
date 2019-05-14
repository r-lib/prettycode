

context("colour_brackets")

col_seq <- list(function(x)
  paste0("1", x),
  function(x)
    paste0("2", x),
  function(x)
    paste0("3", x))

test_that("bracket highlighting", {
  # [](){}
  expect_equal(color_brackets(c("[", "]", "(", ")", "{", "}"), col_seq),
               c("1[", "1]", "1(", "1)", "1{", "1}"))
  
  # [({[({})]})]
  expect_equal(
    color_brackets(c(
      "[", "(", "{", "[", "(", "{", "}", ")", "]", "}", ")", "]"
    ),
    col_seq),
    c(
      "1[",
      "2(",
      "3{",
      "1[",
      "2(",
      "3{",
      "3}",
      "2)",
      "1]",
      "3}",
      "2)",
      "1]"
    )
  )
  
  # [[ [] ]][[ ()() ]]
  expect_equal(
    color_brackets(
      c("[[", "[", "]", "]", "]", "[[", "(", ")", "(", ")", "]", "]"),
      col_seq
    ),
    c(
      "1[[",
      "2[",
      "2]",
      "1]",
      "1]",
      "1[[",
      "2(",
      "2)",
      "2(",
      "2)",
      "1]",
      "1]"
    )
  )
})
