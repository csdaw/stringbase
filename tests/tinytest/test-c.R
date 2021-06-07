# test that a basic examples work
test <- c("a", "b", "c")

expect_equal(str_c(test), test)
expect_equal(str_c(test, sep = " "), test)
expect_equal(str_c(test, collapse = ""), "abc")

# test that tidyverse recycling rules are obeyed
expect_equal(str_c(), character())

expect_equal(str_c("x", character()), character())
expect_equal(str_c("x", NULL), "x")

expect_error(
  str_c(c("x", "y"), character()),
  "Can't recycle input vectors. They should be of length 1 or length 2."
)
expect_equal(str_c(c("x", "y"), NULL), c("x", "y"))

# test that supplying invalid sep or collapse produces error
expect_error(
  str_c(letters, sep = c("a", "b")),
  "`sep` must be a single string"
)

expect_error(
  str_c(c("x", "y", "z"), "a", sep = character()),
  "`sep` must be a single string"
)

expect_error(
  str_c(c("x", "y", "z"), "a", sep = NULL),
  "`sep` must be a single string"
)


expect_error(
  str_c(letters, collapse = c("a", "b")),
  "`collapse` must be NULL or a single string"
)


expect_error(
  str_c(c("x", "y", "z"), "a", collapse = character()),
  "`collapse` must be NULL or a single string"
)
