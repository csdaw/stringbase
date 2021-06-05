# test that a basic examples work
test <- c("a", "b", "c")

expect_equal(str_c(test), test)
expect_equal(str_c(test, sep = " "), test)
expect_equal(str_c(test, collapse = ""), "abc")

# test that if no strings are supplied, output is a character of length 0
expect_equal(str_c(), character())

# test that base R recycling rules are obeyed
expect_equal(str_c(c("x", "y"), character()), c("x", "y"))
expect_equal(str_c("x", NULL), "x")
expect_equal(str_c(c("x", "y"), NULL), c("x", "y"))

# test that supplying invalid sep or collapse produces error
expect_error(
  str_c(c("x", "y", "z"), "a", sep = character()),
  "invalid separator"
)

expect_error(
  str_c(c("x", "y", "z"), "a", sep = NULL),
  "invalid separator"
)

expect_error(
  str_c(c("x", "y", "z"), "a", collapse = character()),
  "invalid 'collapse' argument"
)

# test that vectorised sep or collapse gives warnings
expect_warning(
  str_c(letters, sep = c("a", "b")),
  "argument `sep` should be a single character string; only the first element is used"
)

expect_warning(
  str_c(letters, collapse = c("a", "b")),
  "argument `collapse` should be NULL or a single character string; only the first element is used"
)
