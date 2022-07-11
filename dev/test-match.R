library(tinytest)
library(stringrb)
source("dev/match.R")

set.seed(1410)
num <- matrix(sample(9, 10 * 10, replace = T), ncol = 10)
num_flat <- apply(num, 1, str_c, collapse = "")

phones <- str_c(
  "(", num[, 1], num[, 2], num[, 3], ") ",
  num[, 4], num[, 5], num[, 6], " ",
  num[, 7], num[, 8], num[, 9], num[, 10]
)

# test that empty strings return correct matrix of correct size
expect_equal(
  str_match(c(NA, NA), "(a)"),
  matrix(NA_character_, 2, 2)
)

expect_equal(
  str_match(c(NA, NA, NA), c("(a)", "(b)", "c")),
  matrix(NA_character_, 3, 2)
)

expect_equal(
  str_match(character(), "(a)"),
  matrix(character(), 0, 2)
)

expect_equal(
  str_match(c(character(), "banana"), "(a)"),
  matrix("a", 1, 2)
)

# test that no matching cases returns a 1 column matrix
res <- str_match(c("a", "b"), ".")

expect_equal(nrow(res), 2)
expect_equal(ncol(res), 1)
expect_equal(res[, 1], c("a", "b"))

# test that str_match() works when all match
matches <- str_match(phones, "\\(([0-9]{3})\\) ([0-9]{3}) ([0-9]{4})")

expect_equal(nrow(matches), length(phones))
expect_equal(ncol(matches), 4)

expect_equal(matches[, 1], phones)

matches_flat <- apply(matches[, -1], 1, str_c, collapse = "")
expect_equal(matches_flat, num_flat)

# test that str_match returns NA when some inputs don't match
matches <- str_match(c(phones, "blah", NA),
                     "\\(([0-9]{3})\\) ([0-9]{3}) ([0-9]{4})")

expect_equal(nrow(matches), length(phones) + 2)
expect_equal(ncol(matches), 4)

expect_equal(matches[11, ], rep(NA_character_, 4))
expect_equal(matches[12, ], rep(NA_character_, 4))

# test that match returns NA when optional group doesn't match
expect_equal(str_match(c("ab", "a"), "(a)(b)?")[, 3], c("b", NA_character_))

# test that tidyverse recycling rules are used
expect_error(str_match(c("a", "b"), c("a", "b", "c")))
