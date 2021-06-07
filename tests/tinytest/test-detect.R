# test that str_detect returns a logical vector
expect_equal(str_detect(c(NA, "a", "b"), "a"), c(NA, TRUE, FALSE))

# test that str_detect special cases are correct
expect_equal(str_detect(NA, "x"), NA)
expect_equal(str_detect(character(), "x"), logical())

# test that vectorised patterns work
expect_equal(str_detect("ab", c("a", "b", "c")), c(TRUE, TRUE, FALSE))
expect_equal(str_detect(c("ca", "ab"), c("a", "c")), c(TRUE, FALSE))
expect_equal(str_detect("ab", c("a", "b", "c"), negate = TRUE), c(FALSE, FALSE, TRUE))

# test that str_detect negation works
expect_equal(str_detect(c("a", "ab", "abc"), "c", negate = TRUE), c(TRUE, TRUE, FALSE))

# test that str_detect can take pattern modifiers
expect_false(str_detect("a", fixed(".")))
expect_true(str_detect("a", regex("A", ignore_case = TRUE)))
expect_true(str_detect("a", perl("(?i)A")))

# test that str_starts works
expect_true(str_starts("ab", "a"))
expect_false(str_starts("ab", "b"))

# test that str_starts negation works
expect_false(str_starts("ab", "a", negate = TRUE))
expect_true(str_starts("ab", "b", negate = TRUE))

# test that str_starts can take pattern modifiers
expect_true(str_starts("ab", perl("A", ignore_case = TRUE)))
expect_true(str_starts("ab", regex("A", ignore_case = TRUE)))

# test that str_starts respects operators
expect_true(str_starts("ab", "b|a"))
expect_false(str_starts("ab", "c|b"))

# test that str_ends works
expect_true(str_ends("ab", "b"))
expect_false(str_ends("ab", "a"))

# test that str_ends negation works
expect_false(str_ends("ab", "b", negate = TRUE))
expect_true(str_ends("ab", "a", negate = TRUE))

# test that str_starts can take pattern modifiers
expect_true(str_ends("ab", perl("B", ignore_case = TRUE)))
expect_true(str_ends("ab", regex("B", ignore_case = TRUE)))

# test that str_starts respects operators
expect_true(str_ends("ab", "b|a"))
expect_false(str_ends("ab", "c|a"))

# test that tidyverse recycling rules are used
expect_error(str_detect(1:2, 1:3))
expect_error(str_starts(1:2, 1:3))
expect_error(str_ends(1:2, 1:3))
