# test that str_detect returns a logical vector
expect_equal(str_detect(c(NA, "a", "b"), "a"), c(NA, TRUE, FALSE))

# test that str_detect special cases are correct
expect_equal(str_detect(NA, "x"), NA)
expect_equal(str_detect(character(), "x"), logical())

# test that str_detect negation works
expect_equal(str_detect(c("a", "ab", "abc"), "c", negate = TRUE), c(T, T, F))

# test that str_detect can take pattern modifiers
expect_equal(str_detect("a", fixed(".")), FALSE)
expect_equal(str_detect("a", regex("A", ignore_case = TRUE)), TRUE)
expect_equal(str_detect("a", perl("(?i)A")), TRUE)
