# test that patterns are coerced to character
fct <- factor("a")

expect_warning(perl(fct), "Coercing `pattern` to a plain character vector.")
expect_warning(fixed(fct), "Coercing `pattern` to a plain character vector.")
expect_warning(regex(fct), "Coercing `pattern` to a plain character vector.")

# test that output has the correct classes
fruit <- "banana"

expect_equal(class(perl(fruit)),
             c("stringrb_perl", "stringr_pattern", "character"))
expect_equal(class(fixed(fruit)),
             c("stringrb_fixed", "stringr_pattern", "character"))
expect_equal(class(regex(fruit)),
             c("stringrb_regex", "stringr_pattern", "character"))

# test that output has the correct ignore_case attribute
expect_true(attr(perl(fruit, ignore_case = TRUE), "ignore_case"))
expect_true(attr(regex(fruit, ignore_case = TRUE), "ignore_case"))
