# test that str_extract() extracts first match if found, NA otherwise
shopping_list <- c("apples x4", "bag of flour", "bag of sugar", "milk x2")
word_1_to_4 <- str_extract(shopping_list, "\\b[a-z]{1,4}\\b")

expect_equal(length(word_1_to_4), length(shopping_list))
expect_equal(word_1_to_4[1], NA_character_)

# test that str_extract() handles NA and empty matches
expect_equal(str_extract(c(NA, "", "x"), "x"),
             c(NA, NA, "x"))

# test that tidyverse recycling rules are used
expect_error(str_extract(c("a", "b"), c("a", "b", "c")))
expect_error(str_extract_all(c("a", "b"), c("a", "b", "c")))

# test that no match yields empty vector
expect_equal(str_extract_all("a", "b")[[1]], character())
