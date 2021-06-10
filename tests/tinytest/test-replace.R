# test that basic replacement works
expect_equal(str_replace_all("abababa", "ba", "BA"), "aBABABA")
expect_equal(str_replace("abababa", "ba", "BA"), "aBAbaba")

# test that replacement strings with capture groups refs and dollar signs work
expect_equal(str_replace_all("abc$a$1$2", fixed("a"), "$1"), "$1bc$$1$1$2")
expect_equal(str_replace("abc$a$1$2", fixed("a"), "$1"), "$1bc$a$1$2")

expect_equal(str_replace_all("abcde", "(b)(c)(d)", "\\1"), "abe")
expect_equal(str_replace_all("abcde", "(b)(c)(d)", "\\2"), "ace")
expect_equal(str_replace_all("abcde", "(b)(c)(d)", "\\3"), "ade")

expect_equal(str_replace_all("abcde", "bcd", "\\\\1"), "a\\1e")

expect_equal(str_replace_all("a!1!2!b", "!", "$"), "a$1$2$b")
expect_equal(str_replace("aba", "b", "$"), "a$a")
expect_equal(str_replace("aba", "b", "$$$"), "a$$$a")
expect_equal(str_replace("aba", "(b)", "\\1$\\1$\\1"), "ab$b$ba")
expect_equal(str_replace("aba", "(b)", "\\1$\\\\1$\\1"), "ab$\\1$ba")
expect_equal(str_replace("aba", "(b)", "\\\\1$\\1$\\\\1"), "a\\1$b$\\1a")

# gsub("(b)(c)(d)", "\\0", "abcde", perl=TRUE) gives a0e,
# in ICU regex $0 refers to the whole pattern match.
# This is different to stringr!
expect_equal(str_replace_all("abcde", "(b)(c)(d)", "\\0"), "a0e")

# gsub("(b)(c)(d)", "\\4", "abcde", perl=TRUE) is legal,
# in ICU regex this gives an U_INDEX_OUTOFBOUNDS_ERROR.
# This is different to stringr!
expect_equal(str_replace_all("abcde", "(b)(c)(d)", "\\4"), "ae")

# test that one can replace multiple matches
x <- c("a1", "b2")
y <- str_replace_all(x, c("a" = "1", "b" = "2"))
expect_equal(y, c("11", "22"))

# test that one can replace multiple matches even when lengths differ
x <- c("a1", "b2", "c3")
y <- str_replace_all(x, c("a" = "1", "b" = "2"))
expect_equal(y, c("11", "22", "c3"))

# test that multiple matches respects class
x <- c("x", "y")
y <- str_replace_all(x, regex(c("X" = "a"), ignore_case = TRUE))
expect_equal(y, c("a", "y"))

# test that replacement must be a string
expect_error(str_replace("x", "x", 1), "must be a string or character vector")
expect_equal(str_replace("xyz", "x", NA_character_), NA_character_)

# that that one can replace all types of NA values
expect_equal(str_replace_na(NA), "NA")
expect_equal(str_replace_na(NA_character_), "NA")
expect_equal(str_replace_na(NA_complex_), "NA")
expect_equal(str_replace_na(NA_integer_), "NA")
expect_equal(str_replace_na(NA_real_), "NA")

# functions --------------------------------------------------------------------

# test that one can supply a replacement function
expect_equal(str_replace("abc", "a|c", toupper), "Abc")
expect_equal(str_replace_all("abc", "a|c", toupper), "AbC")

# test that the replacement CAN'T be a different length
# This is different to stringr!
#double <- function(x) str_dup(x, 2)
#expect_equal(str_replace_all("abc", "a|c", double), "aabcc")

# test that replacement with NA works
expect_equal(str_replace("abc", "z", toupper), "abc")

# test that trying to use a formula replacement produces an error
# This is different to stringr!
expect_error(
  str_replace("abc", "b", ~ "x"),
  "using a formula for replacement has not been implemented in stringrb."
)
expect_error(
  str_replace_all("abc", "b", ~ "x"),
  "using a formula for replacement has not been implemented in stringrb."
)
