# test that the correct substring is extracted"
alphabet <- paste0(letters, collapse = "")
expect_equal(str_sub(alphabet, 1, 3),
             "abc")
expect_equal(str_sub(alphabet, 24, 26),
             "xyz")

#test that arguments are expanded to longest
expect_equal(str_sub(alphabet, c(1, 24), c(3, 26)),
             c("abc", "xyz"))
expect_equal(str_sub(c("abc", "xyz"), 2, 2),
             c("b", "y"))

# test_that start and end are recycled
expect_equal(str_sub(c("ab", "cd"), 2, 2),
             c("b", "d"))
expect_equal(str_sub(character(), 1),
             character())

# test that specifying only end subsets from start
expect_equal(str_sub(alphabet, end = 3),
             "abc")

# test_that("start and end are vectorised
expect_equal(str_sub(c("ab", "cde"), -1, -1),
             c("b", "e"))
expect_equal(str_sub(c("ab", "cde"), 1, c(1, 2)),
             c("a", "cd"))

# test that can pass start a matrix
expect_equal(str_sub("abcdefg", cbind(3, 5)),
             "cde")

# test that specifying only start subsets to end
expect_equal(str_sub(alphabet, 24),
             "xyz")

# test_that specifying -1 as end selects entire string
expect_equal(str_sub("ABCDEF", c(4, 5), c(5, -1)),
             c("DE", "EF"))

expect_equal(str_sub("ABCDEF", c(4, 5), c(-1, -1)),
             c("DEF", "EF"))

# test that negative values select from end
expect_equal(str_sub("ABCDEF", 1, -4), "ABC")
expect_equal(str_sub("ABCDEF", -3), "DEF")

# test that missing arguments give missing results
expect_equal(str_sub(NA), NA_character_)
expect_equal(str_sub(NA, 1, 3), NA_character_)
expect_equal(str_sub(c(NA, "NA"), 1, 3), c(NA, "NA"))

# test that (limited) replacement works
x <- "BBCDEF"
str_sub(x, 1, 1) <- "A"
expect_equal(x, "ABCDEF")

str_sub(x, -1, -1) <- "K"
expect_equal(x, "ABCDEK")

# test that replacement with NA works
x <- "BBCDEF"
str_sub(x, NA) <- "A"
expect_equal(x, NA_character_)

x <- "BBCDEF"
str_sub(x, NA, omit_na = TRUE) <- "A"
str_sub(x, 1, 1, omit_na = TRUE) <- NA
expect_equal(x, "BBCDEF")

