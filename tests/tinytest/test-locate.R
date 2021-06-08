# test that basic location matching works
expect_equal(str_locate("abc", "a")[1, ], c(start = 1, end = 1))
expect_equal(str_locate("abc", "b")[1, ], c(start = 2, end = 2))
expect_equal(str_locate("abc", "c")[1, ], c(start = 3, end = 3))
expect_equal(str_locate("abc", ".+")[1, ], c(start = 1, end  = 3))

# test that tidyverse recycling rules are used
expect_error(str_locate(1:2, 1:3))
expect_error(str_locate_all(1:2, 1:3))

# test that locations are integers
strings <- c("a b c a", "d e f")
expect_true(is.integer(str_locate(strings, "a")))

res <- stringr::str_locate_all(strings, "a")[[1]]
expect_true(is.integer(res))
expect_true(is.integer(invert_match(res)))

# test that string is properly vectorised
strings <- c("abca", "defaa")

locs <- str_locate(strings, "a")
expect_equal(locs[, "start"], c(1, 4))

locs <- str_locate_all(strings, "a")
expect_equal(lapply(locs, "[", , "start"), list(c(1, 4), c(4, 5)))

# test that pattern is properly vectorised
locs <- str_locate(strings, c("a", "d"))
expect_equal(locs[, "start"], c(1, 1))
expect_equal(locs[, "end"],   c(1, 1))

locs <- str_locate_all(c("abab"), c("a", "b"))
expect_equal(locs[[1]][, "start"], c(1, 3))
expect_equal(locs[[2]][, "start"], c(2, 4))

# test that str_locate can handle NA and 0 length matches
out <- str_locate(c(NA, "", "x", "xx", "xxx"), "x*")
expect_equal(out[, "start"], c(NA, 1L, 1L, 1L, 1L))
expect_equal(out[, "end"],   c(NA, 0:3))

# test that str_locate_all can handle 0 length matches
out <- str_locate_all(c(NA, ""), "x")
expect_equal(out[[1]], cbind(start = NA_integer_, end = NA_integer_))
expect_equal(out[[2]], cbind(start = integer(), end = integer()))
