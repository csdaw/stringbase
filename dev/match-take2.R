source("dev/regmatches.R")

phone <- "([2-9][0-9]{2})[- .]([0-9]{3})[- .]([0-9]{4})"
strings <- c(" 219 733 8965", "329-293-8753 ", "banana", "595 794 7569",
             "387 287 6718", "apple", "233.398.9187  ", "482 952 3315",
             "239 923 8115 and 842 566 4692", "Work: 579-499-7527", "$1000",
             "Home: 543.355.3679")

str_match3 <- function(string, pattern) {
  pat_len <- length(pattern)

  if (pat_len > 1) {
    matcher <- mapply(
      function(p, s) {
        regexec(
          text = s,
          pattern = p,
          ignore.case = stringrb:::ignore_case(pattern),
          perl = stringrb:::is_perl(pattern),
          fixed = FALSE
        )

      },
      pattern, string,
      USE.NAMES = FALSE
    )

    matches <- regmatches(
      if (length(string) == pat_len) string else rep.int(string, pat_len),
      matcher
    )

    tmp <- stringrb::str_replace_all(pattern, "\\\\\\(", "")
    ntokens <- max(nchar(stringrb::str_replace_all(tmp, "[^(]", ""))) + 1

  } else {
    matcher <- regexec(
      pattern = pattern,
      text = string,
      ignore.case = FALSE,
      perl = TRUE,
      fixed = FALSE,
      useBytes = FALSE
    )
    # print(matcher)

    matches <- regmatches(
      x = string,
      m = matcher,
      invert = FALSE
    )

    tmp <- stringrb::str_replace_all(pattern, "\\\\\\(", "")
    ntokens <- nchar(stringrb::str_replace_all(tmp, "[^(]", "")) + 1
  }

  matches[lengths(matches) == 0L] <- list(rep.int(NA_character_, ntokens))
  matrix(as.character(unlist(matches)), ncol = ntokens, byrow = TRUE)
}

stringr::str_match(stringr::fruit, "aaa")
str_match3(stringr::fruit, "aaa")

stringr::str_match(strings[[1]], phone)
str_match3(strings[[1]], phone)

## TESTS ##
set.seed(1410)
num <- matrix(sample(9, 10 * 10, replace = T), ncol = 10)
num_flat <- apply(num, 1, stringr::str_c, collapse = "")

phones <- stringr::str_c(
  "(", num[, 1], num[, 2], num[, 3], ") ",
  num[, 4], num[, 5], num[, 6], " ",
  num[, 7], num[, 8], num[, 9], num[, 10]
)

# test that empty strings return correct matrix of correct size
str_match3(c(NA, NA), "(a)")
matrix(NA_character_, 2, 2)

# can't deal with multiple patterns yet
str_match3(c(NA, NA, NA), c("(a)", "(b)", "c"))
stringr::str_match(c(NA, NA, NA), c("(a)", "(b)", "c"))

microbenchmark(
  str_match3(strings[1], phone),
  str_match3(strings[1], c(phone, "banana", "grape", "prange", "orange")),
  stringr::str_match(strings[1], c(phone, "banana"))
)

str_match3(character(), "(a)")
matrix(character(), 0, 2)

str_match3(c(character(), "banana"), "(a)")
matrix("a", 1, 2)

# test that no matching cases returns a 1 column matrix
str_match3(c("a", "b"), ".")
stringr::str_match(c("a", "b"), ".")

# test that str_match() works when all match
str_match3(phones, "\\(([0-9]{3})\\) ([0-9]{3}) ([0-9]{4})")
stringr::str_match(phones, "\\(([0-9]{3})\\) ([0-9]{3}) ([0-9]{4})")


# test that str_match returns NA when some inputs don't match
str_match3(c(phones, "blah", NA),
           "\\(([0-9]{3})\\) ([0-9]{3}) ([0-9]{4})")
stringr::str_match(c(phones, "blah", NA),
                   "\\(([0-9]{3})\\) ([0-9]{3}) ([0-9]{4})")


# test that match returns NA when optional group doesn't match
debugonce(regmatches)
str_match3(c("ab", "a"), "(a)(b)?")
stringr::str_match(c("ab", "a"), "(a)(b)?")

# test that tidyverse recycling rules are used
str_match3(c("a", "b"), c("a", "b", "c"))
stringr::str_match(c("a", "b"), c("a", "b", "c"))
