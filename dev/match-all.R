source("dev/regmatches.R")

phone <- "([2-9][0-9]{2})[- .]([0-9]{3})[- .]([0-9]{4})"

strings <- c(" 219 733 8965", "329-293-8753 ", "banana", "595 794 7569",
             "387 287 6718", "apple", "233.398.9187  ", "482 952 3315",
             "239 923 8115 and 842 566 4692", "Work: 579-499-7527", "$1000",
             "Home: 543.355.3679")

str_match_all <- function(string, pattern) {
  pat_len <- length(pattern)
  tmp <- stringrb::str_replace_all(pattern, "\\\\\\(", "")
  ntokens <- max(nchar(stringrb::str_replace_all(tmp, "[^(]", ""))) + 1

  if (pat_len > 1) {
    loc1 <- mapply(
      function(p, s) {
        gregexpr(
          text = s,
          pattern = p,
          perl = TRUE
        )

      },
      patterns2, string2,
      USE.NAMES = FALSE
    )

    m1 <- regmatches(
      if (length(string) == pat_len) string else rep.int(string, pat_len),
      loc1
    )

    Map(function(s, p, n) {
      if (length(s) == 0) return(matrix(character(), ncol = n, byrow = TRUE))

      loc2 <- regexec(p, s)

      m2 <- regmatches(s, loc2)
      matrix(as.character(unlist(m2)), ncol = n, byrow = TRUE)
    }, m1, pattern, ntokens)

  } else {
    loc1 <- gregexpr(pattern, string)

    m1 <- regmatches(string, loc1)

    Map(function(s, n) {
      if (length(s) == 0) return(matrix(character(), ncol = n, byrow = TRUE))

      loc2 <- regexec(pattern, s)

      m2 <- regmatches(s, loc2)
      matrix(as.character(unlist(m2)), ncol = n, byrow = TRUE)
    }, m1, ntokens)
  }
}

stringr::str_match_all(strings, phone)
str_match_all(strings, phone)

t_loc1 <- gregexpr(phone, strings)

t_m1 <- regmatches(strings, t_loc1)
t_m1
# t_m1[lengths(t_m1) == 0] <- list(character())
t_m1

Map(function(s) {
  # do stuff
  if (length(s) == 0) return(matrix(character(), ncol = 4, byrow = TRUE))
  t_loc2 <- regexec(phone, s)

  t_matches <- regmatches(s, t_loc2)

  matrix(as.character(unlist(t_matches)), ncol = 4, byrow = TRUE)
}, t_m1)

t_m1[lengths(t_m1) == 0] <- list(matrix(character(), ncol = 4, byrow = TRUE))
t_m1[lengths(t_m1) > 0] <- list()

stringr::str_match_all(strings, phone)
t_m1

string2 <- "apple"
patterns2 <- c("a", "b", "p")

stringr::str_match_all(string2, patterns2)
str_match_all(string2, patterns2)

microbenchmark(
  stringr::str_match_all(string2, patterns2),
  str_match_all(string2, patterns2)
)

#### TESTS ####
set.seed(1410)
num <- matrix(sample(9, 10 * 10, replace = T), ncol = 10)
num_flat <- apply(num, 1, stringr::str_c, collapse = "")

phones <- stringr::str_c(
  "(", num[, 1], num[, 2], num[, 3], ") ",
  num[, 4], num[, 5], num[, 6], " ",
  num[, 7], num[, 8], num[, 9], num[, 10])

stringr::str_match_all("a", "(a)(b)?")
str_match_all("a", "(a)(b)?")

phones_one <- stringr::str_c(phones, collapse = " ")
stringr::str_match_all(phones_one,
                       "\\(([0-9]{3})\\) ([0-9]{3}) ([0-9]{4})")
str_match_all(phones_one,
              "\\(([0-9]{3})\\) ([0-9]{3}) ([0-9]{4})")

#### SPEED CHECK ####
set.seed(1)
strings_long <- charlatan::ch_phone_number(n = 10000L)
library(microbenchmark)

blah <- function() {
  xxx <- gregexec(phone, strings_long)
  regmatches(strings_long, xxx)
}

microbenchmark(
  stringr::str_match_all(strings_long, phone),
  str_match_all(strings_long, phone),
  blah(),
  times = 20L
)
