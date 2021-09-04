test <- function(string, pattern) {
  xxx <- gregexpr(pattern, string)

  xxx_lengths <- lengths(xxx)
  xxx_lengths[xxx %in% c(-1)] <- 0

  yyy <- regmatches(string, xxx)

  zzz <- lapply(yyy, function(m) do.call(rbind, regmatches(m, regexec(pattern, m))))

  out2 <- lapply(xxx_lengths, function(r) matrix(NA_character_, nrow = r, ncol = max(lengths(zzz))))
  out2[!xxx %in% c(-1, NA)] <- zzz[!xxx %in% c(-1, NA)]
  out2
}

test <- function(string, pattern) {
  xxx <- gregexpr(pattern, string)
  str(xxx %in% c(-1))
  st_length <- length(string)
  xxx_lengths <- lengths(xxx)
  xxx_lengths[xxx %in% c(-1)] <- 0

  yyy <- regmatches(string, xxx)

  zzz <- lapply(yyy, function(m) do.call(rbind, regmatches(m, regexec(pattern, m))))
  #str(zzz)
  print(which(is.null(zzz)))
  zzz[xxx %in% c(-1)] <- matrix(NA_character_, nrow = 0, ncol = max(lengths(zzz)))
  #out2 <- lapply(xxx_lengths, function(r) matrix(NA_character_, nrow = r, ncol = max(lengths(zzz))))
  #out2[!xxx %in% c(-1, NA)] <- zzz[!xxx %in% c(-1, NA)]
  #out2
  zzz
}


st <- list("amber johnson", "anhar link ari")
re <- "\\ba[a-z]+\\b"

stringr::str_match_all(st, re)
test(st, re)

microbenchmark::microbenchmark(
  stringr::str_match_all(st, re),
  test(st, re)
)


st <- c("xy1234yz98xy567", "123xy098")
re <- "xy(\\d+)"

stringr::str_match_all(st, re)
test(st, re)

microbenchmark::microbenchmark(
  stringr::str_match_all(st, re),
  test(st, re)
)


st <- c("aristotle", "plato", "epictetus", "seneca the younger", "epicurus", "marcus aurelius")
re <- ".*us"

stringr::str_match_all(st, re)
#gregexpr(re, st)
test(st, re)

microbenchmark::microbenchmark(
  stringr::str_match_all(st, re),
  test(st, re)
)

st <- c(
  "apple",
  "219 733 8965",
  "329-293-8753",
  "Work: 579-499-7527; Home: 543.355.3679"
)
re <- "([2-9][0-9]{2})[- .]([0-9]{3})[- .]([0-9]{4})"



test(st, re)
#gregexpr(re, st)
#new_st <- regmatches(st, gregexpr(re, st))
#new_st

#regexec(re, new_st)
stringr::str_match_all(st, re)

microbenchmark::microbenchmark(
  test2(st, re),
  stringr::str_match_all(st, re)

)

