# From stringr before it used stringi:
# https://github.com/tidyverse/stringr/blob/stringr-0.6.2/R/match.r
str_match2 <- function(string, pattern) {
  # string <- check_string(string)
  # pattern <- check_pattern(pattern, string)

  if (length(string) == 0) return(character())

  matcher <- re_call("regexec", string, pattern)
  matches <- regmatches(string, matcher)

  # Figure out how many groups there are and coerce into a matrix with
  # nmatches + 1 columns
  tmp <- stringrb::str_replace_all(pattern, "\\\\\\(", "")
  n <- nchar(stringrb::str_replace_all(tmp, "[^(]", "")) + 1

  len <- vapply(matches, length, integer(1))
  matches[len == 0] <- rep(list(rep(NA_character_, n)), sum(len == 0))

  do.call("rbind", matches)
}

compact <- function(l) Filter(Negate(is.null), l)

re_call <- function(f, string, pattern, replacement = NULL) {
  args <- list(pattern, replacement, string,
               fixed = stringrb:::is_fixed(pattern), ignore.case = stringrb:::ignore_case(pattern),
               perl = stringrb:::is_perl(pattern))

  if (!("perl" %in% names(formals(f)))) {
    if (args$perl) message("Perl regexps not supported by ", f)
    args$perl <- NULL
  }

  do.call(f, compact(args))
}

source("dev/match.R")
library(microbenchmark)
library(ggplot2)

#### STR MATCH ####

strings <- c(" 219 733 8965", "329-293-8753 ", "banana", "595 794 7569",
  "387 287 6718", "apple", "233.398.9187  ", "482 952 3315",
  "239 923 8115 and 842 566 4692", "Work: 579-499-7527", "$1000",
  "Home: 543.355.3679")
phone <- "([2-9][0-9]{2})[- .]([0-9]{3})[- .]([0-9]{4})"

# str_extract(strings, phone)
str_match(strings, phone)
str_match2(strings, phone)
stringr::str_match(strings, phone)

microbenchmark::microbenchmark(
  str_match(strings, phone),
  str_match2(strings, phone),
  stringr::str_match(strings, phone),
  times = 500L
)

microbenchmark::microbenchmark(
  str_match("This is a string", c("a", "i")),
  # str_match2("This is a string", c("a", "i")),
  stringr::str_match("This is a string", c("a", "i")),
  times = 500L
)



#### STR MATCH ALL ####
str_match_all2 <- function(string, pattern) {
  matches <- stringrb::str_extract_all(string, pattern)

  lapply(matches, function(match) {
    str_match2(match, pattern)
  })
}

microbenchmark::microbenchmark(
  str_match_all(strings, phone),
  str_match_all2(strings, phone),
  stringr::str_match_all(strings, phone),
  times = 500L
)


microbenchmark::microbenchmark(
  str_match_all("This is a string", c("a", "i")),
  # str_match2("This is a string", c("a", "i")),
  stringr::str_match_all("This is a string", c("a", "i")),
  times = 500L
)

#### plot timings ####
# See: https://win-vector.com/2015/07/27/efficient-accumulation-in-r/
# and: http://winvector.github.io/Accumulation/Accum.html

# Make a list of vectors of increasing length,
# all of which contain fake phone numbers
vec_lens <- seq.int(100, 2000, 100)
strings <- vector("list", length = length(vec_lens))
for (i in seq_along(vec_lens)) {
  strings[[i]] <- charlatan::ch_phone_number(n = vec_lens[[i]])
}

# Make a list to contain the timing results
timings <- vector("list", length = length(vec_lens))

# Perform the timing
for (i in seq_along(vec_lens)) {
  time_taken <- microbenchmark(
    str_match = str_match(strings[[i]], phone),
    str_match2 = str_match2(strings[[i]], phone),
    stringr = stringr::str_match(strings[[i]], phone),
    times = 20L
  )

  result <- data.frame(time_taken)
  result$string_length <- vec_lens[i]
  timings[[i]] <- result
}

plot_timings <- function(df, x_var) {
  # Expect df to have 3 columns:
  # 1. 'expr' = the function called, to plot as colour
  # 2. 'time' = time taken for function call, to plot on y axis
  # 3. x_var e.g. 'string_length' set above = variable to plot on x axis

  df$exprs <- reorder(df$expr, -df$time, FUN = max)

  # Plot x axis vs time, linear scale
  p1 <- ggplot(df, aes(x = !!sym(x_var), y = time, colour = expr)) +
    geom_point(alpha = 0.8) +
    geom_smooth(alpha = 0.8) +
    labs(colour = "Function",
         y = "Time (µs?)")

  # Plot x axis vs time, log10 scale
  p2 <- p1 +
    scale_y_log10()

  # Plot boxplot comparing median times for max x axis value
  x_max <- max(df[[x_var]])
  df_subset <- df[df[[x_var]] == x_max, ]
  df_subset$expr <- reorder(df_subset$expr, df_subset$time, FUN = median)

  p3 <- ggplot(df_subset, aes(x = time, y = exprs)) +
    geom_boxplot(outlier.shape = NA) +
    geom_jitter() +
    labs(x = "Time (µs?)",
         y = "Function")

  print(list(p1, p2, p3))
}

to_plot <- do.call(rbind, timings)

# Hadley's str_match is slightly faster than mine,
# but I don't know if it works correctly for edge cases...
plot_timings(to_plot, "string_length")
