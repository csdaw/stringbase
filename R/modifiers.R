fixed <- function(x) {
  structure(
    x,
    class = c("stringrb_fixed", "stringr_pattern", "character")
  )
}

is_fixed <- function(x) inherits(x, "stringrb_fixed")

ignore_case <- function(x) isTRUE(attr(x, "ignore_case"))

perl <- function(x, ignore_case = FALSE) {
  structure(
    x,
    ignore_case = ignore_case,
    class = c("stringrb_perl", "stringr_pattern", "character")
  )
}

is_perl <- function(x) inherits(x, "stringrb_perl") || is.null(attr(x, "class"))

regex <- function(x, ignore_case = FALSE) {
  structure(
    x,
    ignore_case = ignore_case,
    class = c("stringrb_regex", "stringr_pattern", "character")
  )
}
