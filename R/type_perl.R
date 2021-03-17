perl <- function(x, ignore_case = FALSE) {
  structure(
    x,
    ignore_case = ignore_case,
    class = c("stringrb_perl", "stringr_pattern", "character")
  )
}

is_perl <- function(x) inherits(x, "stringrb_perl") || is.null(attr(x, "class"))
