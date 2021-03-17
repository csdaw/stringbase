perl <- function(x, ignore_case = FALSE) {
  structure(
    x,
    ignore_case = ignore_case,
    class = c("sb_perl", "stringr_pattern", "character")
  )
}

is_perl <- function(x) inherits(x, "sb_perl") || is.null(attr(x, "class"))
