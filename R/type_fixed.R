fixed <- function(x) {
  structure(
    x,
    class = c("sb_fixed", "stringr_pattern", "character")
  )
}

is_fixed <- function(x) inherits(x, "sb_fixed")
