fixed <- function(x) {
  structure(
    x,
    class = c("stringrb_fixed", "stringr_pattern", "character")
  )
}

is_fixed <- function(x) inherits(x, "stringrb_fixed")
