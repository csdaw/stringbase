regex <- function(x, ignore_case = FALSE) {
  structure(
    x,
    ignore_case = ignore_case,
    class = c("sb_regex", "stringr_pattern", "character")
  )
}
