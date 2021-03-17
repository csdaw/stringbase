check_pattern <- function(x) {
  if (!is.character(x) || length(x) != 1) {
    stop("`pattern` must be a single string", call. = FALSE)
  }

  if (is.na(x)) {
    stop("`pattern` can't be NA", call. = FALSE)
  }
}
