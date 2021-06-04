#' Join multiple strings into a single string
#'
#' @description
#' `str_c()` combines multiple character vectors into a single character
#' vector. It's very similar to [`paste0()`] but uses base R recycling and
#' tidyverse `NA` rules.
#'
#' One way to understand how `str_c()` works is picture a 2d matrix of strings,
#' where each argument forms a column. `sep` is inserted between each column,
#' and then each row is combined together into a single string. If `collapse`
#' is set, it's inserted between each row, and then the result is again
#' combined, this time into a single string.
#'
#' @param ... One or more `character vectors`.
#'
#'   `NULL`s are removed; scalar inputs (vectors of length 1) are recycled to
#'   the common length of vector inputs.
#'
#'   Like most other R functions, missing values are "infectious": whenever
#'   a missing value is combined with another string the result will always
#'   be missing. Use `dplyr::coalesce()` or `str_replace_na()` to convert
#'   desired value.
#' @param sep `string` to insert between input vectors.
#' @param collapse Optional `string` used to combine output into single
#'   string. Generally better to use `str_flatten()` if you needed this
#'   behaviour.
#' @return If `collapse = NULL` (the default), returns a character vector with
#'   length equal to the longest input. If `collapse` is a string, returns a
#'   character vector of length 1.
#' @export
#' @keywords internal
#' @examples
#' str_c("Letter: ", letters)
#' str_c("Letter", letters, sep = ": ")
#' str_c(letters, " is for", "...")
#' str_c(letters[-26], " comes before ", letters[-1])
#'
#' str_c(letters, collapse = "")
#' str_c(letters, collapse = ", ")
#'
#' # Differences from paste() ----------------------
#' # Missing inputs give missing outputs
#' str_c(c("a", NA, "b"), "-d")
#' paste0(c("a", NA, "b"), "-d")
#'
#' # Use str_replace_NA to display literal NAs:
#' # str_c(str_replace_na(c("a", NA, "b")), "-d")
#'
#' # Uses base R recycling rules
#' str_c(1:2, 1:3)
#' paste0(1:2, 1:3)
#'
#' str_c("x", character())
#' paste0("x", character())
str_c <- function(..., sep = "", collapse = NULL) {
  if (!is.character(sep) && length(sep) != 1) {
    stop("`sep` must be a single string")
  }

  if (!is.null(collapse) && !is.character(collapse) && length(collapse) != 1) {
    stop("`collapse` must be NULL or single string")
  }

  dots <- list(...)
  # check which of ... args is the longest
  long_arg <- which.max(lengths(dots))

  # perform the pasting
  out <- paste(..., sep = sep, collapse = collapse, recycle0 = FALSE)

  # if any of longest arg == NA, replace output with NA like tidyverse
  if(any(is.na(dots[[long_arg]]))) {
    na_index <- which(is.na(dots[[long_arg]]))
    out[na_index] <- NA_character_
  }
  out
}
