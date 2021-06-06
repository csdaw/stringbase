#' Join multiple strings into a single string
#'
#' @description
#' `str_c()` combines multiple character vectors into a single character
#' vector. It's very similar to [`paste0()`] but uses tidyverse recycling and
#' `NA` rules.
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
#' @param sep String to insert between input vectors.
#' @param collapse Optional string used to combine output into single
#'   string. Generally better to use `str_flatten()` if you needed this
#'   behaviour.
#' @return If `collapse = NULL` (the default) returns a `character vector` with
#'   length equal to the longest input. If `collapse` is a string, returns
#'   a `character vector` of length 1.
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
#' # Differences from paste0() ----------------------
#' # Missing inputs give missing outputs
#' str_c(c("a", NA, "b"), "-d")
#' paste0(c("a", NA, "b"), "-d")
#' # Use str_replace_NA to display literal NAs:
#' ####str_c(str_replace_na(c("a", NA, "b")), "-d")
#'
#' # Uses tidyverse recycling rules
#' \dontrun{str_c(1:2, 1:3)} # errors
#' paste0(1:2, 1:3)
#'
#' str_c("x", character())
#' paste0("x", character())
str_c <- function(..., sep = "", collapse = NULL) {
  if (!is_string(sep)) {
    stop("`sep` must be a single string")
  }
  if (!is.null(collapse) && !is_string(collapse)) {
    stop("`collapse` must be NULL or a single string")
  }

  dots <- list(...)

  if (length(dots) == 0) {
    return(character())
  }

  # ignore NULL values
  dots <- dots[!vapply(dots, is.null, logical(1L), USE.NAMES = FALSE)]

  # return zero length character if any input is of zero length
  dots_lengths <- lengths(dots)

  if (any(dots_lengths == 0)) {
    if (all(dots_lengths == 0 | dots_lengths == 1)) return(character())
  }

  # throw error if tidyverse recycling can't be followed
  dots_max <- max(dots_lengths)

  if (!all(dots_lengths == 1 | dots_lengths == dots_max)) {
    stop(sprintf("Can't recycle input vectors. They should be of length 1 or length %i.", dots_max))
  }

  # perform the pasting
  out <- paste(..., sep = sep, collapse = collapse, recycle0 = FALSE)

  # if any of longest arg == NA, replace output with NA like tidyverse
  if(any(is.na(dots[[which.max(dots_lengths)]]))) {
    na_index <- which(is.na(dots[[which.max(dots_lengths)]]))
    out[na_index] <- NA_character_
  }
  out
}
