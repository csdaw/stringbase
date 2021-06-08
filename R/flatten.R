#' Flatten a string
#'
#' @description This is a summary function for strings: regardless of the
#' length of the input, it always returns a single string.
#'
#' @param string `character vector` to flatten.
#' @param collapse `string` to insert between each element.
#' @param last (Optional) `string` use in place of final separator.
#' @return Returns a `character vector` of length 1
#' @export
#' @examples
#' str_flatten(letters)
#' str_flatten(letters, "-")
#'
#' str_flatten(letters[1:5], ", ", ", and ")
#' str_flatten(letters[1:3], ", ", ", and ")
#' str_flatten(letters[1:2], ", ", ", and ")
#' str_flatten(letters[1], ", ", ", and ")
#' str_flatten(letters[0], ", ", ", and ")
str_flatten <- function(string, collapse = "", last = NULL) {
  if (!is_string(collapse)) {
    stop("`collapse` must be a single string.")
  }

  n <- length(string)
  if (!is.null(last) && n >= 2) {
    string <- c(
      string[seq2(1, n - 2)],
      str_c(string[[n - 1]], last, string[[n]])
    )
  }

  paste(string, collapse = collapse, sep = " ")
}
