#' Extract match from a string using a pattern
#'
#' @description Vectorised over `string`, but not `pattern` which must be
#' a single string (unlike stringr).
#'
#' @param string `character vector` of strings.
#' @param pattern `character`, a pattern to match. Can be:
#'   * A Perl-compatible regular expression (default).
#'   * Wrap with `perl(ignore_case = TRUE)` to use case-insensitive matching.
#'   * Wrap with `fixed()` to use a fixed/literal match.
#'   * Wrap with `regex()` to use a POSIX 1003.2 extended regular expression.
#'   * Wrap with `regex(ignore_case = TRUE)` to use case-insensitive matching with
#'     a POSIX 1003.2 extended regular expression.
#'
#' @return Returns a `character vector` the same length as `string`. Non-matching
#' elements will be replaced with `NA`.
#'
#' @examples
#' shopping_list <- c("apples x4", "bag of flour", "bag of sugar", "milk x2")
#' str_extract(shopping_list, "\\d")
#' str_extract(shopping_list, "[a-z]+")
#' str_extract(shopping_list, "[a-z]{1,4}")
#' str_extract(shopping_list, "\\b[a-z]{1,4}\\b")
#'
#' @export
str_extract <- function(string, pattern) {
  check_lengths(string, pattern)

  out <- rep(NA, length(string))
  r <- regexpr(
    pattern = pattern,
    text = string,
    ignore.case = ignore_case(pattern),
    perl = is_perl(pattern),
    fixed = is_fixed(pattern),
    useBytes = FALSE
  )

  out[r != -1] <- regmatches(string, r)
  out
}

#' Extract all matches from a string using a pattern
#'
#' @description Vectorised over `string`, but not `pattern` which must be
#' a single string (unlike stringr).
#'
#' @param string `character vector` of strings.
#' @param pattern `character`, a pattern to match. Can be:
#'   * A Perl-compatible regular expression (default).
#'   * Wrap with `perl(ignore_case = TRUE)` to use case-insensitive matching.
#'   * Wrap with `fixed()` to use a fixed/literal match.
#'   * Wrap with `regex()` to use a POSIX 1003.2 extended regular expression.
#'   * Wrap with `regex(ignore_case = TRUE)` to use case-insensitive matching with
#'     a POSIX 1003.2 extended regular expression.
#' @param simplify `logical`, should output list be simplified to a matrix?
#' Default is `FALSE`.
#'
#' @return Returns a `list` the same length as `string`, unless `simply = TRUE`
#' wherein a `character matrix` is returned with non-matching elements
#' replaced with `""`.
#'
#' @examples
#' shopping_list <- c("apples x4", "bag of flour", "bag of sugar", "milk x2")
#'
#' # Extract all matches
#' str_extract_all(shopping_list, "[a-z]+")
#' str_extract_all(shopping_list, "\\b[a-z]+\\b")
#' str_extract_all(shopping_list, "\\d")
#'
#' # Simplify results into character matrix
#' str_extract_all(shopping_list, "\\b[a-z]+\\b", simplify = TRUE)
#' str_extract_all(shopping_list, "\\d", simplify = TRUE)
#'
#' # Extract all words
#' str_extract_all("This is, suprisingly, a sentence.", "\\w+")
#'
#' @export
str_extract_all <- function(string, pattern, simplify = FALSE) {
  check_lengths(string, pattern)

  m <- regmatches(
    string,
    gregexpr(
      pattern = pattern,
      text = string,
      ignore.case = ignore_case(pattern),
      perl = is_perl(pattern),
      fixed = is_fixed(pattern),
      useBytes = FALSE
    )
  )

  if (!simplify) {
    m
  } else {
    list_length <- lengths(m)
    max_length <- max(list_length)

    out <- lapply(seq_along(m), function(x) {
      if (list_length[x] < max_length) {
        c(m[[x]], rep("", max_length - list_length[x]))
      } else {
        m[[x]]
      }
    })
    matrix(unlist(out, use.names = FALSE), ncol = max_length, byrow = TRUE)
  }
}
