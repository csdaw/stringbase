#' Detect the presence or absence of a pattern in a string
#'
#' @description Vectorised over `string`, but not `pattern` which must be
#' a single string (unlike stringr). Equivalent to `grepl(pattern, x)`. See
#' `str_which()` for an equivalent to `grep(pattern, x)`.
#'
#' @param string `character vector` of strings.
#' @param pattern `character`, a pattern to match. Can be:
#'   * A Perl-compatible regular expression (default).
#'   * Wrap with `perl(ignore_case = TRUE)` to use case-insensitive matching.
#'   * Wrap with `fixed()` to use a fixed/literal match.
#'   * Wrap with `regex()` to use a POSIX 1003.2 extended regular expression.
#'   * Wrap with `regex(ignore_case = TRUE)` to use case-insensitive matching with
#'     a POSIX 1003.2 extended regular expression.
#' @param negate `logical`, if `TRUE` return non-matching elements. Default is
#' `FALSE`.
#'
#' @return Returns a `logical vector` the same length as `string`.
#'
#' @examples
#' fruit <- c("apple", "banana", "pear", "pineapple")
#' str_detect(fruit, "a")
#' str_detect(fruit, "^a")
#' str_detect(fruit, "a$")
#' str_detect(fruit, "b")
#' str_detect(fruit, "[aeiou]")
#'
#' # Returns TRUE if the pattern do NOT match
#' str_detect(fruit, "^p", negate = TRUE)
#'
#' @export
str_detect <- function(string, pattern, negate = FALSE) {
  check_pattern(pattern)

  out <- grepl(pattern, string,
               fixed = is_fixed(pattern),
               perl = is_perl(pattern),
               ignore.case = ignore_case(pattern)
  )
  out[is.na(string)] <- NA

  if (negate) {
    !out
  } else {
    out
  }
}
