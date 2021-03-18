#' Extract matching patterns from a string
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
#'
#' @export
str_extract <- function(string, pattern) {
  check_pattern(pattern)

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
