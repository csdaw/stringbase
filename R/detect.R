#' Detect the presence or absence of a pattern in a string
#'
#' @description Vectorised over `string` and `pattern`, though using vectorised
#' patterns is relatively slow compared to `stringr`.
#' Equivalent to `grepl(pattern, x)`.
#' See `str_which()` for an equivalent to `grep(pattern, x)`.
#'
#' @param string `character vector` of strings.
#' @param pattern `string` or `character vector`, pattern(s) to match. Can be:
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
#' # Also vectorised over pattern
#' str_detect("aecfg", letters)
#'
#' # Returns TRUE if the pattern do NOT match
#' str_detect(fruit, "^p", negate = TRUE)
#'
#' @export
str_detect <- function(string, pattern, negate = FALSE) {
  check_lengths(string, pattern)

  if (length(pattern) > 1) {
    out <- mapply(
      function(p, s) {
        grepl(p, s,
              fixed = is_fixed(pat),
              perl = is_perl(pat),
              ignore.case = ignore_case(pat))
      },
      pattern,
      string,
      SIMPLIFY = TRUE,
      USE.NAMES = FALSE
    )
  } else {
    out <- grepl(pattern, string,
                 fixed = is_fixed(pattern),
                 perl = is_perl(pattern),
                 ignore.case = ignore_case(pattern))
  }

  out[is.na(string)] <- NA

  if (negate) {
    !out
  } else {
    out
  }
}

#' Detect the presence or absence of a pattern at the beginning or end of a
#' string.
#'
#' @description Vectorised over `string` and `pattern`, though using vectorised
#' patterns is relatively slow compared to `stringr`.
#'
#' @param string `character vector` of strings.
#' @param pattern `character`, a pattern with which the string should start
#' or end.
#'
#' Can be:
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
#' @seealso [str_detect()] which this function wraps.
#' @examples
#' fruit <- c("apple", "banana", "pear", "pineapple")
#' str_starts(fruit, "p")
#' str_starts(fruit, "p", negate = TRUE)
#' str_ends(fruit, "e")
#' str_ends(fruit, "e", negate = TRUE)
#'
#' @export
str_starts <- function(string, pattern, negate = FALSE) {
  check_lengths(string, pattern)

  pattern2 <- paste0("^(", pattern, ")")
  attributes(pattern2) <- attributes(pattern)
  str_detect(string = string, pattern = pattern2, negate = negate)
}

#' @rdname str_starts
#' @export
str_ends <- function(string, pattern, negate = FALSE) {
  check_lengths(string, pattern)

  pattern2 <- paste0("(", pattern, ")$")
  attributes(pattern2) <- attributes(pattern)
  str_detect(string = string, pattern = pattern2, negate = negate)
}
