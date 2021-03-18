#' Control pattern matching with modifier functions
#'
#' @description
#' \describe{
#'   \item{`fixed()`}{Match the string literally.}
#'   \item{`regex()`}{Match using a POSIX 1003.2 extended regular expression.}
#'   \item{`perl()`}{The default, match using a perl-compatible regular expression.}
#' }
#'
#' @param pattern `character`, a pattern to modify the behaviour of.
#' @param ignore_case `logical`, should case-insensitive matching be used?
#' Default is `FALSE`.
#' @name modifiers
#'
#' @examples
#'
#' @export
perl <- function(pattern, ignore_case = FALSE) {
  structure(
    pattern,
    ignore_case = ignore_case,
    class = c("stringrb_perl", "stringr_pattern", "character")
  )
}

is_perl <- function(x) inherits(x, "stringrb_perl") || is.null(attr(x, "class"))

#' @rdname modifiers
#' @export
fixed <- function(pattern) {
  structure(
    pattern,
    class = c("stringrb_fixed", "stringr_pattern", "character")
  )
}

is_fixed <- function(x) inherits(x, "stringrb_fixed")

ignore_case <- function(x) isTRUE(attr(x, "ignore_case"))

#' @rdname modifiers
#' @export
regex <- function(pattern, ignore_case = FALSE) {
  structure(
    pattern,
    ignore_case = ignore_case,
    class = c("stringrb_regex", "stringr_pattern", "character")
  )
}
