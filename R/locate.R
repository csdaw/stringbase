location <- function(x, all = FALSE) {
  start <- as.vector(x)
  if (all && identical(start, -1L)) {
    return(cbind(start = integer(), end = integer()))
  }

  end <- as.vector(x) + attr(x, "match.length") - 1L

  no_match <- start == -1L
  start[no_match] <- NA_integer_
  end[no_match] <- NA_integer_

  cbind(start = start, end = end)
}

#' Locate the position of patterns in a string
#'
#' @description Vectorised over `string` and `pattern`, though using vectorised
#' patterns is relatively slow compared to `stringr`. If the match is of length
#' 0, (e.g. from a special match like `$`) end will be one character less
#' than start.
#'
#' @inheritParams str_detect
#' @return For `str_locate()`, an `integer matrix`. First column gives start
#'   postion of match, and second column gives end position. For
#'   `str_locate_all()` a `list` of integer matrices.
#' @seealso
#'   [str_extract()] for a convenient way of extracting matches.
#'
#' @examples
#' fruit <- c("apple", "banana", "pear", "pineapple")
#' str_locate(fruit, "$")
#' str_locate(fruit, "a")
#' str_locate(fruit, "e")
#' str_locate("apple", c("a", "b", "p", "p"))
#'
#' str_locate_all(fruit, "a")
#' str_locate_all(fruit, "e")
#'
#' # Find location of every character
#' str_locate_all(fruit, "")
#' @export
str_locate <- function(string, pattern) {
  check_lengths(string, pattern)

  if (length(pattern) > 1) {
    out <- mapply(
      function(p, s) {
        regexpr(p, s,
                fixed = is_fixed(p),
                perl = is_perl(p),
                ignore.case = ignore_case(p))
      },
      pattern,
      string,
      SIMPLIFY = FALSE,
      USE.NAMES = FALSE
    )
    do.call(rbind, lapply(out, location))

  } else {
    out <- regexpr(pattern, string,
                   fixed = is_fixed(pattern),
                   perl = is_perl(pattern),
                   ignore.case = ignore_case(pattern))
    location(out)
  }
}

#' @rdname str_locate
#' @export
str_locate_all <- function(string, pattern) {
  check_lengths(string, pattern)

  if (length(pattern) > 1) {
    out <- mapply(
      function(p, s) {
        gregexpr(p, s,
                 fixed = is_fixed(p),
                 perl = is_perl(p),
                 ignore.case = ignore_case(p))
      },
      pattern,
      string,
      SIMPLIFY = TRUE,
      USE.NAMES = FALSE
    )
    lapply(out, function(x) do.call(rbind, lapply(x, location, all = TRUE)))

  } else {
    out <- gregexpr(pattern, string,
                    fixed = is_fixed(pattern),
                    perl = is_perl(pattern),
                    ignore.case = ignore_case(pattern))
    lapply(out, location, all = TRUE)
  }
}
