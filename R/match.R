#' Extract matched groups from a string
#'
#' @description Vectorised over `string` and `pattern`.
#'
#' @inheritParams str_detect
#' @param pattern `string` or `character vector`, pattern(s) to match. Typically
#'   contains multiple capture groups. Can be:
#'   * A Perl-compatible regular expression (default).
#'   * Wrap with `perl(ignore_case = TRUE)` to use case-insensitive matching.
#'   * Wrap with `fixed()` to use a fixed/literal match.
#'   * Wrap with `regex()` to use a POSIX 1003.2 extended regular expression.
#'   * Wrap with `regex(ignore_case = TRUE)` to use case-insensitive matching with
#'     a POSIX 1003.2 extended regular expression.
#' @return Returns a `character matrix` for `str_match()`. The first column is
#'   the complete match, followed by one column for each capture group.
#'   Returns a `list` of `character matrices` for `str_match_all()`.
#'
#' @seealso [str_extract()] to extract the complete match.
#' @export
#' @examples
#' strings <- c(" 219 733 8965", "329-293-8753 ", "banana", "595 794 7569",
#'   "387 287 6718", "apple", "233.398.9187  ", "482 952 3315",
#'   "239 923 8115 and 842 566 4692", "Work: 579-499-7527", "$1000",
#'   "Home: 543.355.3679")
#' phone <- "([2-9][0-9]{2})[- .]([0-9]{3})[- .]([0-9]{4})"
#'
#' str_extract(strings, phone)
#' str_match(strings, phone)
#'
#' # Extract/match all
#' str_extract_all(strings, phone)
#' #str_match_all(strings, phone)
#'
#' x <- c("<a> <b>", "<a> <>", "<a>", "", NA)
#' str_match(x, "<(.*?)> <(.*?)>")
#' #str_match_all(x, "<(.*?)>")
#'
#' str_extract(x, "<.*?>")
#' str_extract_all(x, "<.*?>")
str_match <- function(string, pattern) {
  if (is_fixed(pattern)) stop("Can only match regular expressions")
  check_lengths(string, pattern)

  if (length(pattern) > 1) {
    loc <- mapply(
      function(p, s) {
        regexec(
          text = s,
          pattern = p,
          ignore.case = ignore_case(pattern),
          perl = is_perl(pattern),
          fixed = FALSE
        )

      },
      pattern, string,
      USE.NAMES = FALSE
    )
    out <- regmatches(
      if (length(string) == length(pattern)) string else rep(string, length(pattern)),
      loc
    )
    mat_nrow <- length(pattern)
    mat_ncol <- if (all(is.na(string))) 2 else max(lengths(out))

  } else {
    loc <- regexec(
      pattern = pattern,
      text = string,
      ignore.case = ignore_case(pattern),
      perl = is_perl(pattern),
      fixed = FALSE
    )
    out <- regmatches(string, loc)
    mat_nrow <- length(string)
    mat_ncol <- if (all(is.na(string))) 2 else max(lengths(out))
  }

  out_mat <- matrix(NA_character_, nrow = mat_nrow, ncol = mat_ncol)
  for (i in which(lengths(out) != 0)) {
    # replace each matrix row with list elements that don't have a
    # start position == 0 and match.length == 0
    out_mat[i, ][!(loc[[i]] == 0 & attr(loc[[i]], "match.length") == 0)] <- out[[i]][!(loc[[i]] == 0 & attr(loc[[i]], "match.length") == 0)]
  }
  out_mat
}

#' @rdname str_match
#' @export
str_match_all <- function(string, pattern) {
  xxx <- gregexpr(pattern, string)

  xxx_lengths <- lengths(xxx)
  xxx_lengths[xxx %in% c(-1)] <- 0

  yyy <- regmatches(string, xxx)

  zzz <- lapply(yyy, function(m) do.call(rbind, regmatches(m, regexec(pattern, m))))

  out <- lapply(xxx_lengths, function(r) matrix(NA_character_, nrow = r, ncol = max(lengths(zzz))))
  out[!xxx %in% c(-1, NA)] <- zzz[!xxx %in% c(-1, NA)]
  out
}
