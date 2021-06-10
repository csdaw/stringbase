#' Replace matched patterns in a string
#'
#' @description Vectorised over `string`, `pattern` and `replacement`.
#'
#' @inheritParams str_detect
#' @param pattern `string` or `character vector`, pattern(s) to match. Can be:
#'   * A Perl-compatible regular expression (default).
#'   * Wrap with `perl(ignore_case = TRUE)` to use case-insensitive matching.
#'   * Wrap with `fixed()` to use a fixed/literal match.
#'   * Wrap with `regex()` to use a POSIX 1003.2 extended regular expression.
#'   * Wrap with `regex(ignore_case = TRUE)` to use case-insensitive matching with
#'     a POSIX 1003.2 extended regular expression.
#'   * A __named__ `character vector` (see description in argument below)
#'
#'   * To perform multiple replacements in each element of `string`, use a
#'     named `character vector` in the format: `c("pattern1" = "replacement1")`
#' @param replacement `string` or `character vector` of replacements.
#'   * Should be either of length one or the same length as `string` or `pattern`.
#'   * Can also be a function (or formula) which will be called once for each
#'     match (from right to left) and its return value will be used to replace
#'     the match. Due to limitation with base R, a __warning__ is produced if
#'     the return value is longer than the match.
#'   * References of the form `\1`, `\2`, etc will be replaced with
#'     the contents of the respective matched group (created by `()`).
#'   * To perform multiple replacements in each element of `string`, leave
#'     use a named `character vector` for `pattern` in the format:
#'     `c("pattern1" = "replacement1")`.
#'   * To replace the complete string with `NA`, use `replacement = NA_character_`.
#' @return Returns a `character vector`.
#' @seealso `str_replace_na()` to turn missing values into `NA`.
#' @export
#' @examples
#' fruits <- c("one apple", "two pears", "three bananas")
#' str_replace(fruits, "[aeiou]", "-")
#' str_replace_all(fruits, "[aeiou]", "-")
#' str_replace_all(fruits, "b", NA_character_)
#'
#' str_replace(fruits, "([aeiou])", "")
#' str_replace(fruits, "([aeiou])", "\\1\\1")
#'
#' # Note that str_replace() is vectorised along text, pattern, and replacement
#' str_replace(fruits, "[aeiou]", c("1", "2", "3"))
#' str_replace(fruits, c("a", "e", "i"), "-")
#'
#' # If you want to apply multiple patterns and replacements to the same
#' # string, pass a named vector to pattern.
#' fruits2 <- "one apple---two pears---three bananas"
#' str_replace_all(fruits2, c("one" = "1", "two" = "2", "three" = "3"))
#'
#' # Use a function for more sophisticated replacement. This example converts
#' # all vowels into upper case.
#' str_replace_all(fruits, "[aeiou]", toupper)
#'
#' # If the function used for replacement outputs a longer string than the match
#' # it will replace, a warning is produced. This example tries to replace
#' # colour names with their hex values, which works with stringr but not with
#' # this package unfortunately.
#' colours <- str_c("\\b", colors(), "\\b", collapse="|")
#' col2hex <- function(col) {
#'   rgb <- col2rgb(col)
#'   rgb(rgb["red", ], rgb["green", ], rgb["blue", ], max = 255)
#' }
#'
#' x <- c(
#'   "Roses are red, violets are blue",
#'   "My favourite colour is green"
#' )
#' \dontrun{str_replace_all(x, colours, col2hex)} # produces warnings
str_replace <- function(string, pattern, replacement) {
  if (!missing(replacement) && is_replacement_fun(replacement)) {
    if (is_formula(replacement)) {
      stop("using a formula for replacement has not been implemented in stringrb.")
    }
    replacement <- as.function(replacement)
    return(str_transform(string, pattern, replacement))
  }

  if (!is.character(pattern)) stop("`pattern` must be a string or character vector")
  if (!is.character(replacement)) stop("`replacement` must be a string or character vector")

  check_lengths(string, pattern)

  if (length(replacement) > 1 | length(pattern) > 1) {
    out <- mapply(
      sub,
      pattern = pattern,
      replacement = replacement,
      x = string,
      USE.NAMES = FALSE
    )
  } else {
    out <- sub(
      pattern, replacement, string,
      fixed = is_fixed(pattern),
      perl = is_perl(pattern),
      ignore.case = ignore_case(pattern)
    )
  }
  out
}

str_transform <- function(string, pattern, replacement) {
  loc <- str_locate(string, pattern)
  str_sub(string, loc, omit_na = TRUE) <- replacement(str_sub(string, loc))
  string
}

#' @rdname str_replace
#' @export
str_replace_all <- function(string, pattern, replacement) {
  if (!missing(replacement) && is_replacement_fun(replacement)) {
    if (is_formula(replacement)) {
      stop("using a formula for replacement has not been implemented in stringrb.")
    }
    replacement <- as.function(replacement)
    return(str_transform_all(string, pattern, replacement))
  }


  if (!is.character(pattern)) stop("`pattern` must be a string or character vector")
  if (!missing(replacement) && !is.character(replacement)) stop("`replacement` must be a string or character vector")

  if (!is.null(names(pattern))) {
    replacement <- unname(pattern)
    pattern[] <- names(pattern)
    return(Reduce(function(x, i) {
      gsub(
        pattern = paste0(pattern[i]), replacement = replacement[i], x = x,
        fixed = is_fixed(pattern),
        perl = is_perl(pattern),
        ignore.case = ignore_case(pattern)
      )
    }, seq_along(pattern), string))
  } else {
    check_lengths(string, pattern, replacement)
  }

  if (length(replacement) > 1 | length(pattern) > 1) {
    mapply(gsub, replacement = replacement, x = string, pattern = pattern, USE.NAMES = FALSE)
  } else {
    gsub(
      pattern = pattern, replacement = replacement, x = string,
      fixed = is_fixed(pattern),
      perl = is_perl(pattern),
      ignore.case = ignore_case(pattern)
    )
  }
}

str_transform_all <- function(string, pattern, replacement) {
  locs <- str_locate_all(string, pattern)

  for (i in seq_along(string)) {
    for (j in rev(seq_len(nrow(locs[[i]])))) {
      loc <- locs[[i]]

      if (length(c(loc[j, 1]:loc[j, 2])) < nchar(replacement(str_sub(string[[i]], loc[j, 1], loc[j, 2])))) {
        warning("output of `replacement` function is longer than match and has been truncated.")
      }
      str_sub(string[[i]], loc[j, 1], loc[j, 2]) <- replacement(str_sub(string[[i]], loc[j, 1], loc[j, 2]))
    }
  }
  string
}

#' Turn NA into "NA"
#'
#' @inheritParams str_replace
#' @param replacement a single `string`.
#' @export
#' @examples
#' str_replace_na(c(NA, "abc", "def"))
str_replace_na <- function(string, replacement = "NA") {
  if (!is_string(replacement)) stop("`replacement` must be a single string")
  string[is.na(string)] <- replacement
  string
}
