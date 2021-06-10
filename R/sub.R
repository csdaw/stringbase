#' Extract and replace substrings from a character vector
#'
#' `str_sub` will recycle all arguments to be the same length as the
#' longest argument. If any arguments are of length 0, the output will be
#' a zero length character vector.
#'
#' Substrings are inclusive - they include the characters at both start and
#' end positions. `str_sub(string, 1, -1)` will return the complete
#' substring, from the first character to the last.
#'
#' This function includes limited support for replacement form. Substring length
#' must match replacement length only one replacement can be made per string.
#' For multiple replacements per string it is probably better to use `mgsub()`
#' from the `mgsub` package.
#'
#' @param string `character vector` of strings.
#' @param start `integer vector` giving the position(s) of the first character,
#' default is `1L`. Negative values count backwards from the last character.
#' Alternatively pass a two-column `numeric matrix` e.g. the
#' output of `str_locate_all()`.
#' @param end `integer vector` giving the position(s) of the last character,
#' default is `1000000L`. Negative values count backwards from the last character.
#' @param omit_na `logical`, default is `FALSE`, If `TRUE`, missing values in
#' any of the arguments provided will result in an unchanged input.
#' @param value replacement `string`
#' @return Returns a `character vector` of substring(s) from `start` to `end`
#' (inclusive). Will be length of longest input argument.
#' @examples
#' hw <- "Hadley Wickham"
#'
#' str_sub(hw, 1, 6)
#' str_sub(hw, end = 6)
#' str_sub(hw, 8, 14)
#' str_sub(hw, 8)
#' str_sub(hw, c(1, 8), c(6, 14))
#'
#' # Negative indices
#' str_sub(hw, -1)
#' str_sub(hw, -7)
#' str_sub(hw, end = -7)
#'
#' # Alternatively, you can pass in a two column matrix, as in the
#' # output from str_locate_all
#' pos <- str_locate_all(hw, "[aeio]")[[1]]
#' str_sub(hw, pos)
#' str_sub(hw, pos[, 1], pos[, 2])
#'
#' # Vectorisation
#' str_sub(hw, seq_len(nchar(hw)))
#' str_sub(hw, end = seq_len(nchar(hw)))
#'
#' # Replacement form
#' # (Limited support, substring length must match replacement length and
#' # only one replacement can be made per string.)
#' test <- c("red", "orange", "green", "blue")
#' str_sub(test, 1, 3) <- "333"
#' test
#'
#' test2 <- c("Africa", "Asia", "Australia")
#' str_sub(test2, -2) <- "z"
#' test2
#'
#' # If you want to keep the original string if some argument is NA,
#' # use omit_na = TRUE
#'
#' x1 <- x2 <- x3 <- x4 <- "AAA"
#' str_sub(x1, 1, NA) <- "B"
#' str_sub(x2, 1, 2) <- NA
#' str_sub(x3, 1, NA, omit_na = TRUE) <- "B"
#' str_sub(x4, 1, 2, omit_na = TRUE) <- NA
#' x1; x2; x3; x4
#'
#' @export
str_sub <- function(string, start = 1L, end = 1000000L) {
  if (is.matrix(start)) {
    end <- start[, 2]
    start <- start[, 1]
  }

  if(!missing(end) | is.matrix(start)) {
    vec_size_common(string, start, end)
  } else {
    vec_size_common(string, start)
  }

  if (all(!is.na(string), !is.na(start), !is.na(end))) {
    if (any(start < 0) | any(end < 0)) {
      # argument recycling
      n <- max(length(string), length(start), length(end))
      string <- rep_len(string, n)
      start <- rep_len(start, n)
      end <- rep_len(end, n)

      # dealing with negative integers
      nchars <- nchar(string)
      start[start < 0] <- nchars[start < 0] + 1 + start[start < 0]
      end[end < 0] <- nchars[end < 0] + 1 + end[end < 0]
    }
  }
  substring(string, start, end)
}

#' @export
#' @rdname str_sub
"str_sub<-" <- function(string, start = 1L, end = 1000000L, omit_na = FALSE, value) {
  if (is.matrix(start)) {
    end <- start[, 2]
    start <- start[, 1]
  }

  if(!missing(end) | is.matrix(start)) {
    vec_size_common(string, start, end)
  } else {
    vec_size_common(string, start)
  }

  if (all(!is.na(string), !is.na(start), !is.na(end))) {
    if (any(start < 0) | any(end < 0)) {
      # argument recycling
      n <- max(length(string), length(start), length(end))
      string <- rep_len(string, n)
      start <- rep_len(start, n)
      end <- rep_len(end, n)

      # dealing with negative integers
      nchars <- nchar(string)
      start[start < 0] <- nchars[start < 0] + 1 + start[start < 0]
      end[end < 0] <- nchars[end < 0] + 1 + end[end < 0]
    }
  }

  if (any(is.na(start), is.na(end), is.na(value))) {
    # argument recycling
    n <- max(length(string), length(start), length(end), length(value))

    if (any(is.na(value))) {
      # `substring<-` errors if value is NA so we have to deal with this separately
      out <- character(length = n)
      repl <- `substring<-`(string[!is.na(value)],
                            start[!is.na(value)],
                            end[!is.na(value)],
                            value[!is.na(value)])
      out[is.na(value)] <- ifelse(omit_na, string[is.na(value)], NA)
      out[!is.na(value)] <- repl
    } else {
      # `substring<-` can deal with NA start and end though
      out <- `substring<-`(string, start, end, value)
      if (omit_na) out[is.na(start) | is.na(end)] <- string[is.na(start) | is.na(end)]
    }
  } else {
    out <- "substring<-"(string, start, end, value)
  }
  out
}
