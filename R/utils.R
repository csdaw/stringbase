is_string <- function(x) is.character(x) && length(x) == 1

check_lengths <- function(string, pattern, replacement = NULL) {
  if (!is.null(replacement)) {
    input <- list(string, pattern, replacement)
  } else {
    input <- list(string, pattern)
  }

  in_lengths <- lengths(input)
  in_names <- names(as.list(match.call()[-1]))

  if (any(in_lengths == 0)) {
    if (all(in_lengths == 0 | in_lengths == 1)) return(character())
  }

  max_length <- max(in_lengths)
  max_name <- in_names[which.max(in_lengths)]

  if (!all(in_lengths == 1 | in_lengths == max_length)) {
    stop(sprintf("Can't recycle `%s` (size %i) to match `%s` (size %i).",
                 in_names[in_lengths != 1 & in_lengths != max_length][1],
                 in_lengths[in_lengths != 1 & in_lengths != max_length][1],
                 max_name,
                 max_length))
  } else {
    max_length
  }
}

vec_size_common <- function(string, start, end = NULL) {
  if (!is.null(end)) {
    input <- list(string, start, end)
  } else {
    input <- list(string, start)
  }

  in_lengths <- lengths(input)
  in_names <- names(as.list(match.call()[-1]))

  if (any(in_lengths == 0)) {
    if (all(in_lengths == 0 | in_lengths == 1)) return(character())
  }

  max_length <- max(in_lengths)
  max_name <- in_names[which.max(in_lengths)]

  if (!all(in_lengths == 1 | in_lengths == max_length)) {
    stop(sprintf("Can't recycle `%s` (size %i) to match `%s` (size %i).",
                 in_names[in_lengths != 1 & in_lengths != max_length][1],
                 in_lengths[in_lengths != 1 & in_lengths != max_length][1],
                 max_name,
                 max_length))
  } else {
    max_length
  }
}

#' Increasing sequence of integers in an interval
#'
#' @description These helpers take two endpoints and return the sequence of all
#' integers within that interval. For `seq2_along()`, the upper
#' endpoint is taken from the length of a vector. Unlike
#' `base::seq()`, they return an empty vector if the starting point is
#' a larger integer than the end point.
#'
#' @param from `integer`, the starting point of the sequence.
#' @param to `integer`, the end point.
#' @param x A `vector` whose length is the end point.
#' @return Returns an `integer vector` containing a strictly increasing
#'   sequence.
#' @keywords internal
#' @export
#' @examples
#' seq2(2, 10)
#' seq2(10, 2)
#' seq(10, 2)
#'
#' seq2_along(10, letters)
seq2 <- function(from, to) {
  if (length(from) != 1) {
    stop("`from` must be length one")
  }
  if (length(to) != 1) {
    stop("`to` must be length one")
  }

  if (from > to) {
    integer()
  } else {
    seq.int(from, to)
  }
}

#' @rdname seq2
#' @keywords internal
#' @export
seq2_along <- function(from, x) {
  seq2(from, length(x))
}
