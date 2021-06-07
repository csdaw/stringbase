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
