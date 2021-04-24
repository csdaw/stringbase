str_sub <- function(string, start = 1L, end = 1000000L) {
  if (is.matrix(start)) {
    end <- start[, 2]
    start <- start[, 1]
  }

  if (!any(start < 0) & !any(end < 0)) {
    substring(string, start, end)
  } else {
    n <- max(lt <- length(string), length(start), length(end))

    if (lt && lt < n) string <- rep_len(string, length.out = n)

    nchars <- nchar(string)

    start[start < 0] <- nchars[start < 0] + 1 + start[start < 0]
    end[end < 0] <- nchars[end < 0] + 1 + end[end < 0]

    substring(string, start, end)
  }
}
