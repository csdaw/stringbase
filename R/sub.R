str_sub <- function(string, start = 1L, end = 1000000L) {
  if (!any(start < 0) & !any(end < 0)) {
    substring(string, start, end)
  } else {
    n <- max(lt <- length(string), length(start), length(end))

    if (lt && lt < n)
      string <- rep_len(string, length.out = n)

    nchars <- sapply(string, nchar)
    start_new <- mapply(function(n, st) ifelse(st < 0, n + 1 + st, st),
                        nchars, start, USE.NAMES = FALSE, SIMPLIFY = TRUE)
    end_new <- mapply(function(n, en) ifelse(en < 0, n + 1 + en, en),
                      nchars, end, USE.NAMES = FALSE, SIMPLIFY = TRUE)

    substring(string, start_new, end_new)
  }
}
