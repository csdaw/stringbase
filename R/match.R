str_match <- function(string, pattern) {
  stringrb:::check_lengths(string, pattern)
  p_len <- length(pattern)

  # Figure out how many groups there are.
  # Output is matrix with ngroups + 1 columns.
  tmp <- stringrb::str_replace_all(pattern, "\\\\\\(", "")
  tmp2 <- stringrb::str_replace_all(tmp, "\\(\\?:", "")
  ngroups <- nchar(stringrb::str_replace_all(tmp2, "[^(]", ""))

  if (p_len > 1) {
    matcher <- mapply(
      function(s, p) {
        regexec(
          pattern = p,
          text = s,
          ignore.case = stringrb:::ignore_case(p),
          perl = stringrb:::is_perl(p),
          fixed = stringrb:::is_fixed(p)
        )
      },
      string, pattern, USE.NAMES = FALSE
    )

    matches <- regmatches(
      x = if (length(string) == p_len) string else rep.int(string, p_len),
      m = matcher
    )

    # In this case, ngroups will be a vector but we only want max value
    ngroups <- max(ngroups)

  } else {
    matcher <- regexec(
      pattern = pattern,
      text = string,
      ignore.case = stringrb:::ignore_case(pattern),
      perl = stringrb:::is_perl(pattern),
      fixed = stringrb:::is_fixed(pattern)
    )

    matches <- regmatches(x = string, m = matcher)
  }

  matches[lengths(matches) == 0L] <- list(rep.int(NA_character_, ngroups + 1))

  matrix(as.character(unlist(matches)), ncol = ngroups + 1, byrow = TRUE)
}
