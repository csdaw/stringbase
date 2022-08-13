str_match <- function(string, pattern) {
  stringrb:::check_lengths(string, pattern)
  p_len <- length(pattern)

  # Figure out how many groups there are.
  # Output is matrix with ngroups + 1 columns.
  tmp <- stringrb::str_replace_all(pattern, "\\\\\\(", "")
  tmp2 <- stringrb::str_replace_all(tmp, "\\(\\?:", "")
  ngroups <- nchar(stringrb::str_replace_all(tmp2, "[^(]", "")) + 1

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

  matches[lengths(matches) == 0L] <- list(rep.int(NA_character_, ngroups))

  matrix(as.character(unlist(matches)), ncol = ngroups, byrow = TRUE)
}

str_match_all <- function(string, pattern) {
  stringrb:::check_lengths(string, pattern)
  p_len <- length(pattern)

  # Figure out how many groups there are.
  # Output is matrix with ngroups + 1 columns.
  tmp <- stringrb::str_replace_all(pattern, "\\\\\\(", "")
  tmp2 <- stringrb::str_replace_all(tmp, "\\(\\?:", "")
  ngroups <- nchar(stringrb::str_replace_all(tmp2, "[^(]", "")) + 1

  if (p_len > 1) {
    matcher <- mapply(
      function(s, p) {
        gregexpr(
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

    mapply(function(s, p, n) {
      if (length(s) == 0) return(matrix(character(), ncol = n, byrow = TRUE))

      matcher2 <- regexec(
        pattern = p,
        text = s,
        ignore.case = stringrb:::ignore_case(p),
        perl = stringrb:::is_perl(p),
        fixed = stringrb:::is_fixed(p)
      )

      matches2 <- regmatches(x = s, m = matcher2)

      matrix(as.character(unlist(matches2)), ncol = n, byrow = TRUE)
    }, matches, pattern, ngroups, USE.NAMES = FALSE)
  } else {
    matcher <- gregexpr(
      pattern = pattern,
      text = string,
      ignore.case = stringrb:::ignore_case(pattern),
      perl = stringrb:::is_perl(pattern),
      fixed = stringrb:::is_fixed(pattern)
    )

    matches <- regmatches(x = string, m = matcher, na_return = NA_character_)

    mapply(function(s, n) {
      if (length(s) == 0) return(matrix(character(), ncol = n, byrow = TRUE))

      matcher2 <- regexec(
        pattern = pattern,
        text = s,
        ignore.case = stringrb:::ignore_case(pattern),
        perl = stringrb:::is_perl(pattern),
        fixed = stringrb:::is_fixed(pattern)
      )

      matches2 <- regmatches(x = s, m = matcher2, na_return = NA_character_)

      matrix(as.character(unlist(matches2)), ncol = n, byrow = TRUE)
    }, matches, ngroups, USE.NAMES = FALSE)
  }
}
