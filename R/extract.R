str_extract <- function(string, pattern) {
  check_pattern(pattern)

  regmatches(
    string,
    regexpr(
      pattern = pattern,
      text = string,
      ignore.case = ignore_case(pattern),
      perl = is_perl(pattern),
      fixed = is_fixed(pattern),
      useBytes = FALSE
    )
  )
}
