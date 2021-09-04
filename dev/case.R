str_to_upper <- function(string, locale = "en_GB") {
  cur_locale <- Sys.getlocale(category = "LC_CTYPE")
  on.exit(Sys.setlocale(category = "LC_CTYPE", locale = cur_locale), add = TRUE)

  Sys.setlocale(category = "LC_CTYPE", locale = locale)
  toupper(string)
}

str_to_lower <- function(string, locale = "en_GB") {
  cur_locale <- Sys.getlocale(category = "LC_CTYPE")
  on.exit(Sys.setlocale(category = "LC_CTYPE", locale = cur_locale), add = TRUE)

  Sys.setlocale(category = "LC_CTYPE", locale = locale)
  tolower(string)
}

str_to_title <- function(string, locale = "en_GB") {
  cur_locale <- Sys.getlocale(category = "LC_CTYPE")
  on.exit(Sys.setlocale(category = "LC_CTYPE", locale = cur_locale), add = TRUE)

  Sys.setlocale(category = "LC_CTYPE", locale = locale)
  # https://stackoverflow.com/questions/51857207
  # gsub("(*UCP)\\b(\\p{Ll})", "\\U\\1", tolower(string), perl = TRUE)

  # https://stackoverflow.com/questions/6364783
  tools::toTitleCase(tolower(string))
}

str_to_sentence <- function(string, locale = "en_GB") {
  cur_locale <- Sys.getlocale(category = "LC_CTYPE")
  on.exit(Sys.setlocale(category = "LC_CTYPE", locale = cur_locale), add = TRUE)

  Sys.setlocale(category = "LC_CTYPE", locale = locale)

  # https://stackoverflow.com/questions/18509527
  out <- tolower(string)
  substr(out, 1, 1) <- toupper(substr(out, 1, 1))
  out
}
