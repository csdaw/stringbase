library(here)
source(here("R/match.R"))
source(here("R/regmatches.R"))

strings <- c(" 219 733 8965", "329-293-8753 ", "banana", "595 794 7569",
             "387 287 6718", "apple", "233.398.9187  ", "482 952 3315",
             "239 923 8115 and 842 566 4692", "Work: 579-499-7527", "$1000",
             "Home: 543.355.3679")
phone <- "([2-9][0-9]{2})[- .]([0-9]{3})[- .]([0-9]{4})"

# Extract/match all
str_match_all(strings, phone)
stringr::str_match_all(strings, phone)

x <- c("<a> <b>", "<a> <>", "<a>", "", NA)


str_match(x, "<(.*?)>")
stringr::str_match(x, "<(.*?)>")

str_match_all(x, "<(.*?)>")
stringr::str_match_all(x, "<(.*?)>")
