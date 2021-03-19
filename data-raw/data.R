words <- rcorpora::corpora("words/common")$commonWords
fruit <- rcorpora::corpora("foods/fruits")$fruits
fruit <- fruit[!fruit %in% "tomato"]
sentences <- readr::read_lines("data-raw/harvard-sentences.txt")

usethis::use_data(words, overwrite = TRUE)
usethis::use_data(fruit, overwrite = TRUE)
usethis::use_data(sentences, overwrite = TRUE)
