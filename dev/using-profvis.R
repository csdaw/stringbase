library(profvis)

set.seed(1)
strings <- charlatan::ch_phone_number(n = 10000L)
phone <- "([2-9][0-9]{2})[- .]([0-9]{3})[- .]([0-9]{4})"

profvis({
  str_match(strings, phone)
})

x <- rep(c('[hg19:21:34809787-34809808:+]',
           '[hg19:11:105851118-105851139:+]',
           '[hg19:17:7482245-7482266:+]',
           '[hg19:6:19839915-19839936:+]'), 1000)

microbenchmark::microbenchmark(
  stringr::str_match(x, '(\\[[^:]+:(\\d+):(\\d+)-(\\d+):([-+])])'),
  str_match(x, '(\\[[^:]+:(\\d+):(\\d+)-(\\d+):([-+])])')
)

profvis({
  str_match(x, '(\\[[^:]+:(\\d+):(\\d+)-(\\d+):([-+])])')
})
