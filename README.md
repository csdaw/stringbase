
<!-- README.md is generated from README.Rmd. Please edit that file -->

# stringrb

<!-- badges: start -->
<!-- badges: end -->

An imperfect replication of
[stringr](https://github.com/tidyverse/stringr) in base R.

Builds upon [hadley/stringb](https://github.com/hadley/stringb) and
[petermeissner/stringb](https://github.com/petermeissner/stringb).

## Completed

-   `str_c()`
-   `str_detect()`, `str_starts()`, `str_ends()`
-   `str_extract()`, `str_extract_all()`
-   `str_locate()`, `str_locate_all()`, `invert_match()`
-   `str_sub()`

## To do

1.  `str_replace()` and `str_replace_all()`
2.  `str_trim()`
3.  `str_match()`
4.  etc…
5.  Add `ignore_case` argument to `fixed()`? (see stringr test-detect.R)

## Differences compared with stringr

-   `str_sub()` replacement form is far more limited compared with
    stringr
