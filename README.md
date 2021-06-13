
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
-   `str_replace()`, `str_replace_all()`, `str_replace_na()`
-   `str_sub()`
-   `str_trim()`, `str_squish()`

## To do

1.  `str_match()`
2.  etc…
3.  Add `ignore_case` argument to `fixed()`? (see stringr test-detect.R)

## Differences compared with stringr

-   Generally slower, though this varies across each function
-   Using `str_sub()` replacement form with functions is more limited
-   `str_replace()` and `str_replace_all()` won’t work with formulas
