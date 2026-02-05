# A corpus of common misspellings, for examples and practice

This is a `tbl_df` mapping misspellings of their words, compiled by
Wikipedia, where it is licensed under the CC-BY SA license. (Three words
with non-ASCII characters were filtered out). If you'd like to reproduce
this dataset from Wikipedia, see the example code below.

## Usage

``` r
misspellings
```

## Format

An object of class `tbl_df` (inherits from `tbl`, `data.frame`) with
4505 rows and 2 columns.

## Source

<https://en.wikipedia.org/wiki/Wikipedia:Lists_of_common_misspellings/For_machines>

## Examples

``` r
# \donttest{
library(rvest)
library(readr)
#> 
#> Attaching package: ‘readr’
#> The following object is masked from ‘package:rvest’:
#> 
#>     guess_encoding
library(dplyr)
#> 
#> Attaching package: ‘dplyr’
#> The following objects are masked from ‘package:stats’:
#> 
#>     filter, lag
#> The following objects are masked from ‘package:base’:
#> 
#>     intersect, setdiff, setequal, union
library(stringr)
library(tidyr)

u <- "https://en.wikipedia.org/wiki/Wikipedia:Lists_of_common_misspellings/For_machines"
h <- read_html(u)

misspellings <- h %>%
  html_nodes("pre") %>%
  html_text() %>%
  read_delim(col_names = c("misspelling", "correct"),
                    delim = ">",
                    skip = 1) %>%
  mutate(misspelling = str_sub(misspelling,
                                               1, -2)) |>
  separate_rows(correct, sep = ", ") |>
  filter(Encoding(correct) != "UTF-8")
#> Warning: The `file` argument of `vroom()` must use `I()` for literal data as of vroom
#> 1.5.0.
#>   
#>   # Bad:
#>   vroom("X,Y\n1.5,2.3\n")
#>   
#>   # Good:
#>   vroom(I("X,Y\n1.5,2.3\n"))
#> ℹ The deprecated feature was likely used in the readr package.
#>   Please report the issue at <https://github.com/tidyverse/readr/issues>.
#> Rows: 4309 Columns: 2
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: ">"
#> chr (2): misspelling, correct
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
# }
```
