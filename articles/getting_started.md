# Getting Started with fuzzystring

``` r
library(fuzzystring)
```

## Introduction

**fuzzystring** provides fast, flexible fuzzy string joins for data
frames using approximate string matching. Built on top of `data.table`
and `stringdist`, it’s designed for efficiently merging datasets where
exact matches aren’t possible due to misspellings, inconsistent
formatting, or slight variations in text.

## Installation

You can install the development version of **fuzzystring** from GitHub:

``` r
# Using pak (recommended)
# pak::pak("PaulESantos/fuzzystring")

# Or using remotes
# remotes::install_github("PaulESantos/fuzzystring")
```

## Quick Start

Here’s a simple example matching diamond cuts with slight misspellings:

``` r
# Your messy data
x <- data.frame(
  name = c("Idea", "Premiom", "Very Good"), 
  id = 1:3
)

# Reference data
y <- data.frame(
  approx_name = c("Ideal", "Premium", "VeryGood"), 
  grp = c("A", "B", "C")
)

# Fuzzy join with max distance of 2 edits
fuzzystring_inner_join(
  x, y,
  by = c(name = "approx_name"),
  max_dist = 2,
  distance_col = "distance"
)
#>         name    id approx_name    grp distance
#>       <char> <int>      <char> <char>    <num>
#> 1:      Idea     1       Ideal      A        1
#> 2:   Premiom     2     Premium      B        1
#> 3: Very Good     3    VeryGood      C        1
```

## Key Features

### All Join Types Supported

**fuzzystring** supports all standard join types. Below is a small,
reusable example dataset so you can compare the behavior of each join
family.

``` r
x_join <- data.frame(
  name = c("Idea", "Premiom", "Very Good", "Gooood"),
  id = 1:4
)

y_join <- data.frame(
  approx_name = c("Ideal", "Premium", "VeryGood", "Good"),
  grp = c("A", "B", "C", "D")
)
```

- [`fuzzystring_inner_join()`](https://paulesantos.github.io/fuzzystring/reference/fuzzystring_join.md):
  Only matching rows.
- [`fuzzystring_left_join()`](https://paulesantos.github.io/fuzzystring/reference/fuzzystring_join.md):
  All rows from `x`, matching rows from `y`.
- [`fuzzystring_right_join()`](https://paulesantos.github.io/fuzzystring/reference/fuzzystring_join.md):
  All rows from `y`, matching rows from `x`.
- [`fuzzystring_full_join()`](https://paulesantos.github.io/fuzzystring/reference/fuzzystring_join.md):
  All rows from both tables.
- [`fuzzystring_semi_join()`](https://paulesantos.github.io/fuzzystring/reference/fuzzystring_join.md):
  Rows from `x` that have a match in `y`.
- [`fuzzystring_anti_join()`](https://paulesantos.github.io/fuzzystring/reference/fuzzystring_join.md):
  Rows from `x` that don’t have a match in `y`.

#### Inner join

``` r
fuzzystring_inner_join(
  x_join, y_join,
  by = c(name = "approx_name"),
  max_dist = 2,
  distance_col = "distance"
)
#>         name    id approx_name    grp distance
#>       <char> <int>      <char> <char>    <num>
#> 1:      Idea     1       Ideal      A        1
#> 2:   Premiom     2     Premium      B        1
#> 3: Very Good     3    VeryGood      C        1
#> 4:    Gooood     4        Good      D        2
```

#### Left join

``` r
fuzzystring_left_join(
  x_join, y_join,
  by = c(name = "approx_name"),
  max_dist = 2,
  distance_col = "distance"
)
#>         name    id approx_name    grp distance
#>       <char> <int>      <char> <char>    <num>
#> 1:      Idea     1       Ideal      A        1
#> 2:   Premiom     2     Premium      B        1
#> 3: Very Good     3    VeryGood      C        1
#> 4:    Gooood     4        Good      D        2
```

#### Right join

``` r
fuzzystring_right_join(
  x_join, y_join,
  by = c(name = "approx_name"),
  max_dist = 2,
  distance_col = "distance"
)
#>         name    id approx_name    grp distance
#>       <char> <int>      <char> <char>    <num>
#> 1:      Idea     1       Ideal      A        1
#> 2:   Premiom     2     Premium      B        1
#> 3: Very Good     3    VeryGood      C        1
#> 4:    Gooood     4        Good      D        2
```

#### Full join

``` r
fuzzystring_full_join(
  x_join, y_join,
  by = c(name = "approx_name"),
  max_dist = 2,
  distance_col = "distance"
)
#>         name    id approx_name    grp distance
#>       <char> <int>      <char> <char>    <num>
#> 1:      Idea     1       Ideal      A        1
#> 2:   Premiom     2     Premium      B        1
#> 3: Very Good     3    VeryGood      C        1
#> 4:    Gooood     4        Good      D        2
```

#### Semi join (rows from `x` with a match in `y`)

``` r
fuzzystring_semi_join(
  x_join, y_join,
  by = c(name = "approx_name"),
  max_dist = 2
)
#>         name    id
#>       <char> <int>
#> 1:      Idea     1
#> 2:   Premiom     2
#> 3: Very Good     3
#> 4:    Gooood     4
```

#### Anti join (rows from `x` without a match in `y`)

``` r
fuzzystring_anti_join(
  x_join, y_join,
  by = c(name = "approx_name"),
  max_dist = 2
)
#> Empty data.table (0 rows and 2 cols): name,id
```

#### Using the generic `fuzzystring_join()`

If you prefer a single entry point, you can use
[`fuzzystring_join()`](https://paulesantos.github.io/fuzzystring/reference/fuzzystring_join.md)
directly by specifying `mode`.

``` r
fuzzystring_join(
  x_join, y_join,
  by = c(name = "approx_name"),
  max_dist = 2,
  mode = "left",
  distance_col = "distance"
)
#>         name    id approx_name    grp distance
#>       <char> <int>      <char> <char>    <num>
#> 1:      Idea     1       Ideal      A        1
#> 2:   Premiom     2     Premium      B        1
#> 3: Very Good     3    VeryGood      C        1
#> 4:    Gooood     4        Good      D        2
```

### Multiple Distance Methods

You can choose from various distance metrics provided by the
`stringdist` package:

``` r
# Optimal String Alignment (default)
fuzzystring_inner_join(x, y, by = c(name = "approx_name"), method = "osa")

# Damerau-Levenshtein
fuzzystring_inner_join(x, y, by = c(name = "approx_name"), method = "dl")

# Jaro-Winkler (good for names)
fuzzystring_inner_join(x, y, by = c(name = "approx_name"), method = "jw")

# Soundex (phonetic matching)
fuzzystring_inner_join(x, y, by = c(name = "approx_name"), method = "soundex")
```

### Case-Insensitive Matching

Use `ignore_case = TRUE` to ignore capitalization:

``` r
fuzzystring_inner_join(
  x, y, 
  by = c(name = "approx_name"),
  ignore_case = TRUE,
  max_dist = 1
)
```

## Advanced Usage

### Multiple Column Joins

You can match on multiple columns using different matching functions for
each:

``` r
fuzzystring_inner_join(
  x, y,
  by = c(name = "approx_name", value = "approx_value"),
  match_fun = list(
    name = function(x, y) stringdist::stringdist(x, y) <= 1,
    value = function(x, y) abs(x - y) < 0.5
  )
)
```

## Performance

**fuzzystring** uses a C++ implementation for row binding combined with
a `data.table` backend for fast performance on large datasets. It is
optimized for memory efficiency and type safety.
