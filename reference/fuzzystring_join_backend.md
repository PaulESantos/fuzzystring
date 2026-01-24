# Fuzzy join backend using data.table + C++ row binding

Low-level engine used by
[`fuzzystring_join`](https://paulesantos.github.io/fuzzystring/reference/fuzzystring_join.md)
and the C++-optimized fuzzy join helpers. It builds the match index with
R/data.table and then assembles the result using a compiled C++ binder
for speed.

## Usage

``` r
fuzzystring_join_backend(
  x,
  y,
  by = NULL,
  match_fun = NULL,
  multi_by = NULL,
  multi_match_fun = NULL,
  index_match_fun = NULL,
  mode = "inner",
  ...
)
```

## Arguments

- x:

  A `data.frame` or `data.table`.

- y:

  A `data.frame` or `data.table`.

- by:

  Columns by which to join the two tables. See
  [`fuzzystring_join`](https://paulesantos.github.io/fuzzystring/reference/fuzzystring_join.md).

- match_fun:

  A function used to match values. It must return a logical vector (or a
  data.frame/data.table whose first column is logical) indicating which
  pairs match. For multi-column joins, you may pass a list of functions
  (one per column).

- multi_by:

  A character vector of column names used for multi-column matching when
  `multi_match_fun` is supplied.

- multi_match_fun:

  A function that receives matrices of unique values for `x` and `y`
  (rows correspond to unique combinations of `multi_by`). It must return
  a logical vector (or a data.frame/data.table whose first column is
  logical) indicating which rows match.

- index_match_fun:

  A function that receives the joined columns from `x` and `y` and
  returns a table with integer columns `x` and `y` (1-based row
  indices).

- mode:

  One of `"inner"`, `"left"`, `"right"`, `"full"`, `"semi"`, or
  `"anti"`.

- ...:

  Additional arguments passed to the matching function(s).

## Value

A joined table (same container type as `x`). See
[`fuzzystring_join`](https://paulesantos.github.io/fuzzystring/reference/fuzzystring_join.md).

## Details

This function works like
[`fuzzystring_join`](https://paulesantos.github.io/fuzzystring/reference/fuzzystring_join.md),
but replaces the R-based row binding with a C++ implementation. This
provides better performance, especially for large joins with many
matches. It is intended as a backend and does not compute distances
itself; use
[`fuzzystring_join`](https://paulesantos.github.io/fuzzystring/reference/fuzzystring_join.md)
for string-distance based matching.

The C++ implementation handles:

- Efficient subsetting by row indices

- Proper handling of NA values in outer joins

- Type-safe column operations for all common R types

- Preservation of factor levels and attributes

- Column name conflicts with .x/.y suffixes
