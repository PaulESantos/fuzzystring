# Fuzzy right join

Convenience wrapper for `fuzzystring_join_backend(mode = "right")`.

## Usage

``` r
fstring_right_join(x, y, by = NULL, match_fun, ...)
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

- ...:

  Additional arguments passed to the matching function(s).

## Value

See
[`fuzzystring_join_backend`](https://paulesantos.github.io/fuzzystring/reference/fuzzystring_join_backend.md).
