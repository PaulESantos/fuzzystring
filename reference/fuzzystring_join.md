# Join two tables based on fuzzy string matching

Uses
[`stringdist::stringdist()`](https://rdrr.io/pkg/stringdist/man/stringdist.html)
to compute distances and a `data.table`-orchestrated backend with
compiled 'C++' assembly to produce the final result. This is the main
user-facing entry point for fuzzy joins on strings.

## Usage

``` r
fuzzystring_join(
  x,
  y,
  by = NULL,
  max_dist = 2,
  method = c("osa", "lv", "dl", "hamming", "lcs", "qgram", "cosine", "jaccard", "jw",
    "soundex"),
  mode = "inner",
  ignore_case = FALSE,
  distance_col = NULL,
  ...
)

fuzzystring_inner_join(x, y, by = NULL, distance_col = NULL, ...)

fuzzystring_left_join(x, y, by = NULL, distance_col = NULL, ...)

fuzzystring_right_join(x, y, by = NULL, distance_col = NULL, ...)

fuzzystring_full_join(x, y, by = NULL, distance_col = NULL, ...)

fuzzystring_semi_join(x, y, by = NULL, distance_col = NULL, ...)

fuzzystring_anti_join(x, y, by = NULL, distance_col = NULL, ...)
```

## Arguments

- x:

  A `data.frame` or `data.table`.

- y:

  A `data.frame` or `data.table`.

- by:

  Columns by which to join the two tables. You can supply a character
  vector of common names (e.g. `c("name")` ), or a named vector mapping
  `x` to `y` (e.g. `c(name = "approx_name")`).

- max_dist:

  Maximum distance to use for joining. Smaller values are stricter.

- method:

  Method for computing string distance, see
  [`?stringdist::stringdist`](https://rdrr.io/pkg/stringdist/man/stringdist.html)
  and the `stringdist` package vignettes.

- mode:

  One of `"inner"`, `"left"`, `"right"`, `"full"`, `"semi"`, or
  `"anti"`.

- ignore_case:

  Logical; if `TRUE`, comparisons are case-insensitive.

- distance_col:

  If not `NULL`, adds a column with this name containing the computed
  distance for each matched pair (or `NA` for unmatched rows in outer
  joins).

- ...:

  Additional arguments passed to
  [`stringdist`](https://rdrr.io/pkg/stringdist/man/stringdist.html).

## Value

A joined table (same container type as `x`). See
[`fuzzystring_join_backend`](https://paulesantos.github.io/fuzzystring/reference/fuzzystring_join_backend.md)
for details on output structure.

## Details

If `method = "soundex"`, `max_dist` is automatically set to 0.5, since
Soundex distance is 0 (match) or 1 (no match).

For single-column joins, fuzzystring uses adaptive candidate planning
before calling
[`stringdist::stringdist()`](https://rdrr.io/pkg/stringdist/man/stringdist.html).
For Levenshtein-like methods (`"osa"`, `"lv"`, `"dl"`), a fast prefilter
is applied: if `abs(nchar(v1) - nchar(v2)) > max_dist`, the pair cannot
match, so distance is not computed for that pair. For low-duplication
workloads, the planner can also evaluate larger dense blocks of unique
values to reduce orchestration overhead while preserving the same
matching semantics.

## Examples

``` r
# \donttest{
if (requireNamespace("ggplot2", quietly = TRUE)) {
  d <- data.table::data.table(approximate_name = c("Idea", "Premiom"))
  # Match diamonds$cut to d$approximate_name
  res <- fuzzystring_inner_join(ggplot2::diamonds, d,
    by = c(cut = "approximate_name"),
    max_dist = 1
  )
  head(res)
}
#>   carat     cut color clarity depth table price    x    y    z approximate_name
#> 1  0.23   Ideal     E     SI2  61.5    55   326 3.95 3.98 2.43             Idea
#> 2  0.21 Premium     E     SI1  59.8    61   326 3.89 3.84 2.31          Premiom
#> 3  0.29 Premium     I     VS2  62.4    58   334 4.20 4.23 2.63          Premiom
#> 4  0.23   Ideal     J     VS1  62.8    56   340 3.93 3.90 2.46             Idea
#> 5  0.22 Premium     F     SI1  60.4    61   342 3.88 3.84 2.33          Premiom
#> 6  0.31   Ideal     J     SI2  62.2    54   344 4.35 4.37 2.71             Idea
# }
```
