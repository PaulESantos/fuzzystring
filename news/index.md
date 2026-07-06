# Changelog

## fuzzystring 0.0.6

## fuzzystring 0.0.5

CRAN release: 2026-03-28

- [`fuzzystring_join()`](https://paulesantos.github.io/fuzzystring/reference/fuzzystring_join.md)
  now validates join arguments before planning, prevents distance-column
  name collisions, reports the maximum component distance for
  multi-column joins, rejects results that exceed supported index sizes,
  and uses a keyed exact-match route for safe `max_dist = 0` calls
  (stability and performance audit).
- Guard test bootstrap so CRAN pretests do not fail when `testthat` is
  unavailable from `Suggests`.
- Capitalized `Jaccard` in `DESCRIPTION` to avoid the incoming
  feasibility note.

## fuzzystring 0.0.4

## fuzzystring 0.0.3

- Refined package metadata and pkgdown/homepage descriptions for the
  current `data.table` plus compiled C++ backend.
- Reworked the README to match the public API and added direct links to
  the getting-started and benchmark articles.
- Added a benchmark vignette comparing `fuzzystring` against `fuzzyjoin`
  from a precomputed CSV snapshot shipped in `inst/extdata/`.
- Updated vignette text to reflect the current implementation and
  installation paths.
