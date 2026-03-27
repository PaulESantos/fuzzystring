# fuzzystring: Fast fuzzy string joins for data frames

`fuzzystring` provides fuzzy inner, left, right, full, semi, and anti
joins for `data.frame` and `data.table` objects using approximate string
matching. It combines `stringdist` metrics with a `data.table` backend
and compiled C++ result assembly to reduce overhead in large joins while
preserving familiar join semantics.

## Details

Main entry points are
[`fuzzystring_join()`](https://paulesantos.github.io/fuzzystring/reference/fuzzystring_join.md)
and the convenience wrappers
[`fuzzystring_inner_join()`](https://paulesantos.github.io/fuzzystring/reference/fuzzystring_join.md),
[`fuzzystring_left_join()`](https://paulesantos.github.io/fuzzystring/reference/fuzzystring_join.md),
[`fuzzystring_right_join()`](https://paulesantos.github.io/fuzzystring/reference/fuzzystring_join.md),
[`fuzzystring_full_join()`](https://paulesantos.github.io/fuzzystring/reference/fuzzystring_join.md),
[`fuzzystring_semi_join()`](https://paulesantos.github.io/fuzzystring/reference/fuzzystring_join.md),
and
[`fuzzystring_anti_join()`](https://paulesantos.github.io/fuzzystring/reference/fuzzystring_join.md).

The package also includes the example dataset
[misspellings](https://paulesantos.github.io/fuzzystring/reference/misspellings.md).

## See also

Useful links:

- <https://github.com/PaulESantos/fuzzystring>

- <https://paulesantos.github.io/fuzzystring/>

- Report bugs at <https://github.com/PaulESantos/fuzzystring/issues>

## Author

**Maintainer**: Paul E. Santos Andrade <paulefrens@gmail.com>
([ORCID](https://orcid.org/0000-0002-6635-0375))

Other contributors:

- David Robinson <admiral.david@gmail.com> (aut of fuzzyjoin)
  \[contributor\]
