# fuzzystring: Fast Fuzzy String Joins for Data Frames

Perform fuzzy joins on data frames using approximate string matching.
Implements all standard join types (inner, left, right, full, semi,
anti) with support for multiple string distance metrics from the
'stringdist' package including Levenshtein, Damerau-Levenshtein,
Jaro-Winkler, and Soundex. Features a high-performance 'data.table'
backend with C++ row binding for efficient processing of large datasets.
Ideal for matching misspellings, inconsistent labels, messy user input,
or reconciling datasets with slight variations in identifiers.
Optionally returns distance metrics alongside matched records.

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
