#' fuzzystring: Fast fuzzy string joins for data frames
#'
#' `fuzzystring` provides fuzzy inner, left, right, full, semi, and anti joins
#' for `data.frame` and `data.table` objects using approximate string matching.
#' It combines `stringdist` metrics with a `data.table` backend and compiled C++
#' result assembly to reduce overhead in large joins while preserving familiar
#' join semantics.
#'
#' Main entry points are [fuzzystring_join()] and the convenience wrappers
#' [fuzzystring_inner_join()], [fuzzystring_left_join()],
#' [fuzzystring_right_join()], [fuzzystring_full_join()],
#' [fuzzystring_semi_join()], and [fuzzystring_anti_join()].
#'
#' The package also includes the example dataset [misspellings].
#'
#' @keywords package
#' @name fuzzystring
#' @docType package
#' @useDynLib fuzzystring, .registration = TRUE
#' @importFrom Rcpp evalCpp
"_PACKAGE"
