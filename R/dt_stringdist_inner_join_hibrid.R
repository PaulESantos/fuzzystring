#' Fuzzy stringdist inner join (hybrid optimized strategy)
#'
#' Memory-optimized strategy using C++ binding with minimal overhead.
#' Computes matches efficiently and uses C++ for the final binding step.
#'
#' @param x A \code{data.frame} or \code{data.table}.
#' @param y A \code{data.frame} or \code{data.table}.
#' @param by Columns by which to join the two tables. See \code{\link{dt_fuzzy_join}}.
#' @param max_dist Maximum distance to use for joining.
#' @param ignore_case Logical; if \code{TRUE}, comparisons are case-insensitive.
#' @param method Method for computing string distance, see
#'   \code{?stringdist::stringdist} and the \code{stringdist} package vignettes.
#' @param distance_col If not \code{NULL}, adds a column with this name containing
#'   the computed distance for each matched pair.
#' @param ... Additional arguments passed to \code{\link[stringdist]{stringdist}}.
#'
#' @return A joined table (same container type as \code{x}). See \code{\link{dt_fuzzy_join}}.
#'
#' @export
dt_stringdist_inner_join_hibrid <- function(x, y, by = NULL, max_dist = 2,
                                         method = c("osa", "lv", "dl", "hamming", "lcs", "qgram",
                                                    "cosine", "jaccard", "jw", "soundex"),
                                         ignore_case = FALSE,
                                         distance_col = NULL, ...) {
  method <- match.arg(method)

  if (method == "soundex") {
    max_dist <- 0.5
  }

  match_fun <- function(v1, v2) {
    v1 <- as.character(v1)
    v2 <- as.character(v2)

    if (isTRUE(ignore_case)) {
      v1 <- tolower(v1)
      v2 <- tolower(v2)
    }

    na_pair <- is.na(v1) | is.na(v2)

    if (method %in% c("osa", "lv", "dl")) {
      len1 <- ifelse(is.na(v1), NA_integer_, nchar(v1, type = "chars"))
      len2 <- ifelse(is.na(v2), NA_integer_, nchar(v2, type = "chars"))

      length_diff <- abs(len1 - len2)
      include <- (!na_pair) & (length_diff <= max_dist)

      dists <- rep(NA_real_, length(v1))
      if (any(include)) {
        dists[include] <- stringdist::stringdist(
          v1[include], v2[include],
          method = method, ...
        )
      }
    } else {
      dists <- rep(NA_real_, length(v1))
      ok <- !na_pair
      if (any(ok)) {
        dists[ok] <- stringdist::stringdist(
          v1[ok], v2[ok],
          method = method, ...
        )
      }
    }

    inc <- (!is.na(dists)) & (dists <= max_dist)

    ret <- data.table::data.table(include = inc)
    if (!is.null(distance_col)) {
      data.table::set(ret, j = distance_col, value = dists)
    }
    ret
  }

  # Use dt_fuzzy_join_cpp which already has optimized C++ binding
  dt_fuzzy_join_cpp(x, y, by = by, mode = "inner", match_fun = match_fun)
}
