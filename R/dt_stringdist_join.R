#' Join two tables based on fuzzy string matching of their columns (data.table)
#'
#' Join two tables based on fuzzy string matching of their columns, using
#' \code{stringdist::stringdist()} to compute distances and a data.table-based
#' \code{dt_fuzzy_join()} backend.
#'
#' @param x A \code{data.frame} or \code{data.table}.
#' @param y A \code{data.frame} or \code{data.table}.
#' @param by Columns by which to join the two tables. See \code{\link{dt_fuzzy_join}}.
#' @param max_dist Maximum distance to use for joining.
#' @param ignore_case Logical; if \code{TRUE}, comparisons are case-insensitive.
#' @param method Method for computing string distance, see
#'   \code{?stringdist::stringdist} and the \code{stringdist} package vignettes.
#' @param distance_col If not \code{NULL}, adds a column with this name containing
#'   the computed distance for each matched pair (or \code{NA} for unmatched rows
#'   in outer joins).
#' @param use_cpp Use C++ code implementation for bind_by_rowid.
#' @param mode One of \code{"inner"}, \code{"left"}, \code{"right"}, \code{"full"},
#'   \code{"semi"}, or \code{"anti"}.
#' @param ... Additional arguments passed to \code{\link[stringdist]{stringdist}}.
#'
#' @details
#' If \code{method = "soundex"}, \code{max_dist} is automatically set to 0.5,
#' since Soundex distance is 0 (match) or 1 (no match).
#'
#' For Levenshtein-like methods (\code{"osa"}, \code{"lv"}, \code{"dl"}), a fast
#' prefilter is applied: if \code{abs(nchar(v1) - nchar(v2)) > max_dist}, the pair
#' cannot match, so distance is not computed for that pair.
#'
#' @return A joined table (same container type as \code{x}). See \code{\link{dt_fuzzy_join}}.
#'
#' @export
dt_stringdist_join <- function(x, y, by = NULL, max_dist = 2,
                            method = c("osa", "lv", "dl", "hamming", "lcs", "qgram",
                                       "cosine", "jaccard", "jw", "soundex"),
                            mode = "inner",
                            ignore_case = FALSE,
                            distance_col = NULL,
                            use_cpp = TRUE,...) {
  method <- match.arg(method)

  if (method == "soundex") {
    max_dist <- 0.5
  }

  # match_fun debe ser vectorizada y eficiente.
  match_fun <- function(v1, v2) {
    # Coerce to character early (handles factors, ordered, etc.)
    v1 <- as.character(v1)
    v2 <- as.character(v2)

    if (isTRUE(ignore_case)) {
      v1 <- tolower(v1)
      v2 <- tolower(v2)
    }

    # NA handling: treat NA pairs as non-matching
    na_pair <- is.na(v1) | is.na(v2)

    # Prefilter for Levenshtein-like distances
    if (method %in% c("osa", "lv", "dl")) {
      # base::nchar doesn't accept allowNA/keepNA in some R versions;
      # use safe nchar + NA propagation
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

  # Use C++ or R version
  if (use_cpp) {
    out <- dt_fuzzy_join_cpp(x, y, by = by, mode = mode, match_fun = match_fun)
  } else {
    out <- dt_fuzzy_join(x, y, by = by, mode = mode, match_fun = match_fun)
  }

  # asegura distancia en outer joins aunque no existan matches
  dt_ensure_distance_col(out, distance_col, mode)
}


#' @rdname dt_stringdist_join
#' @export
dt_stringdist_inner_join <- function(x, y, by = NULL, distance_col = NULL, ...) {
  dt_stringdist_join(x, y, by = by, mode = "inner", distance_col = distance_col, ...)
}

#' @rdname dt_stringdist_join
#' @export
dt_stringdist_left_join <- function(x, y, by = NULL, distance_col = NULL, ...) {
  dt_stringdist_join(x, y, by = by, mode = "left", distance_col = distance_col, ...)
}

#' @rdname dt_stringdist_join
#' @export
dt_stringdist_right_join <- function(x, y, by = NULL, distance_col = NULL, ...) {
  dt_stringdist_join(x, y, by = by, mode = "right", distance_col = distance_col, ...)
}

#' @rdname dt_stringdist_join
#' @export
dt_stringdist_full_join <- function(x, y, by = NULL, distance_col = NULL, ...) {
  dt_stringdist_join(x, y, by = by, mode = "full", distance_col = distance_col, ...)
}

#' @rdname dt_stringdist_join
#' @export
dt_stringdist_semi_join <- function(x, y, by = NULL, distance_col = NULL, ...) {
  dt_stringdist_join(x, y, by = by, mode = "semi", distance_col = distance_col, ...)
}

#' @rdname dt_stringdist_join
#' @export
dt_stringdist_anti_join <- function(x, y, by = NULL, distance_col = NULL, ...) {
  dt_stringdist_join(x, y, by = by, mode = "anti", distance_col = distance_col, ...)
}

