#' Join two tables based on fuzzy string matching
#'
#' Uses \code{stringdist::stringdist()} to compute distances and a data.table-based
#' backend to assemble the final result. This is the main user-facing entry point
#' for fuzzy joins on strings.
#'
#' @param x A \code{data.frame} or \code{data.table}.
#' @param y A \code{data.frame} or \code{data.table}.
#' @param by Columns by which to join the two tables. You can supply a character
#'   vector of common names (e.g. \code{c("name")} ), or a named vector mapping
#'   \code{x} to \code{y} (e.g. \code{c(name = "approx_name")}).
#' @param max_dist Maximum distance to use for joining. Smaller values are stricter.
#' @param ignore_case Logical; if \code{TRUE}, comparisons are case-insensitive.
#' @param method Method for computing string distance, see
#'   \code{?stringdist::stringdist} and the \code{stringdist} package vignettes.
#' @param distance_col If not \code{NULL}, adds a column with this name containing
#'   the computed distance for each matched pair (or \code{NA} for unmatched rows
#'   in outer joins).
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
#' @return A joined table (same container type as \code{x}). See
#'   \code{\link{fuzzystring_join_backend}} for details on output structure.
#'
#' @examples
#' \donttest{
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   d <- data.table::data.table(approximate_name = c("Idea", "Premiom"))
#'   # Match diamonds$cut to d$approximate_name
#'   res <- fuzzystring_inner_join(ggplot2::diamonds, d,
#'     by = c(cut = "approximate_name"),
#'     max_dist = 1
#'   )
#'   head(res)
#' }
#' }
#'
#' @export
fuzzystring_join <- function(x, y, by = NULL, max_dist = 2,
                            method = c("osa", "lv", "dl", "hamming", "lcs", "qgram",
                                       "cosine", "jaccard", "jw", "soundex"),
                            mode = "inner",
                            ignore_case = FALSE,
                            distance_col = NULL,
                            ...) {
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

  # Use C++ backend
  out <- fuzzystring_join_backend(x, y, by = by, mode = mode, match_fun = match_fun)

  # asegura distancia en outer joins aunque no existan matches
  fst_ensure_distance_col(out, distance_col, mode)
}


#' @rdname fuzzystring_join
#' @export
fuzzystring_inner_join <- function(x, y, by = NULL, distance_col = NULL, ...) {
  fuzzystring_join(x, y, by = by, mode = "inner", distance_col = distance_col, ...)
}

#' @rdname fuzzystring_join
#' @export
fuzzystring_left_join <- function(x, y, by = NULL, distance_col = NULL, ...) {
  fuzzystring_join(x, y, by = by, mode = "left", distance_col = distance_col, ...)
}

#' @rdname fuzzystring_join
#' @export
fuzzystring_right_join <- function(x, y, by = NULL, distance_col = NULL, ...) {
  fuzzystring_join(x, y, by = by, mode = "right", distance_col = distance_col, ...)
}

#' @rdname fuzzystring_join
#' @export
fuzzystring_full_join <- function(x, y, by = NULL, distance_col = NULL, ...) {
  fuzzystring_join(x, y, by = by, mode = "full", distance_col = distance_col, ...)
}

#' @rdname fuzzystring_join
#' @export
fuzzystring_semi_join <- function(x, y, by = NULL, distance_col = NULL, ...) {
  fuzzystring_join(x, y, by = by, mode = "semi", distance_col = distance_col, ...)
}

#' @rdname fuzzystring_join
#' @export
fuzzystring_anti_join <- function(x, y, by = NULL, distance_col = NULL, ...) {
  fuzzystring_join(x, y, by = by, mode = "anti", distance_col = distance_col, ...)
}
