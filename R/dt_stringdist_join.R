fst_expand_match_indices <- function(x_lists, y_lists) {
  x_len <- lengths(x_lists)
  y_len <- lengths(y_lists)
  rep_counts <- x_len * y_len
  total_size <- sum(rep_counts)

  if (total_size == 0L) {
    return(list(x = integer(0), y = integer(0), rep_counts = integer(0)))
  }

  x_rep <- integer(total_size)
  y_rep <- integer(total_size)

  pos <- 1L
  for (i in seq_along(x_lists)) {
    lx <- x_len[i]
    ly <- y_len[i]
    if (lx == 0L || ly == 0L) {
      next
    }

    n <- lx * ly
    end_pos <- pos + n - 1L
    x_rep[pos:end_pos] <- rep.int(x_lists[[i]], times = ly)
    y_rep[pos:end_pos] <- rep(y_lists[[i]], each = lx)
    pos <- end_pos + 1L
  }

  list(x = x_rep, y = y_rep, rep_counts = rep_counts)
}

fst_chunk_size_y <- function(n_x, target_pairs = 250000L) {
  n_x <- max(1L, as.integer(n_x))
  max(1L, as.integer(target_pairs %/% n_x))
}

fst_is_length_pruned_method <- function(method) {
  method %in% c("osa", "lv", "dl", "hamming")
}

fst_choose_target_pairs <- function(method, n_rows_x, n_rows_y,
                                    n_unique_x, n_unique_y,
                                    base_target_pairs = 250000L) {
  if (fst_is_length_pruned_method(method)) {
    return(as.integer(base_target_pairs))
  }

  avg_dup_x <- n_rows_x / max(1L, n_unique_x)
  avg_dup_y <- n_rows_y / max(1L, n_unique_y)
  max_avg_dup <- max(avg_dup_x, avg_dup_y)

  if (max_avg_dup <= 1.10) {
    return(3000000L)
  }
  if (max_avg_dup <= 1.50) {
    return(2000000L)
  }
  if (max_avg_dup <= 2.00) {
    return(1000000L)
  }

  as.integer(base_target_pairs)
}

fst_use_dense_unique_block <- function(method, n_rows_x, n_rows_y,
                                       n_unique_x, n_unique_y,
                                       max_pairs = 12000000L) {
  if (fst_is_length_pruned_method(method)) {
    return(FALSE)
  }

  if (n_unique_x == 0L || n_unique_y == 0L) {
    return(FALSE)
  }

  avg_dup_x <- n_rows_x / n_unique_x
  avg_dup_y <- n_rows_y / n_unique_y
  pair_count <- as.double(n_unique_x) * as.double(n_unique_y)

  pair_count <= max_pairs && avg_dup_x <= 2 && avg_dup_y <= 2
}

fst_stringdist_eval_block <- function(x_vals, y_vals, x_rows, y_rows,
                                      method, max_dist, distance_col,
                                      extra_args = list()) {
  n_x <- length(x_vals)
  n_y <- length(y_vals)
  if (n_x == 0L || n_y == 0L) {
    return(data.table::data.table(x = integer(0), y = integer(0)))
  }

  x_rep <- rep.int(x_vals, times = n_y)
  y_rep <- rep(y_vals, each = n_x)
  dists <- do.call(
    stringdist::stringdist,
    c(list(a = x_rep, b = y_rep, method = method), extra_args)
  )

  keep <- (!is.na(dists)) & (dists <= max_dist)
  if (!any(keep)) {
    return(data.table::data.table(x = integer(0), y = integer(0)))
  }

  w <- which(keep) - 1L
  gx <- (w %% n_x) + 1L
  gy <- (w %/% n_x) + 1L

  x_lists <- x_rows[gx]
  y_lists <- y_rows[gy]
  expanded <- fst_expand_match_indices(x_lists, y_lists)

  out <- data.table::data.table(x = expanded$x, y = expanded$y)
  if (!is.null(distance_col)) {
    out[[distance_col]] <- rep.int(dists[keep], times = expanded$rep_counts)
  }

  out
}

fst_stringdist_single_col_matches <- function(v1, v2, max_dist, method,
                                              ignore_case = FALSE,
                                              distance_col = NULL,
                                              extra_args = list(),
                                              target_pairs = 250000L) {
  v1 <- as.character(v1)
  v2 <- as.character(v2)

  if (isTRUE(ignore_case)) {
    v1 <- tolower(v1)
    v2 <- tolower(v2)
  }

  x_groups <- data.table::data.table(value = v1, row = seq_along(v1))
  y_groups <- data.table::data.table(value = v2, row = seq_along(v2))

  x_groups <- x_groups[!is.na(value), .(rows = list(row)), by = value]
  y_groups <- y_groups[!is.na(value), .(rows = list(row)), by = value]

  if (nrow(x_groups) == 0L || nrow(y_groups) == 0L) {
    return(data.table::data.table(x = integer(0), y = integer(0)))
  }

  x_groups[, grp := .I]
  y_groups[, grp := .I]

  use_length_pruning <- fst_is_length_pruned_method(method)
  target_pairs <- fst_choose_target_pairs(
    method = method,
    n_rows_x = length(v1),
    n_rows_y = length(v2),
    n_unique_x = nrow(x_groups),
    n_unique_y = nrow(y_groups),
    base_target_pairs = target_pairs
  )

  if (use_length_pruning) {
    x_groups[, nchars := nchar(value, type = "chars")]
    y_groups[, nchars := nchar(value, type = "chars")]
  }

  if (fst_use_dense_unique_block(
    method = method,
    n_rows_x = length(v1),
    n_rows_y = length(v2),
    n_unique_x = nrow(x_groups),
    n_unique_y = nrow(y_groups)
  )) {
    return(fst_stringdist_eval_block(
      x_vals = x_groups$value,
      y_vals = y_groups$value,
      x_rows = x_groups$rows,
      y_rows = y_groups$rows,
      method = method,
      max_dist = max_dist,
      distance_col = distance_col,
      extra_args = extra_args
    ))
  }

  parts <- vector("list", 0L)

  if (use_length_pruning) {
    x_by_len <- split(x_groups$grp, x_groups$nchars)
    y_by_len <- split(y_groups$grp, y_groups$nchars)

    for (len_name in names(x_by_len)) {
      x_ids <- x_by_len[[len_name]]
      if (!length(x_ids)) {
        next
      }

      len_val <- as.integer(len_name)
      if (method == "hamming") {
        y_len_candidates <- as.character(len_val)
      } else {
        lower_len <- max(0L, as.integer(ceiling(len_val - max_dist)))
        upper_len <- as.integer(floor(len_val + max_dist))
        y_len_candidates <- as.character(seq.int(lower_len, upper_len))
      }
      y_len_candidates <- intersect(y_len_candidates, names(y_by_len))
      if (!length(y_len_candidates)) {
        next
      }

      for (y_len_name in y_len_candidates) {
        y_ids_all <- y_by_len[[y_len_name]]
        if (!length(y_ids_all)) {
          next
        }

        y_block_size <- fst_chunk_size_y(length(x_ids), target_pairs = target_pairs)
        y_splits <- split(y_ids_all, ceiling(seq_along(y_ids_all) / y_block_size))

        for (y_ids in y_splits) {
          parts[[length(parts) + 1L]] <- fst_stringdist_eval_block(
            x_vals = x_groups$value[x_ids],
            y_vals = y_groups$value[y_ids],
            x_rows = x_groups$rows[x_ids],
            y_rows = y_groups$rows[y_ids],
            method = method,
            max_dist = max_dist,
            distance_col = distance_col,
            extra_args = extra_args
          )
        }
      }
    }
  } else {
    x_ids <- x_groups$grp
    y_ids_all <- y_groups$grp
    y_block_size <- fst_chunk_size_y(length(x_ids), target_pairs = target_pairs)
    y_splits <- split(y_ids_all, ceiling(seq_along(y_ids_all) / y_block_size))

    for (y_ids in y_splits) {
      parts[[length(parts) + 1L]] <- fst_stringdist_eval_block(
        x_vals = x_groups$value[x_ids],
        y_vals = y_groups$value[y_ids],
        x_rows = x_groups$rows[x_ids],
        y_rows = y_groups$rows[y_ids],
        method = method,
        max_dist = max_dist,
        distance_col = distance_col,
        extra_args = extra_args
      )
    }
  }

  if (!length(parts)) {
    return(data.table::data.table(x = integer(0), y = integer(0)))
  }

  data.table::rbindlist(parts, use.names = TRUE, fill = TRUE)
}

#' Join two tables based on fuzzy string matching
#'
#' Uses \code{stringdist::stringdist()} to compute distances and a
#' \code{data.table}-orchestrated backend with compiled 'C++' assembly to produce
#' the final result. This is the main user-facing entry point for fuzzy joins on
#' strings.
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
#' For single-column joins, fuzzystring uses adaptive candidate planning before
#' calling \code{stringdist::stringdist()}. For Levenshtein-like methods
#' (\code{"osa"}, \code{"lv"}, \code{"dl"}), a fast prefilter is applied: if
#' \code{abs(nchar(v1) - nchar(v2)) > max_dist}, the pair cannot match, so
#' distance is not computed for that pair. For low-duplication workloads, the
#' planner can also evaluate larger dense blocks of unique values to reduce
#' orchestration overhead while preserving the same matching semantics.
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

  if (!is.numeric(max_dist) || length(max_dist) != 1) {
    stop("max_dist must be a single numeric value", call. = FALSE)
  }
  if (max_dist < 0) {
    stop("max_dist must be non-negative", call. = FALSE)
  }

  by2 <- fst_common_by(by, x, y)
  dot_args <- list(...)

  if (length(by2$x) == 1L) {
    index_match_fun <- function(d1, d2) {
      fst_stringdist_single_col_matches(
        d1[[1]],
        d2[[1]],
        max_dist = max_dist,
        method = method,
        ignore_case = ignore_case,
        distance_col = distance_col,
        extra_args = dot_args
      )
    }

    out <- fuzzystring_join_backend(
      x,
      y,
      multi_by = by2,
      mode = mode,
      index_match_fun = index_match_fun
    )
  } else {
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
          dists[include] <- do.call(
            stringdist::stringdist,
            c(list(a = v1[include], b = v2[include], method = method), dot_args)
          )
        }
      } else {
        dists <- rep(NA_real_, length(v1))
        ok <- !na_pair
        if (any(ok)) {
          dists[ok] <- do.call(
            stringdist::stringdist,
            c(list(a = v1[ok], b = v2[ok], method = method), dot_args)
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

    out <- fuzzystring_join_backend(x, y, by = by2, mode = mode, match_fun = match_fun)
  }

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

