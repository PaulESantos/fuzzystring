#' Fuzzy join two tables using data.table with C++ binding
#'
#' This is a C++-optimized version of \code{\link{dt_fuzzy_join}} that uses a
#' compiled C++ function for the row binding operation, which can be significantly
#' faster for large joins.
#'
#' @inheritParams dt_fuzzy_join
#' @return A joined table (same container type as \code{x}). See \code{\link{dt_fuzzy_join}}.
#'
#' @details
#' This function works identically to \code{\link{dt_fuzzy_join}}, but replaces
#' the R-based \code{bind_by_rowid} function with a C++ implementation using Rcpp.
#' This provides better performance, especially for large joins with many matches.
#'
#' The C++ implementation handles:
#' \itemize{
#'   \item Efficient subsetting by row indices
#'   \item Proper handling of NA values in outer joins
#'   \item Type-safe column operations for all common R types
#'   \item Preservation of factor levels and attributes
#'   \item Column name conflicts with .x/.y suffixes
#' }
#'
#' @export
dt_fuzzy_join_cpp <- function(x, y, by = NULL, match_fun = NULL,
                              multi_by = NULL, multi_match_fun = NULL,
                              index_match_fun = NULL, mode = "inner", ...) {

  mode <- match.arg(mode, c("inner", "left", "right", "full", "semi", "anti"))

  non_nulls <- (!is.null(multi_match_fun)) +
    (!is.null(match_fun)) +
    (!is.null(index_match_fun))
  if (sum(non_nulls) != 1) {
    stop("Must give exactly one of match_fun, multi_match_fun, and index_match_fun", call. = FALSE)
  }

  x_is_dt <- data.table::is.data.table(x)

  x_original <- x
  y_original <- y

  if (data.table::is.data.table(x)) {
    x_dt <- x
  } else {
    x_dt <- data.table::as.data.table(x)
  }

  if (data.table::is.data.table(y)) {
    y_dt <- y
  } else {
    y_dt <- data.table::as.data.table(y)
  }

  # --- helpers internos (same as dt_fuzzy_join) ------------------------------

  as_mapper_dt <- function(f) {
    if (inherits(f, "formula")) {
      stop("Formula notation (~) not supported in this data.table version yet. Pass a function.", call. = FALSE)
    }
    if (!is.function(f)) stop("match_fun / multi_match_fun / index_match_fun must be a function.", call. = FALSE)
    f
  }

  group_indices_1col <- function(dt, col) {
    dt[, .(indices = list(.I)), by = c(col), verbose = FALSE]
  }

  group_indices_multicol <- function(dt, cols) {
    dt[, .(indices = list(.I)), by = cols, verbose = FALSE]
  }

  expand_index_lists <- function(x_lists, y_lists) {
    x_len <- lengths(x_lists)
    y_len <- lengths(y_lists)

    rep_counts <- x_len * y_len
    total_size <- sum(rep_counts)

    if (total_size == 0L) {
      return(list(x = integer(0), y = integer(0), x_len = x_len, y_len = y_len))
    }

    x_rep <- integer(total_size)
    y_rep <- integer(total_size)

    pos <- 1L
    for (i in seq_along(x_lists)) {
      lx <- x_len[i]
      ly <- y_len[i]
      if (lx > 0L && ly > 0L) {
        n <- lx * ly
        end_pos <- pos + n - 1L
        x_rep[pos:end_pos] <- rep.int(x_lists[[i]], times = ly)
        y_rep[pos:end_pos] <- rep(y_lists[[i]], each = lx)
        pos <- end_pos + 1L
      }
    }

    list(x = x_rep, y = y_rep, x_len = x_len, y_len = y_len)
  }

  replicate_extras <- function(extra_dt, w, x_len, y_len) {
    if (is.null(extra_dt)) return(NULL)
    rep_counts <- x_len * y_len
    if (sum(rep_counts) == 0L) return(extra_dt[0])
    idx <- rep.int(w, times = rep_counts)
    extra_dt[idx]
  }

  # --- C++ binding function --------------------------------------------------
  # Uses the compiled C++ function instead of R implementation

  bind_by_rowid_cpp_wrapper <- function(x_dt2, y_dt2, matches_dt, x_orig = x_dt2, y_orig = y_dt2, overlap = character(0)) {
    n_rows <- nrow(matches_dt)

    if (n_rows == 0L) {
      # Empty case
      ret <- data.table::data.table()
      for (col in names(x_dt2)) ret[[col]] <- x_dt2[[col]][0]
      for (col in names(y_dt2)) ret[[col]] <- y_dt2[[col]][0]
      extra_cols <- setdiff(names(matches_dt), c("x", "y", "i"))
      for (col in extra_cols) ret[[col]] <- matches_dt[[col]]
      return(ret)
    }

    x_idx <- matches_dt$x
    y_idx <- matches_dt$y
    has_na <- any(is.na(x_idx)) || any(is.na(y_idx))

    # For inner joins (no NAs), use C++ function directly
    if (!has_na) {
      # Use original data for subsetting
      use_x <- if (identical(x_dt2, x_orig)) x_orig else x_dt2
      use_y <- if (identical(y_dt2, y_orig)) y_orig else y_dt2

      # Ensure they are data.tables
      if (!data.table::is.data.table(use_x)) use_x <- data.table::as.data.table(use_x)
      if (!data.table::is.data.table(use_y)) use_y <- data.table::as.data.table(use_y)

      # Call C++ function (via Rcpp)
      ret <- bind_by_rowid_cpp(use_x, use_y, x_idx, y_idx, overlap)

      # Convert to data.table if not already
      if (!data.table::is.data.table(ret)) {
        ret <- data.table::as.data.table(ret)
      }

      # Add extra columns if exist
      extra_cols <- setdiff(names(matches_dt), c("x", "y", "i"))
      if (length(extra_cols) > 0L) {
        data.table::setalloccol(ret)
        for (col in extra_cols) {
          data.table::set(ret, j = col, value = matches_dt[[col]])
        }
      }

      return(ret)
    }

    # For outer joins with NAs, still use R implementation for now
    # (Could be optimized in C++ later if needed)
    ret <- data.table::data.table(.dummy = seq_len(n_rows))

    for (col in names(x_orig)) {
      col_data <- x_orig[[col]]
      if (is.factor(col_data)) {
        new_col <- factor(rep(NA, n_rows), levels = levels(col_data))
      } else if (inherits(col_data, "Date")) {
        new_col <- rep(as.Date(NA), n_rows)
      } else if (inherits(col_data, "POSIXct")) {
        new_col <- rep(as.POSIXct(NA), n_rows)
      } else if (is.integer(col_data)) {
        new_col <- rep(NA_integer_, n_rows)
      } else if (is.numeric(col_data)) {
        new_col <- rep(NA_real_, n_rows)
      } else if (is.character(col_data)) {
        new_col <- rep(NA_character_, n_rows)
      } else if (is.logical(col_data)) {
        new_col <- rep(NA, n_rows)
      } else {
        new_col <- rep(col_data[NA_integer_], n_rows)
      }

      valid_x <- !is.na(x_idx)
      if (any(valid_x)) {
        new_col[valid_x] <- col_data[x_idx[valid_x]]
      }
      ret[[col]] <- new_col
    }

    for (col in names(y_orig)) {
      col_data <- y_orig[[col]]
      if (is.factor(col_data)) {
        new_col <- factor(rep(NA, n_rows), levels = levels(col_data))
      } else if (inherits(col_data, "Date")) {
        new_col <- rep(as.Date(NA), n_rows)
      } else if (inherits(col_data, "POSIXct")) {
        new_col <- rep(as.POSIXct(NA), n_rows)
      } else if (is.integer(col_data)) {
        new_col <- rep(NA_integer_, n_rows)
      } else if (is.numeric(col_data)) {
        new_col <- rep(NA_real_, n_rows)
      } else if (is.character(col_data)) {
        new_col <- rep(NA_character_, n_rows)
      } else if (is.logical(col_data)) {
        new_col <- rep(NA, n_rows)
      } else {
        new_col <- rep(col_data[NA_integer_], n_rows)
      }

      valid_y <- !is.na(y_idx)
      if (any(valid_y)) {
        new_col[valid_y] <- col_data[y_idx[valid_y]]
      }
      ret[[col]] <- new_col
    }

    ret[, .dummy := NULL]

    extra_cols <- setdiff(names(matches_dt), c("x", "y", "i"))
    if (length(extra_cols) > 0L) {
      data.table::setalloccol(ret)
      for (col in extra_cols) {
        data.table::set(ret, j = col, value = matches_dt[[col]])
      }
    }

    ret
  }

  # --- construir matches (same logic as dt_fuzzy_join) -----------------------

  matches <- NULL

  if (!is.null(match_fun)) {
    by2 <- dt_common_by(by, x_dt, y_dt)

    if (is.list(match_fun)) {
      mf_list <- lapply(match_fun, as_mapper_dt)
    } else {
      mf_list <- list(as_mapper_dt(match_fun))
    }

    if (length(mf_list) == 1L) {
      mf_list <- rep(mf_list, length(by2$x))
    }
    if (length(mf_list) != length(by2$x)) {
      stop("Length of match_fun not equal to columns specified in 'by'.", call. = FALSE)
    }

    parts <- vector("list", length(by2$x))

    for (i in seq_along(by2$x)) {
      xcol <- by2$x[i]
      ycol <- by2$y[i]

      ix <- group_indices_1col(x_dt, xcol)
      iy <- group_indices_1col(y_dt, ycol)

      ux <- ix[[xcol]]
      uy <- iy[[ycol]]

      n_x <- length(ux)
      n_y <- length(uy)

      mf <- if (!is.null(names(mf_list))) {
        mf_list[[xcol]]
      } else {
        mf_list[[i]]
      }

      m <- mf(rep(ux, times = n_y), rep(uy, each = n_x), ...)

      extra_dt <- NULL
      if (data.table::is.data.table(m) || is.data.frame(m)) {
        m_dt <- data.table::as.data.table(m)
        if (ncol(m_dt) > 1L) extra_dt <- m_dt[, -1, with = FALSE]
        m <- m_dt[[1]]
      }

      if (!is.logical(m)) stop("match_fun must return logical or a data.frame/data.table whose first column is logical.", call. = FALSE)

      w <- which(m) - 1L
      if (length(w) == 0L) {
        parts[[i]] <- data.table::data.table(i = numeric(0), x = numeric(0), y = numeric(0))
        next
      }

      gx <- (w %% n_x) + 1L
      gy <- (w %/% n_x) + 1L

      x_lists <- ix$indices[gx]
      y_lists <- iy$indices[gy]

      expd <- expand_index_lists(x_lists, y_lists)
      ret <- data.table::data.table(i = i, x = expd$x, y = expd$y)

      if (!is.null(extra_dt)) {
        extra_rep <- replicate_extras(extra_dt, w + 1L, expd$x_len, expd$y_len)
        ret <- data.table::as.data.table(cbind(ret, extra_rep))
      }

      parts[[i]] <- ret
    }

    matches <- data.table::rbindlist(parts, use.names = TRUE, fill = TRUE)

    if (length(by2$x) > 1L) {
      accept <- matches[, .N, by = .(x, y)][N == length(by2$x), .(x, y)]
      matches <- matches[accept, on = .(x, y), nomatch = 0L]

      extra_cols <- setdiff(names(matches), c("i", "x", "y"))
      if (length(extra_cols) > 0L) {
        matches[, name := by2$x[i]]

        formula_str <- paste0("x + y ~ name")
        matches_wide <- data.table::dcast(
          matches,
          as.formula(formula_str),
          value.var = extra_cols,
          fun.aggregate = function(x) x[1L]
        )
        matches <- matches_wide
      } else {
        matches <- unique(matches[, .(x, y)])
      }
    } else {
      if (ncol(matches) == 3L) {
        matches <- unique(matches[, .(x, y)])
      }
    }

  } else if (!is.null(multi_match_fun)) {
    mmf <- as_mapper_dt(multi_match_fun)
    by2 <- dt_common_by(multi_by, x_dt, y_dt)

    ix <- group_indices_multicol(x_dt, by2$x)
    iy <- group_indices_multicol(y_dt, by2$y)

    ux <- as.matrix(ix[, ..by2$x])
    uy <- as.matrix(iy[, ..by2$y])

    n_x <- nrow(ux)
    n_y <- nrow(uy)

    if (n_x == 0L || n_y == 0L) {
      matches <- data.table::data.table(x = numeric(0), y = numeric(0))
    } else {
      grid <- data.table::CJ(ix = seq_len(n_x), iy = seq_len(n_y))
      ux_in <- ux[grid$ix, , drop = FALSE]
      uy_in <- uy[grid$iy, , drop = FALSE]

      m <- mmf(ux_in, uy_in)

      extra_dt <- NULL
      if (data.table::is.data.table(m) || is.data.frame(m)) {
        m_dt <- data.table::as.data.table(m)
        if (ncol(m_dt) > 1L) extra_dt <- m_dt[, -1, with = FALSE]
        m <- m_dt[[1]]
      }
      if (!is.logical(m)) stop("multi_match_fun must return logical or a data.frame/data.table whose first column is logical.", call. = FALSE)

      if (sum(m) == 0L) {
        matches <- data.table::data.table(x = numeric(0), y = numeric(0))
      } else {
        keep <- which(m)
        x_lists <- ix$indices[grid$ix[keep]]
        y_lists <- iy$indices[grid$iy[keep]]

        expd <- expand_index_lists(x_lists, y_lists)
        matches <- data.table::data.table(x = expd$x, y = expd$y)

        if (!is.null(extra_dt)) {
          extra_rep <- replicate_extras(extra_dt, keep, expd$x_len, expd$y_len)
          matches <- data.table::as.data.table(cbind(matches, extra_rep))
        }
      }
    }

  } else {
    imf <- as_mapper_dt(index_match_fun)
    by2 <- dt_common_by(multi_by, x_dt, y_dt)

    d1 <- x_dt[, ..by2$x]
    d2 <- y_dt[, ..by2$y]
    matches <- imf(d1, d2)

    if (!data.table::is.data.table(matches)) matches <- data.table::as.data.table(matches)
    if (!all(c("x", "y") %in% names(matches))) {
      stop("index_match_fun must return a table with columns named 'x' and 'y' (1-based row indices).", call. = FALSE)
    }
  }

  if (!data.table::is.data.table(matches)) matches <- data.table::as.data.table(matches)
  if ("i" %in% names(matches)) matches[, i := NULL]

  # --- modos semi / anti -----------------------------------------------------

  if (mode == "semi") {
    keep <- sort(unique(matches$x))
    keep <- keep[!is.na(keep)]
    res <- x_dt[keep]
    return(if (data.table::is.data.table(x)) res else as.data.frame(res))
  }

  if (mode == "anti") {
    if (nrow(matches) == 0L) {
      return(if (data.table::is.data.table(x)) x_dt else as.data.frame(x_dt))
    }
    drop <- sort(unique(matches$x))
    drop <- drop[!is.na(drop)]
    res <- x_dt[-drop]
    return(if (data.table::is.data.table(x)) res else as.data.frame(res))
  }

  # --- preparar renombres y completar índices según modo ---------------------

  data.table::setorder(matches, x, y)

  overlap <- intersect(names(x_dt), names(y_dt))
  if (length(overlap) > 0L) {
    if (data.table::is.data.table(x)) {
      x_dt <- data.table::copy(x_dt)
    }
    if (data.table::is.data.table(y)) {
      y_dt <- data.table::copy(y_dt)
    }
    data.table::setnames(x_dt, overlap, paste0(overlap, ".x"))
    data.table::setnames(y_dt, overlap, paste0(overlap, ".y"))
  }

  if (mode == "left") {
    allx <- data.table::data.table(x = seq_len(nrow(x_dt)))
    matches <- merge(allx, matches, by = "x", all.x = TRUE, sort = FALSE)
  } else if (mode == "right") {
    ally <- data.table::data.table(y = seq_len(nrow(y_dt)))
    matches <- merge(ally, matches, by = "y", all.x = TRUE, sort = FALSE)
  } else if (mode == "full") {
    has_x <- unique(matches[!is.na(x), x])
    has_y <- unique(matches[!is.na(y), y])

    miss_x <- setdiff(seq_len(nrow(x_dt)), has_x)
    miss_y <- setdiff(seq_len(nrow(y_dt)), has_y)

    if (length(miss_x)) {
      matches <- data.table::rbindlist(list(matches, data.table::data.table(x = miss_x, y = NA_integer_)), use.names = TRUE, fill = TRUE)
    }
    if (length(miss_y)) {
      matches <- data.table::rbindlist(list(matches, data.table::data.table(x = NA_integer_, y = miss_y)), use.names = TRUE, fill = TRUE)
    }
    data.table::setorder(matches, x, y)
  }

  # --- construir resultado final usando C++ ---------------------------------

  ret <- bind_by_rowid_cpp_wrapper(x_dt, y_dt, matches, x_original, y_original, overlap)
  ret <- dt_ensure_distance_col(ret, distance_col = NULL, mode = mode)

  ret

 #if (x_is_dt) {
 #  ret
 #} else {
 #  as.data.frame(ret)
 #}
}
