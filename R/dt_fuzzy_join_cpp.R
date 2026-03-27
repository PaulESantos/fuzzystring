#' Fuzzy join backend using 'data.table' plus compiled 'C++' assembly
#'
#' Low-level engine used by \code{\link{fuzzystring_join}} and the compiled
#' fuzzy join helpers. It builds the match index with R 'data.table' and then
#' expands and assembles the result using compiled 'C++' binding code for speed.
#'
#' @param match_fun A function used to match values. It must return a logical
#'   vector (or a data.frame/data.table whose first column is logical) indicating
#'   which pairs match. For multi-column joins, you may pass a list of functions
#'   (one per column).
#' @param x A \code{data.frame} or \code{data.table}.
#' @param y A \code{data.frame} or \code{data.table}.
#' @param by Columns by which to join the two tables. See
#'   \code{\link{fuzzystring_join}}.
#' @param multi_by A character vector of column names used for multi-column
#'   matching when \code{multi_match_fun} is supplied.
#' @param multi_match_fun A function that receives matrices of unique values for
#'   \code{x} and \code{y} (rows correspond to unique combinations of \code{multi_by}).
#'   It must return a logical vector (or a data.frame/data.table whose first column
#'   is logical) indicating which rows match.
#' @param index_match_fun A function that receives the joined columns from
#'   \code{x} and \code{y} and returns a table with integer columns \code{x} and
#'   \code{y} (1-based row indices).
#' @param mode One of \code{"inner"}, \code{"left"}, \code{"right"}, \code{"full"},
#'   \code{"semi"}, or \code{"anti"}.
#' @param ... Additional arguments passed to the matching function(s).
#'
#' @return A joined table that preserves the container class of \code{x}:
#'   \code{data.table} inputs return \code{data.table}, tibble inputs return
#'   tibble, and plain \code{data.frame} inputs return plain
#'   \code{data.frame}. See \code{\link{fuzzystring_join}}.
#'
#' @details
#' This function works like \code{\link{fuzzystring_join}}, but replaces the
#' R-based output assembly with a compiled 'C++' implementation. This provides
#' better performance, especially for large joins, outer joins, and wide tables.
#' It is intended as a backend and does not compute distances itself; use
#' \code{\link{fuzzystring_join}} for string-distance based matching.
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
#' @importFrom stats as.formula
#' @importFrom data.table as.data.table is.data.table copy setorder setnames CJ rbindlist melt dcast .I .N :=
#' @keywords internal
fuzzystring_join_backend <- function(x, y, by = NULL, match_fun = NULL,
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
  x_is_tibble <- inherits(x, "tbl_df")

  restore_output_class <- function(out) {
    if (x_is_dt) {
      return(out)
    }

    if (x_is_tibble) {
      if (!requireNamespace("tibble", quietly = TRUE)) {
        stop(
          "Returning tibble output requires the 'tibble' package to be installed.",
          call. = FALSE
        )
      }
      return(tibble::as_tibble(out, .name_repair = "minimal"))
    }

    as.data.frame(out)
  }

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
    # Group rows by values in a single column and return row indices for each group.
    # Used to identify which rows in x/y have the same value before matching.
    # Returns: data.table with columns (col_name, indices) where indices is a list of row numbers
    dt[, .(indices = list(.I)), by = c(col), verbose = FALSE]
  }

  group_indices_multicol <- function(dt, cols) {
    # Group rows by values in multiple columns and return row indices for each combination.
    # Used for multi-column fuzzy matching (e.g., match on both "first_name" and "last_name").
    # Returns: data.table with columns (cols..., indices) where indices is a list of row numbers
    dt[, .(indices = list(.I)), by = cols, verbose = FALSE]
  }

  expand_index_lists <- function(x_lists, y_lists) {
    # Compute Cartesian product of index groups for matched values.
    #
    # When two values match (e.g., "Alice" from x matches "Alice" from y),
    # all row pairs must be combined: if x has rows [1,3] and y has rows [2,5],
    # the output includes pairs (1,2), (1,5), (3,2), (3,5).
    #
    # Args:
    #   x_lists: list of integer vectors, each containing row indices for one value
    #   y_lists: corresponding list for y table (same length as x_lists)
    #
    # Returns: list with components:
    #   x: all row indices from x, repeated appropriately
    #   y: all row indices from y, repeated appropriately
    #   x_len, y_len: original group sizes (for validation/bookkeeping)

    x_len <- lengths(x_lists)
    y_len <- lengths(y_lists)

    rep_counts <- x_len * y_len  # Cartesian product size per group
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
        # Repeat each x index ly times (expanding rows for all y in this group)
        x_rep[pos:end_pos] <- rep.int(x_lists[[i]], times = ly)
        # Repeat y indices each lx times (pairing each y with all x in this group)
        y_rep[pos:end_pos] <- rep(y_lists[[i]], each = lx)
        pos <- end_pos + 1L
      }
    }

    list(x = x_rep, y = y_rep, x_len = x_len, y_len = y_len)
  }

  replicate_extras <- function(extra_dt, w, x_len, y_len) {
    # Replicate extra columns (e.g., distance values) to match Cartesian product expansion.
    # When matching produces n Cartesian product rows from one group, the extra column value
    # (which is computed once per group) must be replicated n times.
    #
    # Args:
    #   extra_dt: data.table with extra columns (e.g., distance values), one row per match
    #   w: indices of matched groups (1-based, to index into extra_dt)
    #   x_len, y_len: group sizes used to compute rep_counts = x_len * y_len
    #
    # Returns: expanded extra_dt with rows replicated according to Cartesian product sizes
    if (is.null(extra_dt)) return(NULL)
    rep_counts <- x_len * y_len
    if (sum(rep_counts) == 0L) return(extra_dt[0])
    idx <- rep.int(w, times = rep_counts)
    extra_dt[idx]
  }

  # --- C++ binding function --------------------------------------------------
  # Uses the compiled C++ function instead of R implementation

  bind_by_rowid_cpp_wrapper <- function(x_dt2, y_dt2, matches_dt, overlap = character(0)) {
    ret <- bind_by_rowid_cpp_matches(x_dt2, y_dt2, matches_dt, overlap)
    if (!data.table::is.data.table(ret)) {
      ret <- data.table::as.data.table(ret)
    }
    ret
  }

  # --- construir matches (same logic as dt_fuzzy_join) -----------------------

  matches <- NULL

  if (!is.null(match_fun)) {
    by2 <- fst_common_by(by, x_dt, y_dt)

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
    by2 <- fst_common_by(multi_by, x_dt, y_dt)

    ix <- group_indices_multicol(x_dt, by2$x)
    iy <- group_indices_multicol(y_dt, by2$y)

    ux <- as.matrix(ix[, by2$x, with = FALSE])
    uy <- as.matrix(iy[, by2$y, with = FALSE])

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
    by2 <- fst_common_by(multi_by, x_dt, y_dt)

    d1 <- x_dt[, by2$x, with = FALSE]
    d2 <- y_dt[, by2$y, with = FALSE]
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
    return(restore_output_class(res))
  }

  if (mode == "anti") {
    if (nrow(matches) == 0L) {
      return(restore_output_class(x_dt))
    }
    drop <- sort(unique(matches$x))
    drop <- drop[!is.na(drop)]
    res <- x_dt[-drop]
    return(restore_output_class(res))
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

  ret <- bind_by_rowid_cpp_wrapper(x_dt, y_dt, matches, overlap)
  ret <- fst_ensure_distance_col(ret, distance_col = NULL, mode = mode)

  restore_output_class(ret)
}
#
