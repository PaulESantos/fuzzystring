#' Null-coalescing operator (internal)
#'
#' Internal helper that returns \code{y} only when \code{x} is \code{NULL},
#' otherwise returns \code{x}. This is a small convenience used throughout the
#' package to express "use a default if NULL" semantics.
#'
#' @param x An object that may be \code{NULL}.
#' @param y A default value returned when \code{x} is \code{NULL}.
#'
#' @return \code{x} if it is not \code{NULL}; otherwise \code{y}.
#'
#' @keywords internal
#' @noRd
`%||%` <- function(x, y) if (is.null(x)) y else x

fst_checked_cartesian_sizes <- function(x_len, y_len) {
  rep_counts <- as.double(x_len) * as.double(y_len)
  total_size <- sum(rep_counts)

  if (!is.finite(total_size) || total_size > .Machine$integer.max) {
    stop(
      "The join result is too large to represent safely.",
      call. = FALSE
    )
  }

  list(
    rep_counts = as.integer(rep_counts),
    total_size = as.integer(total_size)
  )
}

fst_validate_join_inputs <- function(
  x,
  y,
  by,
  max_dist,
  ignore_case,
  distance_col
) {
  if (!is.data.frame(x) || !is.data.frame(y)) {
    stop("`x` and `y` must be data frames.", call. = FALSE)
  }
  if (
    !is.numeric(max_dist) ||
      length(max_dist) != 1L ||
      is.na(max_dist) ||
      !is.finite(max_dist) ||
      max_dist < 0
  ) {
    stop(
      "`max_dist` must be a single finite non-negative number.",
      call. = FALSE
    )
  }
  if (
    !is.logical(ignore_case) || length(ignore_case) != 1L || is.na(ignore_case)
  ) {
    stop("`ignore_case` must be `TRUE` or `FALSE`.", call. = FALSE)
  }
  if (!is.null(distance_col)) {
    valid_distance_col <- is.character(distance_col) &&
      length(distance_col) == 1L &&
      !is.na(distance_col) &&
      nzchar(distance_col)
    if (!valid_distance_col) {
      stop(
        "`distance_col` must be `NULL` or one non-empty string.",
        call. = FALSE
      )
    }
    unavailable <- c("x", "y", "i", names(x), names(y))
    if (distance_col %in% unavailable) {
      stop(
        "`distance_col` must not collide with input or internal columns.",
        call. = FALSE
      )
    }
  }

  by2 <- fst_common_by(by, x, y)
  valid_by <- is.list(by2) &&
    identical(sort(names(by2)), c("x", "y")) &&
    is.character(by2$x) &&
    is.character(by2$y) &&
    length(by2$x) > 0L &&
    length(by2$x) == length(by2$y) &&
    !anyNA(by2$x) &&
    !anyNA(by2$y) &&
    all(nzchar(by2$x)) &&
    all(nzchar(by2$y))
  if (!valid_by) {
    stop("`by` must specify at least one valid pair of columns.", call. = FALSE)
  }
  missing_x <- setdiff(by2$x, names(x))
  missing_y <- setdiff(by2$y, names(y))
  if (length(missing_x) || length(missing_y)) {
    missing <- character()
    if (length(missing_x)) {
      missing <- c(missing, paste0("x$", missing_x))
    }
    if (length(missing_y)) {
      missing <- c(missing, paste0("y$", missing_y))
    }
    stop(
      "Join columns not found: ",
      paste(missing, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  by2
}


#' Normalize join columns specification (data.table backend) (internal)
#'
#' Internal helper that standardizes a join specification into a list with
#' components \code{x} and \code{y}, containing the column names to use from
#' each input table.
#'
#' This mirrors the behavior of the \code{common_by()} helper used in
#' dplyr/tidyr-style joins, but is implemented using base R / data.table
#' conventions.
#'
#' @param by Join specification. May be:
#' \itemize{
#'   \item \code{NULL}: use the intersection of \code{names(x)} and \code{names(y)}.
#'   \item A character vector: column names used in both \code{x} and \code{y}.
#'   \item A named character vector: names are columns in \code{x}, values are columns in \code{y}.
#'   \item A list with components \code{x} and \code{y}: returned as-is.
#' }
#' @param x A \code{data.frame} or \code{data.table}.
#' @param y A \code{data.frame} or \code{data.table}.
#'
#' @details
#' If \code{by} is \code{NULL}, the function uses the intersection of column
#' names in \code{x} and \code{y}. If there are no common variables, it throws
#' an error and asks the caller to provide \code{by}.
#'
#' If \code{by} is a partially named character vector, unnamed entries (i.e.
#' \code{""} names) are assumed to refer to identical column names in both
#' tables.
#'
#' The function prints a message when joining by common variables (i.e. when
#' \code{by = NULL}), matching dplyr's behavior.
#'
#' @return A list with:
#' \itemize{
#'   \item \code{x}: character vector of column names in \code{x}
#'   \item \code{y}: character vector of column names in \code{y}
#' }
#'
#' @keywords internal
#' @noRd
fst_common_by <- function(by = NULL, x, y) {
  if (is.list(by)) {
    return(by)
  }

  if (!is.null(by)) {
    x_names <- names(by) %||% by
    y_names <- unname(by)

    # If x is partially named, assume blanks refer to identical names in y
    x_names[x_names == ""] <- y_names[x_names == ""]

    return(list(x = x_names, y = y_names))
  }

  common <- intersect(names(x), names(y))

  if (length(common) == 0) {
    stop("No common variables. Please specify `by` param.", call. = FALSE)
  }

  message("Joining by: ", paste(common, collapse = ", "))

  list(x = common, y = common)
}


#' Ensure distance column exists (data.table backend) (internal)
#'
#' Internal helper used by joins that optionally return a distance/similarity
#' metric (e.g. \code{stringdist_join()}). When \code{distance_col} is requested
#' and the join mode is not \code{"semi"} or \code{"anti"}, this function ensures
#' the output contains a column named \code{distance_col}.
#'
#' For \code{data.table} inputs, the column is added by reference via
#' \code{data.table::set()} for efficiency.
#'
#' @param ret A join result. Usually a \code{data.table}, but may be a
#'   \code{data.frame}.
#' @param distance_col A string naming the distance column to ensure, or
#'   \code{NULL} to do nothing.
#' @param mode Join mode. One of \code{"inner"}, \code{"left"}, \code{"right"},
#'   \code{"full"}, \code{"semi"}, or \code{"anti"}.
#'
#' @details
#' If \code{ret} already contains \code{distance_col}, it is returned unchanged.
#' Otherwise:
#' \itemize{
#'   \item If \code{nrow(ret) == 0}, an empty numeric vector is used.
#'   \item Else the column is filled with \code{NA}.
#' }
#'
#' No column is added for \code{mode = "semi"} or \code{mode = "anti"}, since
#' those modes return subsets of \code{x} only.
#'
#' @return \code{ret}, potentially modified to include \code{distance_col}.
#'
#' @keywords internal
#' @noRd
fst_ensure_distance_col <- function(ret, distance_col, mode) {
  if (!(mode %in% c("semi", "anti")) && !is.null(distance_col)) {
    if (is.null(ret[[distance_col]])) {
      val <- if (nrow(ret) == 0) numeric(0) else rep(NA_real_, nrow(ret))

      if (data.table::is.data.table(ret)) {
        # Asegurar que tiene espacio pre-alocado para nuevas columnas
        data.table::setalloccol(ret)
        data.table::set(ret, j = distance_col, value = val)
      } else {
        ret[[distance_col]] <- val
      }
    }
  }
  ret
}


#' Drop row names (internal)
#'
#' Internal helper that removes row names from a \code{data.frame}-like object.
#' This mirrors the behavior of the original implementation used to avoid
#' propagating row names through join results.
#'
#' For \code{data.table}, row names are not normally used, but some operations
#' may attach a \code{"rownames"} attribute. This helper normalizes the result
#' by resetting that attribute to the internal "row names" representation.
#'
#' @param x A \code{data.frame} or \code{data.table}.
#'
#' @return \code{x} with row names removed/reset.
#'
#' @keywords internal
#' @noRd
fst_unrowwname <- function(x) {
  if (data.table::is.data.table(x)) {
    data.table::setattr(x, "rownames", .set_row_names(nrow(x)))
  } else {
    rownames(x) <- NULL
  }
  x
}
