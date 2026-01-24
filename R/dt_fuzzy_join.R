#' Fuzzy join two tables using data.table
#'
#' Join two tables based on approximate matches rather than exact equality.
#' This data.table-backed implementation generates candidate pairs by comparing
#' all pairs of *unique* values (or unique rows when using \code{multi_match_fun}),
#' then expands those matches back to original row indices.
#'
#' Exactly one of \code{match_fun}, \code{multi_match_fun}, or \code{index_match_fun}
#' must be supplied.
#'
#' @param x A \code{data.frame} or \code{data.table}. This is the primary table.
#' @param y A \code{data.frame} or \code{data.table}. The table to join to \code{x}.
#' @param by Join columns specification for per-column matching (used with \code{match_fun}).
#'   Can be:
#'   \itemize{
#'     \item \code{NULL} (default): uses the intersection of column names of \code{x} and \code{y}.
#'     \item A character vector: column names used in both \code{x} and \code{y}.
#'     \item A named character vector: names are columns in \code{x}, values are columns in \code{y}.
#'     \item A list with components \code{x} and \code{y} (already normalized).
#'   }
#' @param match_fun A vectorized function used to match each pair of columns specified
#'   by \code{by}. It is called once per \code{by} column pair on vectors that represent
#'   all unique cross-comparisons between \code{x} and \code{y}.
#'
#'   \code{match_fun} may be:
#'   \itemize{
#'     \item A single function (recycled across all column pairs), or
#'     \item A list of functions (one per pair), optionally named by \code{x} column names.
#'   }
#'
#'   The function must return either:
#'   \itemize{
#'     \item A logical vector indicating which comparisons match, or
#'     \item A \code{data.frame}/\code{data.table} whose first column is logical and
#'       remaining columns are treated as additional "extra" columns appended to the output
#'       (e.g., distances or similarity scores).
#'   }
#' @param multi_by Join columns specification for multi-column matching (used with
#'   \code{multi_match_fun} or \code{index_match_fun}). Same forms as \code{by}.
#' @param multi_match_fun A vectorized function used to match on multiple columns simultaneously.
#'   It is called on two matrices: \code{ux_input} and \code{uy_input}, where each row represents
#'   one unique combination of \code{multi_by} columns from \code{x} and \code{y}.
#'
#'   It must return either a logical vector or a \code{data.frame}/\code{data.table} where the
#'   first column is logical and remaining columns are appended as "extra" columns.
#' @param index_match_fun A function that performs matching directly on the original columns and
#'   returns an index table. It is called as \code{index_match_fun(d1, d2)}, where \code{d1} is
#'   \code{x[, multi_by$x, with = FALSE]} and \code{d2} is \code{y[, multi_by$y, with = FALSE]}.
#'
#'   It must return a \code{data.frame}/\code{data.table} with integer columns \code{x} and \code{y}
#'   containing 1-based row indices into \code{x} and \code{y}.
#' @param mode Join mode. One of \code{"inner"}, \code{"left"}, \code{"right"}, \code{"full"},
#'   \code{"semi"}, or \code{"anti"}.
#' @param ... Extra arguments passed to \code{match_fun} or \code{multi_match_fun}.
#'
#' @details
#' For \code{match_fun}, when multiple columns are provided in \code{by}, a pair of rows
#' \code{(x_i, y_j)} is retained only if it matches on \emph{all} column pairs.
#'
#' If \code{x} and \code{y} share column names, overlapping columns are renamed to \code{.x}
#' (for \code{x}) and \code{.y} (for \code{y}) in the output.
#'
#' \strong{Complexity note:} candidate generation compares all unique values (or unique rows)
#' between tables, which can be expensive for high-cardinality keys (approximately
#' \code{O(Ux * Uy)}). Consider blocking/prefiltering strategies or \code{index_match_fun}
#' for large problems.
#'
#' @return
#' A joined table in the same container type as \code{x}:
#' \itemize{
#'   \item If \code{x} is a \code{data.table}, returns a \code{data.table}.
#'   \item Otherwise returns a \code{data.frame}.
#' }
#'
#' For \code{mode = "semi"} the result contains only rows of \code{x} that have at least one match.
#' For \code{mode = "anti"} the result contains only rows of \code{x} that have no matches.
#' For other modes, the result contains columns from both \code{x} and \code{y}, plus any extra
#' columns produced by \code{match_fun}/\code{multi_match_fun}.
#'
#' @examples
#' library(data.table)
#'
#' x <- data.table(id = 1:3, name = c("Peru", "Brasil", "Chile"))
#' y <- data.table(code = c("PE", "BR", "CL"), country = c("Perú", "Brazil", "Chili"))
#'
#' # Example match_fun: case-insensitive exact match after ASCII transliteration
#' norm <- function(z) tolower(iconv(z, to = "ASCII//TRANSLIT"))
#' mf <- function(a, b) norm(a) == norm(b)
#'
#' dt_fuzzy_inner_join(x, y, by = c(name = "country"), match_fun = mf)
#'
#' # Example returning extra columns (distance)
#' dist_mf <- function(a, b) {
#'   d <- adist(a, b)
#'   data.frame(match = d <= 1, dist = as.integer(d))
#' }
#'
#' dt_fuzzy_left_join(x, y, by = c(name = "country"), match_fun = dist_mf)
#'
#' @export
#'
#' @importFrom stats as.formula
#' @importFrom data.table as.data.table is.data.table copy setorder setnames CJ rbindlist melt dcast .I .N :=
#' @export
dt_fuzzy_join <- function(x, y, by = NULL, match_fun = NULL,
                       multi_by = NULL, multi_match_fun = NULL,
                       index_match_fun = NULL, mode = "inner", ...) {

  mode <- match.arg(mode, c("inner", "left", "right", "full", "semi", "anti"))

  non_nulls <- (!is.null(multi_match_fun)) +
    (!is.null(match_fun)) +
    (!is.null(index_match_fun))
  if (sum(non_nulls) != 1) {
    stop("Must give exactly one of match_fun, multi_match_fun, and index_match_fun", call. = FALSE)
  }

  # Guardamos tipo original de x para retornar el tipo correcto
  x_is_dt <- data.table::is.data.table(x)

  # OPTIMIZACIÓN: Para matching, necesitamos data.table solo para group operations
  # Mantener referencias originales para subsetting eficiente
  x_original <- x
  y_original <- y

  # Convertir a data.table solo si no lo es ya (para group_indices)
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

  # --- helpers internos ------------------------------------------------------

  as_mapper_dt <- function(f) {
    # Soporta función o fórmula simple (~ expr) sin purrr
    if (inherits(f, "formula")) {
      stop("Formula notation (~) not supported in this data.table version yet. Pass a function.", call. = FALSE)
    }
    if (!is.function(f)) stop("match_fun / multi_match_fun / index_match_fun must be a function.", call. = FALSE)
    f
  }

  # genera grupos únicos con lista de índices (.I)
  # Optimizado: usa .GRP para evitar crear listas innecesarias
  group_indices_1col <- function(dt, col) {
    dt[, .(indices = list(.I)), by = c(col), verbose = FALSE]
  }

  group_indices_multicol <- function(dt, cols) {
    dt[, .(indices = list(.I)), by = cols, verbose = FALSE]
  }

  # Expande listas de índices x/y (cartesiano por par de grupos)
  # OPTIMIZADO: Vectorización pura sin Map/unlist
  expand_index_lists <- function(x_lists, y_lists) {
    x_len <- lengths(x_lists)
    y_len <- lengths(y_lists)

    # Pre-calcular tamaño total para pre-alocar
    rep_counts <- x_len * y_len
    total_size <- sum(rep_counts)

    if (total_size == 0L) {
      return(list(x = integer(0), y = integer(0), x_len = x_len, y_len = y_len))
    }

    # Pre-alocar vectores de salida
    x_rep <- integer(total_size)
    y_rep <- integer(total_size)

    # Llenar usando vectorización eficiente
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

  # Replica extra cols por cada expansión (una fila por par de grupos, se repite por lx*ly)
  # OPTIMIZADO: Evita drop=FALSE y usa indexación de data.table
  replicate_extras <- function(extra_dt, w, x_len, y_len) {
    if (is.null(extra_dt)) return(NULL)
    rep_counts <- x_len * y_len
    if (sum(rep_counts) == 0L) return(extra_dt[0])
    idx <- rep.int(w, times = rep_counts)
    extra_dt[idx]
  }

  # Ensambla salida final según matches (con NA rows cuando haga falta)
  # OPTIMIZADO: Usa subsetting directo cuando es posible, evita copias innecesarias
  # x_dt2/y_dt2 son data.tables para group operations
  # x_orig/y_orig son formatos originales para subsetting eficiente
  bind_by_rowid <- function(x_dt2, y_dt2, matches_dt, x_orig = x_dt2, y_orig = y_dt2) {
    n_rows <- nrow(matches_dt)

    if (n_rows == 0L) {
      # Caso especial: sin matches
      ret <- data.table::data.table()
      for (col in names(x_dt2)) ret[[col]] <- x_dt2[[col]][0]
      for (col in names(y_dt2)) ret[[col]] <- y_dt2[[col]][0]
      extra_cols <- setdiff(names(matches_dt), c("x", "y", "i"))
      for (col in extra_cols) ret[[col]] <- matches_dt[[col]]
      return(ret)
    }

    # OPTIMIZACIÓN: Para inner joins (sin NAs), usar subsetting directo
    x_idx <- matches_dt$x
    y_idx <- matches_dt$y
    has_na <- any(is.na(x_idx)) || any(is.na(y_idx))

    if (!has_na) {
      # OPTIMIZACIÓN CLAVE para inner joins:
      # Enfoque simple: subset, cbind, convertir UNA VEZ al final
      # Usamos x_dt2/y_dt2 (que ya tienen sufijos .x/.y aplicados si hay overlap)
      # Si son data.tables, subsetting es eficiente por referencia
      # Si eran data.frames originales pero se convirtieron, usamos x_orig/y_orig

      # Determinar si usar dt2 (ya tiene sufijos) o orig (más eficiente si no hay overlap)
      use_x <- if (identical(x_dt2, x_orig)) x_orig else x_dt2
      use_y <- if (identical(y_dt2, y_orig)) y_orig else y_dt2

      # Subset
      x_subset <- use_x[x_idx, , drop = FALSE]
      y_subset <- use_y[y_idx, , drop = FALSE]

      # Combinar con cbind
      ret <- cbind(x_subset, y_subset)

      # Convertir a data.table UNA SOLA VEZ al final (si no lo es ya)
      if (!data.table::is.data.table(ret)) {
        ret <- data.table::as.data.table(ret)
      }

      # Agregar columnas extra si existen
      extra_cols <- setdiff(names(matches_dt), c("x", "y", "i"))
      if (length(extra_cols) > 0L) {
        data.table::setalloccol(ret)
        for (col in extra_cols) {
          data.table::set(ret, j = col, value = matches_dt[[col]])
        }
      }

      return(ret)
    }

    # Para outer joins: usar estrategia diferente
    # Crear una tabla resultado vacía con la estructura correcta
    ret <- data.table::data.table(.dummy = seq_len(n_rows))

    # Agregar columnas de x (usar original para acceso eficiente)
    for (col in names(x_orig)) {
      col_data <- x_orig[[col]]
      # Crear columna llena de NAs
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

      # Llenar donde hay índices válidos
      valid_x <- !is.na(x_idx)
      if (any(valid_x)) {
        new_col[valid_x] <- col_data[x_idx[valid_x]]
      }
      ret[[col]] <- new_col
    }

    # Agregar columnas de y (usar original para acceso eficiente)
    for (col in names(y_orig)) {
      col_data <- y_orig[[col]]
      # Crear columna llena de NAs
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

      # Llenar donde hay índices válidos
      valid_y <- !is.na(y_idx)
      if (any(valid_y)) {
        new_col[valid_y] <- col_data[y_idx[valid_y]]
      }
      ret[[col]] <- new_col
    }

    # Remover columna dummy
    ret[, .dummy := NULL]

    # Agregar columnas extra
    extra_cols <- setdiff(names(matches_dt), c("x", "y", "i"))
    if (length(extra_cols) > 0L) {
      data.table::setalloccol(ret)
      for (col in extra_cols) {
        data.table::set(ret, j = col, value = matches_dt[[col]])
      }
    }

    ret
  }

  # --- construir matches -----------------------------------------------------

  matches <- NULL

  if (!is.null(match_fun)) {
    by2 <- dt_common_by(by, x_dt, y_dt)

    # normalizar match_fun: función única o lista por columna
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

    # para cada par de columnas (xcol, ycol) calcula matches de índices
    parts <- vector("list", length(by2$x))

    for (i in seq_along(by2$x)) {
      xcol <- by2$x[i]
      ycol <- by2$y[i]

      # grupos únicos con lista de índices
      ix <- group_indices_1col(x_dt, xcol)
      iy <- group_indices_1col(y_dt, ycol)

      ux <- ix[[xcol]]
      uy <- iy[[ycol]]

      n_x <- length(ux)
      n_y <- length(uy)

      mf <- if (!is.null(names(mf_list))) {
        # lista nombrada por nombres en x (compat simple)
        mf_list[[xcol]]
      } else {
        mf_list[[i]]
      }

      # comparar todos los pares de únicos de forma vectorizada
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
      # quedarnos solo con pares (x,y) que pasaron todas las columnas
      accept <- matches[, .N, by = .(x, y)][N == length(by2$x), .(x, y)]
      matches <- matches[accept, on = .(x, y), nomatch = 0L]

      # OPTIMIZADO: Evita melt/dcast usando dcast directamente (más eficiente que antes)
      extra_cols <- setdiff(names(matches), c("i", "x", "y"))
      if (length(extra_cols) > 0L) {
        matches[, name := by2$x[i]]

        # Usar dcast pero de forma más eficiente: una sola llamada
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
      # una sola columna: si hay extras mantenerlos tal cual; si no, distinct x,y
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
          # aquí cada fila extra_dt corresponde a un par de grupos (keep),
          # replicamos por lx*ly del par
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

  # OPTIMIZADO: Renombrar solo si es necesario, y crear copias solo cuando vayamos a modificar
  overlap <- intersect(names(x_dt), names(y_dt))
  if (length(overlap) > 0L) {
    # Solo copiar si el input original era data.table (para no modificarlo)
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
    # agrega x-only y y-only
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

  # --- construir resultado final --------------------------------------------

  ret <- bind_by_rowid(x_dt, y_dt, matches, x_original, y_original)
  ret <- dt_ensure_distance_col(ret, distance_col = NULL, mode = mode)

  # Retornar el tipo correcto según la entrada
  ret
  #if (x_is_dt) {
  #  ret
  #} else {
  #  as.data.frame(ret)
  #}
}

#' Fuzzy join with chunked binding
#'
#' This variant of \code{\link{dt_fuzzy_join}} performs the final binding step
#' in chunks to reduce peak memory usage for massive matches.
#'
#' @inheritParams dt_fuzzy_join
#' @param chunk_size Number of rows from the matches table to bind per chunk.
#'   When \code{NULL} or \code{0}, binding is done in a single pass.
#'
#' @return See \code{\link{dt_fuzzy_join}}.
#' @export
dt_fuzzy_join_chunk <- function(x, y, by = NULL, match_fun = NULL,
                             multi_by = NULL, multi_match_fun = NULL,
                             index_match_fun = NULL, mode = "inner",
                             chunk_size = 100000L, ...) {

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

  bind_by_rowid <- function(x_dt2, y_dt2, matches_dt, x_orig = x_dt2, y_orig = y_dt2) {
    n_rows <- nrow(matches_dt)

    if (n_rows == 0L) {
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

    if (!has_na) {
      use_x <- if (identical(x_dt2, x_orig)) x_orig else x_dt2
      use_y <- if (identical(y_dt2, y_orig)) y_orig else y_dt2

      x_subset <- use_x[x_idx, , drop = FALSE]
      y_subset <- use_y[y_idx, , drop = FALSE]

      ret <- cbind(x_subset, y_subset)

      if (!data.table::is.data.table(ret)) {
        ret <- data.table::as.data.table(ret)
      }

      extra_cols <- setdiff(names(matches_dt), c("x", "y", "i"))
      if (length(extra_cols) > 0L) {
        data.table::setalloccol(ret)
        for (col in extra_cols) {
          data.table::set(ret, j = col, value = matches_dt[[col]])
        }
      }

      return(ret)
    }

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

  bind_by_rowid_chunked <- function(x_dt2, y_dt2, matches_dt, x_orig = x_dt2, y_orig = y_dt2, chunk_size = 0L) {
    n_rows <- nrow(matches_dt)
    if (is.null(chunk_size) || chunk_size <= 0L || n_rows <= chunk_size) {
      return(bind_by_rowid(x_dt2, y_dt2, matches_dt, x_orig, y_orig))
    }

    starts <- seq.int(1L, n_rows, by = chunk_size)
    parts <- vector("list", length(starts))
    for (i in seq_along(starts)) {
      start <- starts[i]
      end <- min(start + chunk_size - 1L, n_rows)
      chunk <- matches_dt[start:end]
      parts[[i]] <- bind_by_rowid(x_dt2, y_dt2, chunk, x_orig, y_orig)
    }
    data.table::rbindlist(parts, use.names = TRUE, fill = TRUE)
  }

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

  ret <- bind_by_rowid_chunked(x_dt, y_dt, matches, x_original, y_original, chunk_size = chunk_size)
  ret <- dt_ensure_distance_col(ret, distance_col = NULL, mode = mode)

  ret
}

# Wrappers ------------------------------------------------------------

#' Fuzzy inner join
#'
#' Convenience wrapper for \code{fuzzy_join(mode = "inner")}.
#'
#' @inheritParams dt_fuzzy_join
#' @return See \code{\link{dt_fuzzy_join}}.
#' @export
dt_fuzzy_inner_join <- function(x, y, by = NULL, match_fun, ...) {
  dt_fuzzy_join(x, y, by = by, match_fun = match_fun, mode = "inner", ...)
}

#' Fuzzy left join
#'
#' Convenience wrapper for \code{dt_fuzzy_join(mode = "left")}.
#'
#' @inheritParams dt_fuzzy_join
#' @return See \code{\link{dt_fuzzy_join}}.
#' @export
dt_fuzzy_left_join <- function(x, y, by = NULL, match_fun, ...) {
  dt_fuzzy_join(x, y, by = by, match_fun = match_fun, mode = "left", ...)
}

#' Fuzzy right join
#'
#' Convenience wrapper for \code{dt_fuzzy_join(mode = "right")}.
#'
#' @inheritParams dt_fuzzy_join
#' @return See \code{\link{dt_fuzzy_join}}.
#' @export
dt_fuzzy_right_join <- function(x, y, by = NULL, match_fun, ...) {
  dt_fuzzy_join(x, y, by = by, match_fun = match_fun, mode = "right", ...)
}

#' Fuzzy full join
#'
#' Convenience wrapper for \code{dt_fuzzy_join(mode = "full")}.
#'
#' @inheritParams dt_fuzzy_join
#' @return See \code{\link{dt_fuzzy_join}}.
#' @export
dt_fuzzy_full_join <- function(x, y, by = NULL, match_fun, ...) {
  dt_fuzzy_join(x, y, by = by, match_fun = match_fun, mode = "full", ...)
}

#' Fuzzy semi join
#'
#' Convenience wrapper for \code{dt_fuzzy_join(mode = "semi")}.
#'
#' @inheritParams dt_fuzzy_join
#' @return See \code{\link{dt_fuzzy_join}}.
#' @export
dt_fuzzy_semi_join <- function(x, y, by = NULL, match_fun, ...) {
  dt_fuzzy_join(x, y, by = by, match_fun = match_fun, mode = "semi", ...)
}

#' Fuzzy anti join
#'
#' Convenience wrapper for \code{dt_fuzzy_join(mode = "anti")}.
#'
#' @inheritParams dt_fuzzy_join
#' @return See \code{\link{dt_fuzzy_join}}.
#' @export
dt_fuzzy_anti_join <- function(x, y, by = NULL, match_fun, ...) {
  dt_fuzzy_join(x, y, by = by, match_fun = match_fun, mode = "anti", ...)
}

# Wrappers (chunk) ----------------------------------------------------

#' Fuzzy inner join (chunked binding)
#'
#' Convenience wrapper for \code{dt_fuzzy_join_chunk(mode = "inner")}.
#'
#' @inheritParams dt_fuzzy_join_chunk
#' @return See \code{\link{dt_fuzzy_join_chunk}}.
#' @export
dt_fuzzy_inner_join_chunk <- function(x, y, by = NULL, match_fun, chunk_size = 100000L, ...) {
  dt_fuzzy_join_chunk(x, y, by = by, match_fun = match_fun, mode = "inner", chunk_size = chunk_size, ...)
}

#' Fuzzy left join (chunked binding)
#'
#' Convenience wrapper for \code{dt_fuzzy_join_chunk(mode = "left")}.
#'
#' @inheritParams dt_fuzzy_join_chunk
#' @return See \code{\link{dt_fuzzy_join_chunk}}.
#' @export
dt_fuzzy_left_join_chunk <- function(x, y, by = NULL, match_fun, chunk_size = 100000L, ...) {
  dt_fuzzy_join_chunk(x, y, by = by, match_fun = match_fun, mode = "left", chunk_size = chunk_size, ...)
}

#' Fuzzy right join (chunked binding)
#'
#' Convenience wrapper for \code{dt_fuzzy_join_chunk(mode = "right")}.
#'
#' @inheritParams dt_fuzzy_join_chunk
#' @return See \code{\link{dt_fuzzy_join_chunk}}.
#' @export
dt_fuzzy_right_join_chunk <- function(x, y, by = NULL, match_fun, chunk_size = 100000L, ...) {
  dt_fuzzy_join_chunk(x, y, by = by, match_fun = match_fun, mode = "right", chunk_size = chunk_size, ...)
}

#' Fuzzy full join (chunked binding)
#'
#' Convenience wrapper for \code{dt_fuzzy_join_chunk(mode = "full")}.
#'
#' @inheritParams dt_fuzzy_join_chunk
#' @return See \code{\link{dt_fuzzy_join_chunk}}.
#' @export
dt_fuzzy_full_join_chunk <- function(x, y, by = NULL, match_fun, chunk_size = 100000L, ...) {
  dt_fuzzy_join_chunk(x, y, by = by, match_fun = match_fun, mode = "full", chunk_size = chunk_size, ...)
}

#' Fuzzy semi join (chunked binding)
#'
#' Convenience wrapper for \code{dt_fuzzy_join_chunk(mode = "semi")}.
#'
#' @inheritParams dt_fuzzy_join_chunk
#' @return See \code{\link{dt_fuzzy_join_chunk}}.
#' @export
dt_fuzzy_semi_join_chunk <- function(x, y, by = NULL, match_fun, chunk_size = 100000L, ...) {
  dt_fuzzy_join_chunk(x, y, by = by, match_fun = match_fun, mode = "semi", chunk_size = chunk_size, ...)
}

#' Fuzzy anti join (chunked binding)
#'
#' Convenience wrapper for \code{dt_fuzzy_join_chunk(mode = "anti")}.
#'
#' @inheritParams dt_fuzzy_join_chunk
#' @return See \code{\link{dt_fuzzy_join_chunk}}.
#' @export
dt_fuzzy_anti_join_chunk <- function(x, y, by = NULL, match_fun, chunk_size = 100000L, ...) {
  dt_fuzzy_join_chunk(x, y, by = by, match_fun = match_fun, mode = "anti", chunk_size = chunk_size, ...)
}
