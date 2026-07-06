# distance column cannot corrupt input or internal columns

    Code
      fuzzystring_inner_join(x, y, by = c(value = "approx"), distance_col = "x")
    Condition
      Error:
      ! `distance_col` must not collide with input or internal columns.

---

    Code
      fuzzystring_inner_join(x, y, by = c(value = "approx"), distance_col = "value")
    Condition
      Error:
      ! `distance_col` must not collide with input or internal columns.

# public arguments are validated before join planning

    Code
      fuzzystring_join(x, y, by = c(value = "approx"), max_dist = NA_real_)
    Condition
      Error:
      ! `max_dist` must be a single finite non-negative number.

---

    Code
      fuzzystring_join(x, y, by = c(value = "approx"), max_dist = Inf)
    Condition
      Error:
      ! `max_dist` must be a single finite non-negative number.

---

    Code
      fuzzystring_join(x, y, by = c(value = "approx"), ignore_case = "yes")
    Condition
      Error:
      ! `ignore_case` must be `TRUE` or `FALSE`.

---

    Code
      fuzzystring_join(x, y, by = character())
    Condition
      Error:
      ! `by` must specify at least one valid pair of columns.

---

    Code
      fuzzystring_join(x, y, by = c(missing = "approx"))
    Condition
      Error:
      ! Join columns not found: x$missing.

# Cartesian expansion rejects unsupported result sizes

    Code
      fst_checked_cartesian_sizes(.Machine$integer.max, 2L)
    Condition
      Error:
      ! The join result is too large to represent safely.

