test_that("multi-column joins report the maximum component distance", {
  x <- data.frame(
    first = c("John", "Bob"),
    last = c("Smith", "Jones")
  )
  y <- data.frame(
    fname = c("Jon", "Bob"),
    lname = c("Smit", "Jones")
  )

  result <- fuzzystring_inner_join(
    x,
    y,
    by = c(first = "fname", last = "lname"),
    max_dist = 1,
    distance_col = "distance"
  )

  expect_named(result, c("first", "last", "fname", "lname", "distance"))
  expect_equal(result$distance, c(1, 0))
})

test_that("distance column cannot corrupt input or internal columns", {
  x <- data.frame(value = "foo")
  y <- data.frame(approx = "foo")

  expect_snapshot(
    error = TRUE,
    fuzzystring_inner_join(
      x,
      y,
      by = c(value = "approx"),
      distance_col = "x"
    )
  )
  expect_snapshot(
    error = TRUE,
    fuzzystring_inner_join(
      x,
      y,
      by = c(value = "approx"),
      distance_col = "value"
    )
  )
})

test_that("public arguments are validated before join planning", {
  x <- data.frame(value = "foo")
  y <- data.frame(approx = "foo")

  expect_snapshot(
    error = TRUE,
    fuzzystring_join(x, y, by = c(value = "approx"), max_dist = NA_real_)
  )
  expect_snapshot(
    error = TRUE,
    fuzzystring_join(x, y, by = c(value = "approx"), max_dist = Inf)
  )
  expect_snapshot(
    error = TRUE,
    fuzzystring_join(x, y, by = c(value = "approx"), ignore_case = "yes")
  )
  expect_snapshot(
    error = TRUE,
    fuzzystring_join(x, y, by = character())
  )
  expect_snapshot(
    error = TRUE,
    fuzzystring_join(x, y, by = c(missing = "approx"))
  )
})

test_that("Cartesian expansion rejects unsupported result sizes", {
  expect_snapshot(
    error = TRUE,
    fst_checked_cartesian_sizes(.Machine$integer.max, 2L)
  )
})

test_that("optimized single-column matching agrees with direct distances", {
  x_values <- c("alpha", "alfa", "beta", NA_character_, "alpha")
  y_values <- c("alpah", "beta", "delta", NA_character_)

  for (method in c("osa", "lv", "dl", "hamming", "lcs", "jw")) {
    actual <- fst_stringdist_single_col_matches(
      x_values,
      y_values,
      max_dist = 2,
      method = method,
      distance_col = "distance"
    )
    grid <- expand.grid(
      x = seq_along(x_values),
      y = seq_along(y_values)
    )
    grid$distance <- stringdist::stringdist(
      x_values[grid$x],
      y_values[grid$y],
      method = method
    )
    expected <- grid[!is.na(grid$distance) & grid$distance <= 2, ]
    attr(expected, "out.attrs") <- NULL

    actual <- actual[order(actual$x, actual$y), ]
    expected <- expected[order(expected$x, expected$y), ]
    rownames(expected) <- NULL
    expect_equal(as.data.frame(actual), expected, info = method)
  }
})
