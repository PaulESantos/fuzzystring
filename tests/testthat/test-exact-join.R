test_that("exact route preserves duplicate and missing-value semantics", {
  x <- data.frame(name = c("Alpha", "beta", "Alpha", NA_character_))
  y <- data.frame(approx = c("Alpha", "gamma", "Alpha", NA_character_))

  for (method in c("osa", "lv", "dl", "hamming", "lcs", "jw")) {
    inner <- fuzzystring_inner_join(
      x,
      y,
      by = c(name = "approx"),
      method = method,
      max_dist = 0,
      distance_col = "distance"
    )

    expect_equal(nrow(inner), 4L, info = method)
    expect_equal(inner$distance, rep(0, 4L), info = method)
  }

  expect_equal(
    nrow(fuzzystring_left_join(x, y, by = c(name = "approx"), max_dist = 0)),
    6L
  )
  expect_equal(
    nrow(fuzzystring_right_join(x, y, by = c(name = "approx"), max_dist = 0)),
    6L
  )
  expect_equal(
    nrow(fuzzystring_full_join(x, y, by = c(name = "approx"), max_dist = 0)),
    8L
  )
  expect_equal(
    fuzzystring_semi_join(x, y, by = c(name = "approx"), max_dist = 0)$name,
    c("Alpha", "Alpha")
  )
  expect_equal(
    fuzzystring_anti_join(x, y, by = c(name = "approx"), max_dist = 0)$name,
    c("beta", NA_character_)
  )
})

test_that("exact route supports case folding and multiple columns", {
  x <- data.frame(
    first = c("Alice", "Bob", "ALICE"),
    last = c("Smith", "Jones", "Smith")
  )
  y <- data.frame(
    given = c("alice", "bob"),
    family = c("smith", "stone")
  )

  result <- fuzzystring_inner_join(
    x,
    y,
    by = c(first = "given", last = "family"),
    max_dist = 0,
    ignore_case = TRUE,
    distance_col = "distance"
  )

  expect_equal(result$first, c("Alice", "ALICE"))
  expect_equal(result$given, c("alice", "alice"))
  expect_equal(result$distance, c(0, 0))
})

test_that("exact route remains conservative when metric semantics may differ", {
  expect_identical(fst_can_use_exact_join("osa", 0, list()), TRUE)
  expect_identical(
    fst_can_use_exact_join("osa", 0, list(weight = rep(1, 4))),
    FALSE
  )
  expect_identical(fst_can_use_exact_join("qgram", 0, list()), FALSE)
  expect_identical(fst_can_use_exact_join("soundex", 0, list()), FALSE)
})
