test_that("stringdist_join / variants (data.table backend)", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("stringdist")
  skip_if_not_installed("data.table")

  # Use ggplot2::diamonds (available when ggplot2 is installed)
  diamonds <- ggplot2::diamonds

  # ------------------------------------------------------------------
  # Setup helpers
  # ------------------------------------------------------------------
  d <- data.table::data.table(
    cut2 = c("Idea", "Premiums", "Premiom", "VeryGood", "VeryGood", "Faiir"),
    type = 1:6
  )

  d2 <- d[1:3]
  included <- c("Ideal", "Premium")
  notin <- c("Fair", "Good", "Very Good")

  d3 <- data.table::rbindlist(list(d2, data.table::data.table(cut2 = "NewType", type = 4L)),
                              use.names = TRUE, fill = TRUE)

  # ------------------------------------------------------------------
  # INNER join (multiples + distance column)
  # ------------------------------------------------------------------
  j <- fuzzystring_inner_join(diamonds, d,
                                by = c(cut = "cut2"),
                                distance_col = "distance")

  # count(cut, cut2) + arrange(cut)
  result <- as.data.table(j)[, .N, by = .(cut, cut2)]
  data.table::setorder(result, cut)

  expect_equal(as.character(result$cut),
               c("Fair", "Very Good", "Premium", "Premium", "Ideal"))
  expect_equal(result$cut2,
               c("Faiir", "VeryGood", "Premiums", "Premiom", "Idea"))

  # Premium and Very Good each match two entries in d (Premiom/Premiums, VeryGood twice)
  expect_equal(sum(j$cut == "Premium"), sum(diamonds$cut == "Premium") * 2L)
  expect_equal(sum(j$cut == "Very Good"), sum(diamonds$cut == "Very Good") * 2L)
  expect_equal(sum(j$cut2 == "Premiom"), sum(diamonds$cut == "Premium"))

  # With default max_dist=2, here all accepted should be distance 1
  expect_true(all(j$distance == 1))

  vg <- as.data.table(j)[cut == "Very Good", .N, by = type]
  data.table::setorder(vg, type)

  expect_equal(vg$type, c(4L, 5L))
  expect_equal(vg$N, rep.int(sum(diamonds$cut == "Very Good"), 2L))

  expect_true(all(j$type[j$cut2 == "Faiir"] == 6L))

  # ------------------------------------------------------------------
  # LEFT join
  # ------------------------------------------------------------------
  result_l <- fuzzystring_left_join(diamonds, d2, by = c(cut = "cut2"))

  expect_true(all(is.na(result_l$cut2[result_l$cut %in% notin])))
  expect_equal(sum(result_l$cut %in% notin), sum(diamonds$cut %in% notin))

  expect_equal(sum(result_l$cut2 == "Premiom", na.rm = TRUE),
               sum(diamonds$cut == "Premium"))
  expect_equal(sum(result_l$cut2 == "Premiom", na.rm = TRUE),
               sum(result_l$cut2 == "Premiums", na.rm = TRUE))

  # ------------------------------------------------------------------
  # RIGHT join
  # ------------------------------------------------------------------
  result_r <- fuzzystring_right_join(diamonds, d3, by = c(cut = "cut2"))

  expect_equal(sum(result_r$cut2 == "NewType"), 1L)
  expect_equal(sum(is.na(result_r$cut)), 1L)
  expect_true(all(is.na(result_r$cut[result_r$cut2 == "NewType"])))

  expect_equal(sum(result_r$cut2 == "Premiom", na.rm = TRUE),
               sum(diamonds$cut == "Premium"))
  expect_equal(sum(result_r$cut2 == "Premiom", na.rm = TRUE),
               sum(result_r$cut2 == "Premiums", na.rm = TRUE))

  # ------------------------------------------------------------------
  # FULL join
  # ------------------------------------------------------------------
  result_f <- fuzzystring_full_join(diamonds, d3, by = c(cut = "cut2"))

  expect_equal(sum(result_f$cut2 == "NewType", na.rm = TRUE), 1L)
  expect_equal(sum(is.na(result_f$cut)), 1L)
  expect_true(all(is.na(result_f$cut[result_f$cut2 == "NewType"])))

  expect_true(all(is.na(result_f$cut2[result_f$cut %in% notin])))
  expect_equal(sum(result_f$cut %in% notin), sum(diamonds$cut %in% notin))

  expect_equal(sum(result_f$cut2 == "Premiom", na.rm = TRUE),
               sum(diamonds$cut == "Premium"))
  expect_equal(sum(result_f$cut2 == "Premiom", na.rm = TRUE),
               sum(result_f$cut2 == "Premiums", na.rm = TRUE))

  # ------------------------------------------------------------------
  # SEMI join
  # ------------------------------------------------------------------
  result_s <- fuzzystring_semi_join(diamonds, d2, by = c(cut = "cut2"))

  expect_equal(sort(as.character(unique(result_s$cut))), included)
  expect_equal(nrow(result_s), sum(result_s$cut %in% included))
  expect_false("cut2" %in% names(result_s))

  # ------------------------------------------------------------------
  # ANTI join
  # ------------------------------------------------------------------
  result_a <- fuzzystring_anti_join(diamonds, d2, by = c(cut = "cut2"))

  expect_equal(sort(as.character(unique(result_a$cut))), notin)
  expect_equal(nrow(result_a), sum(result_a$cut %in% notin))

  # ------------------------------------------------------------------
  # Multiple match functions via fuzzy_inner_join (data.table backend)
  # ------------------------------------------------------------------
#  d_multi <- data.table::data.table(
#    cut2   = c("Idea", "Premiums", "Premiom", "VeryGood", "VeryGood", "Faiir"),
#    carat2 = c(0, 0.5, 1, 1.5, 2, 2.5),
#    type   = 1:6
#  )
#
#  sdist <- function(s1, s2) stringdist::stringdist(s1, s2) <= 1
#  ndist <- function(n1, n2) abs(n1 - n2) < 0.25
#
#  j_multi <- fstring_inner_join(
#    diamonds, d_multi,
#    by = c(cut = "cut2", carat = "carat2"),
#    match_fun = list(sdist, ndist)
#  )
#
#  result_multi <- as.data.table(j_multi)[, .N, by = .(cut, cut2)]
#  data.table::setorder(result_multi, cut)
#
#  expect_equal(as.character(result_multi$cut),
#               c("Fair", "Very Good", "Premium", "Premium", "Ideal"))
#  expect_equal(result_multi$cut2,
#               c("Faiir", "VeryGood", "Premiums", "Premiom", "Idea"))
#
#  expect_lt(max(abs(j_multi$carat - j_multi$carat2)), 0.25)
#
#  # named list (by x column names)
#  j_multi_named <- fstring_inner_join(
#    diamonds, d_multi,
#    by = c(cut = "cut2", carat = "carat2"),
#    match_fun = list(carat = ndist, cut = sdist)
#  )
#  expect_equal(j_multi, j_multi_named)
#
  # ------------------------------------------------------------------
  # No matches cases (including overlapping column names)
  # ------------------------------------------------------------------
  d_nomatch <- data.table::data.table(
    cut2 = c("Ideolll", "Premiumsss", "Premiomzzz", "VeryVeryGood", "VeryVeryGood", "FaiirsFair"),
    type = 1:6
  )

  j1 <- fuzzystring_inner_join(diamonds, d_nomatch, by = c(cut = "cut2"))
  expect_equal(nrow(j1), 0L)
  expect_true(all(c("carat", "cut", "cut2", "type") %in% names(j1)))

  d_nomatch2 <- data.table::copy(d_nomatch)
  data.table::setnames(d_nomatch2, "cut2", "cut")
  j1_5 <- fuzzystring_inner_join(diamonds, d_nomatch2, by = c(cut = "cut"))
  expect_equal(nrow(j1_5), 0L)
  expect_true(all(c("carat", "cut.x", "cut.y", "type") %in% names(j1_5)))

  j2 <- fuzzystring_left_join(diamonds, d_nomatch, by = c(cut = "cut2"))
  expect_equal(nrow(j2), nrow(diamonds))
  expect_true(all(is.na(j2$cut2)))

  j3 <- fuzzystring_right_join(diamonds, d_nomatch, by = c(cut = "cut2"))
  expect_equal(nrow(j3), nrow(d_nomatch))
  expect_true(all(is.na(j3$carat)))
  expect_true(all(is.na(j3$cut)))

  j4 <- fuzzystring_full_join(diamonds, d_nomatch, by = c(cut = "cut2"))
  expect_equal(nrow(j4), nrow(diamonds) + nrow(d_nomatch))
  expect_true(all(is.na(j4$cut) | is.na(j4$cut2)))
  expect_true(all(is.na(j4$carat) | is.na(j4$type)))

  j5 <- fuzzystring_semi_join(diamonds, d_nomatch, by = c(cut = "cut2"))
  expect_equal(nrow(j5), 0L)
  expect_true("cut" %in% names(diamonds))
  expect_true("carat" %in% names(diamonds))
  expect_false("cut2" %in% names(diamonds))
  expect_false("type" %in% names(diamonds))

  j6 <- fuzzystring_anti_join(diamonds, d_nomatch, by = c(cut = "cut2"))
  expect_equal(nrow(j6), nrow(diamonds))
  expect_true("cut" %in% names(diamonds))
  expect_false("cut2" %in% names(diamonds))

  # ------------------------------------------------------------------
  # ignore_case
  # ------------------------------------------------------------------
  d_lowercase <- data.table::copy(d)
  d_lowercase[, cut2 := tolower(cut2)]

  j_ic_0 <- fuzzystring_inner_join(
    diamonds, d_lowercase,
    by = c(cut = "cut2"),
    distance_col = "distance",
    max_dist = 1
  )
  expect_equal(nrow(j_ic_0), 0L)

  j_ic <- fuzzystring_inner_join(
    diamonds, d_lowercase,
    by = c(cut = "cut2"),
    distance_col = "distance",
    ignore_case = TRUE,
    max_dist = 1
  )
  expect_gt(nrow(j_ic), 0L)
  expect_equal(sum(j_ic$cut == "Premium"), sum(diamonds$cut == "Premium") * 2L)
  expect_true(all(j_ic$distance[j_ic$cut2 == "idea"] == 1))

  # ------------------------------------------------------------------
  # soundex
  # ------------------------------------------------------------------
  j_sdx <- fuzzystring_inner_join(
    diamonds, d,
    by = c(cut = "cut2"),
    distance_col = "distance",
    method = "soundex"
  )
  expect_gt(nrow(j_sdx), 0L)
  expect_equal(sum(j_sdx$cut == "Premium"), sum(diamonds$cut == "Premium") * 2L)

  # ------------------------------------------------------------------
  # Renaming overlapping columns (.x / .y)
  # ------------------------------------------------------------------
  d_overlap <- data.table::data.table(
    cut = c("Idea", "Premiums", "Premiom", "VeryGood", "VeryGood", "Faiir"),
    price = 1:6
  )

  j_ov <- fuzzystring_inner_join(diamonds, d_overlap, by = "cut")

  expect_true("cut.x" %in% names(j_ov))
  expect_true("price.x" %in% names(j_ov))
  expect_true("cut.y" %in% names(j_ov))
  expect_true("price.y" %in% names(j_ov))
  expect_true(all(j_ov$cut.y %in% d_overlap$cut))
  expect_true(all(j_ov$price.y %in% d_overlap$price))

  # ------------------------------------------------------------------
  # Return type: data.frame in -> data.frame out
  # ------------------------------------------------------------------
  result_df <- fuzzystring_inner_join(as.data.frame(diamonds), d, by = c(cut = "cut2"))
  expect_s3_class(result_df, "data.frame")
  expect_false(inherits(result_df, "tbl_df"))

  result_df2 <- fuzzystring_inner_join(as.data.frame(diamonds), as.data.frame(d), by = c(cut = "cut2"))
  expect_s3_class(result_df2, "data.frame")
  expect_false(inherits(result_df2, "tbl_df"))

  # ------------------------------------------------------------------
  # One-column data.frames (regression)
  # ------------------------------------------------------------------
  onecol <- data.frame(cut2 = c("Idea", "Premiums", "Premiom", "VeryGood", "VeryGood", "Faiir"))
  diamonds_df <- as.data.frame(diamonds)
  res_onecol <- fuzzystring_inner_join(diamonds_df, onecol, by = c(cut = "cut2"))
  expect_s3_class(res_onecol, "data.frame")
  expect_true("cut2" %in% names(res_onecol))
  expect_gt(nrow(res_onecol), 0L)

  # ------------------------------------------------------------------
  # No common variables should error
  # ------------------------------------------------------------------
  expect_error(fuzzystring_inner_join(diamonds, d),
               "No common variables")

  # ------------------------------------------------------------------
  # Distance column existence when no overlaps (outer joins vs semi/anti)
  # ------------------------------------------------------------------
  a <- data.table::data.table(x = c("apple", "banana"))
  b <- data.table::data.table(y = c("orange", "mango"))

  res_left <- fuzzystring_left_join(a, b, by = c(x = "y"), max_dist = 1, distance_col = "distance")
  expect_equal(names(res_left), c("x", "y", "distance"))
  expect_equal(nrow(res_left), 2L)
  expect_true(all(is.na(res_left$y)))
  expect_true(all(is.na(res_left$distance)))

  res_inner <- fuzzystring_inner_join(a, b, by = c(x = "y"), max_dist = 1, distance_col = "distance")
  expect_equal(names(res_inner), c("x", "y", "distance"))
  expect_equal(nrow(res_inner), 0L)

  res_semi <- fuzzystring_semi_join(a, b, by = c(x = "y"), max_dist = 1, distance_col = "distance")
  expect_equal(names(res_semi), "x")
  expect_equal(nrow(res_semi), 0L)

  res_anti <- fuzzystring_anti_join(a, b, by = c(x = "y"), max_dist = 1, distance_col = "distance")
  expect_equal(as.data.frame(a), as.data.frame(res_anti))
})
