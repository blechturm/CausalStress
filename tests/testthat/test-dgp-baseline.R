test_that("dgp_synth_baseline returns a valid synthetic DGP", {
  n <- 200L
  dgp <- dgp_synth_baseline(n = n, seed = 123)

  # basic structure
  expect_type(dgp, "list")
  expect_named(dgp, c("df", "true_att", "true_qst", "meta"))

  # df checks
  df <- dgp$df
  expect_s3_class(df, "tbl_df")
  expect_equal(nrow(df), n)

  required_cols <- c("y", "w", "y0", "y1", "p",
                     "structural_te", "X1", "X2", "X3", "X4", "X5")
  expect_true(all(required_cols %in% names(df)))

  # no NA in core columns
  expect_false(any(is.na(df$y)))
  expect_false(any(is.na(df$w)))
  expect_false(any(is.na(df$y0)))
  expect_false(any(is.na(df$y1)))
  expect_false(any(is.na(df$structural_te)))
  expect_true(all(df$w %in% c(0, 1)))

  # meta checks
  meta <- dgp$meta
  expect_true(is.list(meta))
  expect_equal(meta$dgp_id, "synth_baseline")
  expect_equal(meta$type, "synthetic")
  expect_true(is.numeric(meta$structural_te))
  expect_equal(length(meta$structural_te), n)

  # structural_te consistency between meta and df
  expect_equal(meta$structural_te, df$structural_te)

  # truth objects
  expect_true(is.numeric(dgp$true_att))
  expect_length(dgp$true_att, 1L)

  expect_true(tibble::is_tibble(dgp$true_qst))
  expect_true(all(c("tau", "value") %in% names(dgp$true_qst)))

  # contract helper should accept this DGP (with current contract)
  expect_invisible(cs_check_dgp_synthetic(dgp))
})

test_that("baseline true_qst is oracle-based and invariant to seed", {
  d1 <- dgp_synth_baseline(n = 100, seed = 1L)$true_qst
  d2 <- dgp_synth_baseline(n = 150, seed = 99L)$true_qst

  expect_equal(d1$value, d2$value, tolerance = 1e-6)
})
