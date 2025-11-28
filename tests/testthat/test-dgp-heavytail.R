test_that("dgp_synth_heavytail returns a valid synthetic DGP object", {
  dgp <- dgp_synth_heavytail(n = 500, seed = 123L)

  expect_invisible(cs_check_dgp_synthetic(dgp))

  expect_true(is.list(dgp))
  expect_true(is.data.frame(dgp$df))
  expect_true(is.numeric(dgp$true_att))
  expect_true(tibble::is_tibble(dgp$true_qst))

  expect_equal(dgp$meta$dgp_id, "synth_heavytail")
  expect_equal(dgp$meta$type, "synthetic")

  required_cols <- c("y", "w", "y0", "y1", "p", "structural_te", "X1", "X2", "X3", "X4", "X5")
  expect_true(all(required_cols %in% names(dgp$df)))
})

test_that("dgp_synth_heavytail has heavier tails than baseline on large n", {
  n <- 20000

  dgp_base <- dgp_synth_baseline(n = n, seed = 1L)
  dgp_ht   <- dgp_synth_heavytail(n = n, seed = 1L)

  y_base <- dgp_base$df$y
  y_ht   <- dgp_ht$df$y

  max_abs_base <- max(abs(y_base))
  max_abs_ht   <- max(abs(y_ht))

  expect_gt(max_abs_ht, max_abs_base)
})

test_that("heavytail true_qst is oracle-based and invariant to seed", {
  d1 <- dgp_synth_heavytail(n = 100, seed = 1L)$true_qst
  d2 <- dgp_synth_heavytail(n = 150, seed = 99L)$true_qst

  expect_equal(d1$value, d2$value, tolerance = 1e-6)
})
