test_that("nonlinear_heteroskedastic_v140 has extreme heteroskedasticity", {
  dgp <- dgp_synth_nonlinear_heteroskedastic_v140(n = 1000, seed = 1, include_truth = FALSE)
  df <- dgp$df

  mu0 <- 1 + 0.5 * df$X1^3 + 1.5 * df$X2^2 - 1.0 * df$X4
  res <- df$y0 - mu0

  low <- df$X2 < -2.5
  high <- df$X2 > 2.5

  expect_gt(sum(low), 0)
  expect_gt(sum(high), 0)
  expect_gt(stats::sd(res[high]), 2.5)
  expect_lt(stats::sd(res[low]), 0.7)
  expect_gt(stats::sd(res[high]), 3 * stats::sd(res[low]))
})

test_that("overlap_stressed_v140 has extreme propensities", {
  dgp <- dgp_synth_overlap_stressed_v140(n = 1000, seed = 1, include_truth = FALSE)
  p <- dgp$df$p

  share_outside <- mean(p < 0.05 | p > 0.95)
  expect_gt(share_outside, 0.80)
})

test_that("hd_sparse_plm_v140 has high collinearity", {
  dgp <- dgp_synth_hd_sparse_plm_v140(n = 1000, seed = 1, include_truth = FALSE)
  df <- dgp$df

  expect_gt(stats::cor(df$X1, df$X2), 0.90)
})
