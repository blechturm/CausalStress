test_that("synth_nonlinear_heteroskedastic satisfies synthetic DGP contract", {
  dgp <- dgp_synth_nonlinear_heteroskedastic(n = 500, seed = 123)
  expect_invisible(cs_check_dgp_synthetic(dgp))
})

test_that("synth_nonlinear_heteroskedastic is reproducible for same seed", {
  d1 <- dgp_synth_nonlinear_heteroskedastic(n = 500, seed = 42)
  d2 <- dgp_synth_nonlinear_heteroskedastic(n = 500, seed = 42)

  expect_identical(d1$df, d2$df)
  expect_identical(d1$true_att, d2$true_att)
  expect_identical(d1$true_qst, d2$true_qst)
  expect_identical(d1$meta$structural_te, d2$meta$structural_te)
})

test_that("synth_nonlinear_heteroskedastic exhibits increasing noise with |X3|", {
  dgp <- dgp_synth_nonlinear_heteroskedastic(n = 5000, seed = 1)
  df  <- dgp$df

  mu0_hat <- 1 + 0.8 * sin(df$X1) + 0.5 * df$X2^2 - 0.3 * df$X4
  res     <- df$y0 - mu0_hat

  high  <- abs(df$X3) > 1
  low   <- abs(df$X3) <= 0.5

  var_high <- stats::var(res[high])
  var_low  <- stats::var(res[low])

  expect_gt(var_high, var_low)
})

test_that("synth_nonlinear_heteroskedastic is registered", {
  reg <- cs_dgp_registry()
  row <- reg[reg$dgp_id == "synth_nonlinear_heteroskedastic", , drop = FALSE]
  expect_equal(nrow(row), 1L)
  expect_equal(row$type, "synthetic")
  expect_true(nchar(row$description) > 0)
})
