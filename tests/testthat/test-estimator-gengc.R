test_that("gengc estimator returns contract-compliant output", {
  skip_if_not_installed("GenGC")

  dgp <- dgp_synth_baseline(n = 120, seed = 123)

  expect_no_error({
    res <- est_gengc(
      dgp$df,
      tau = cs_tau_oracle,
      config = list(n_draws = 50, num_trees = 50, seed = 1)
    )
    cs_check_estimator_output(res, require_qst = TRUE, tau = cs_tau_oracle)
  })
})

test_that("gengc estimator strips oracle columns (airlock)", {
  skip_if_not_installed("GenGC")

  dgp <- dgp_synth_baseline(n = 120, seed = 321)
  df_bad <- dgp$df
  df_bad$structural_te <- 0  # forbidden column should be dropped internally

  expect_no_error(
    est_gengc(
      df_bad,
      tau = cs_tau_oracle,
      config = list(n_draws = 10, num_trees = 20, seed = 2)
    )
  )
})
