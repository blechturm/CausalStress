test_that("gengc_dr estimator returns contract-compliant output", {
  skip_if_not_installed("GenGC")

  dgp <- dgp_synth_baseline(n = 120, seed = 123)

  expect_no_error({
    res <- est_gengc_dr(
      dgp$df,
      tau = cs_tau_oracle,
      config = list(n_draws = 50, num_trees = 50, crossfit_folds = 3, num_threads = 1)
    )
    cs_check_estimator_output(res, require_qst = TRUE, tau = cs_tau_oracle)
  })
})

test_that("gengc_dr estimator strips oracle columns (airlock)", {
  skip_if_not_installed("GenGC")

  dgp <- dgp_synth_baseline(n = 120, seed = 321)
  df_bad <- dgp$df
  df_bad$structural_te <- 0  # forbidden column should be dropped internally

  expect_no_error(
    est_gengc_dr(
      df_bad,
      tau = cs_tau_oracle,
      config = list(n_draws = 10, num_trees = 20, crossfit_folds = 2, num_threads = 1)
    )
  )
})

