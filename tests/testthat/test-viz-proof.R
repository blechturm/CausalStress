test_that("GenGC recovers S-curve on synth_qte1", {
  skip_if_not_installed("GenGC")

  # run GenGC on sign-flip DGP
  dgp <- dgp_synth_qte1(n = 1000, seed = 42)
  res <- est_gengc(dgp$df, tau = cs_tau_oracle, config = list(num_trees = 200, n_draws = 100))

  # rename to estimate for convenience
  qst_est <- dplyr::rename(res$qst, estimate = value)

  # numeric sign check: low quantiles negative, high quantiles positive
  low_idx <- which.min(abs(qst_est$tau - 0.1))
  high_idx <- which.min(abs(qst_est$tau - 0.9))
  low_tau <- qst_est$estimate[low_idx]
  high_tau <- qst_est$estimate[high_idx]
  expect_true(low_tau < high_tau)
  expect_true(high_tau > 0)

  # plotting
  plot_obj <- cs_plot_qst(
    tibble::tibble(
      dgp_id = "synth_qte1",
      estimator_id = "gengc",
      tau = qst_est$tau,
      estimate = qst_est$estimate,
      true = dgp$true_qst$value
    )
  )
  expect_s3_class(plot_obj, "ggplot")
})
