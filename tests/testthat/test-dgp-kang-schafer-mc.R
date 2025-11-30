test_that("synth_placebo_kangschafer yields small bias for core estimators in small MC", {
  runs <- cs_run_grid(
    dgp_ids       = "synth_placebo_kangschafer",
    estimator_ids = c("lm_att", "ipw_att"),
    n             = 300,
    seeds         = 1:3,
    bootstrap     = FALSE
  )

  summ <- cs_summarise_runs(runs)
  expect_true(all(abs(summ$mean_true_att) < 1e-6))
  expect_true(all(is.finite(summ$mean_error)))
})
