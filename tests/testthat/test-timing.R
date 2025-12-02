test_that("cs_run_single records timing metadata", {
  res <- cs_run_single(
    dgp_id       = "synth_baseline",
    estimator_id = "lm_att",
    n            = 20,
    seed         = 1,
    bootstrap    = FALSE
  )
  row <- cs_tidy_run(res)
  expect_true(is.numeric(row$run_time_total))
  expect_true(row$run_time_total > 0)
  expect_true(row$run_time_dgp >= 0)
  expect_true(row$run_time_est >= 0)
})
