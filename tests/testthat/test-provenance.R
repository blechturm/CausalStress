test_that("cs_run_single exposes estimator_reported_version", {
  res <- cs_run_single(
    dgp_id = "synth_baseline",
    estimator_id = "lm_att",
    n = 50,
    seed = 1,
    bootstrap = FALSE
  )

  expect_true("estimator_reported_version" %in% names(res$meta))
  expect_true(is.na(res$meta$estimator_reported_version) || is.character(res$meta$estimator_reported_version))
})
