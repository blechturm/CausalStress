test_that("bootstrap=FALSE forces ci_method=none", {
  res <- cs_run_single(
    dgp_id = "synth_baseline",
    estimator_id = "lm_att",
    n = 50,
    seed = 1,
    bootstrap = FALSE,
    B = 200,
    config = list()
  )

  expect_identical(res$meta$ci_method, "none")
  expect_true(is.null(res$meta$n_boot_ok) || identical(res$meta$n_boot_ok, 0L))
})
