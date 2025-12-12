test_that("cs_run_campaign runs multiple DGPs in parallel", {
  skip_if_not_installed("future")
  skip_if_not_installed("furrr")

  future::plan(future::multisession, workers = 2)
  withr::defer(future::plan(future::sequential))

  res <- cs_run_campaign(
    dgp_ids       = c("synth_baseline", "synth_placebo_tau0"),
    estimator_ids = "lm_att",
    seeds         = 1:2,
    n             = 30,
    parallel      = TRUE,
    show_progress = FALSE
  )

  expect_s3_class(res, "tbl_df")
  expect_equal(nrow(res), 4)
  expect_true(all(res$success))
})
