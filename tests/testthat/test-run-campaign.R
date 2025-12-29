test_that("cs_run_campaign runs multiple DGPs in parallel", {
  skip_if_not_installed("future")
  skip_if_not_installed("furrr")

  old_plan <- future::plan()
  on.exit(future::plan(old_plan), add = TRUE)
  # Note: sequential plan only exercises the experimental-parallel code path;
  # it does not create true multiprocessing workers.
  future::plan(future::sequential)

  expect_warning(
    res <- cs_run_campaign(
      dgp_ids       = c("synth_baseline", "synth_placebo_tau0"),
      estimator_ids = "lm_att",
      seeds         = 1:2,
      n             = 30,
      parallel      = TRUE,
      experimental_parallel = TRUE,
      show_progress = FALSE
    ),
    class = "causalstress_experimental_parallel"
  )

  expect_s3_class(res, "tbl_df")
  expect_equal(nrow(res), 4)
  expect_true(all(res$success))
})
