skip_if_not_installed("future")
skip_if_not_installed("furrr")

test_that("cs_run_seeds parallel reproducibility matches serial", {
  old_plan <- future::plan()
  on.exit(future::plan(old_plan), add = TRUE)
  # Note: sequential plan only exercises the experimental-parallel code path;
  # it does not create true multiprocessing workers.
  future::plan(future::sequential)

  serial <- cs_run_seeds(
    dgp_id       = "synth_baseline",
    estimator_id = "lm_att",
    n            = 50,
    seeds        = 1:4,
    bootstrap    = FALSE,
    B            = 0,
    show_progress = FALSE,
    parallel      = FALSE
  )

  expect_warning(
    parallel_res <- cs_run_seeds(
      dgp_id       = "synth_baseline",
      estimator_id = "lm_att",
      n            = 50,
      seeds        = 1:4,
      bootstrap    = FALSE,
      B            = 0,
      show_progress = FALSE,
      parallel      = TRUE,
      experimental_parallel = TRUE
    ),
    class = "causalstress_experimental_parallel"
  )

  expect_equal(serial$est_att, parallel_res$est_att)
})
