skip_on_cran()

# Strict RNG locking should make serial vs parallel identical

test_that("parallel seeds match serial seeds under strict RNG", {
  # ensure future is available
  skip_if_not_installed("future")
  skip_if_not_installed("furrr")
  seeds <- 1:2

  old_plan <- future::plan()
  on.exit(future::plan(old_plan), add = TRUE)
  # Note: sequential plan only exercises the experimental-parallel code path;
  # it does not create true multiprocessing workers.
  future::plan(future::sequential)

  res_serial <- cs_run_seeds(
    dgp_id        = "synth_baseline",
    estimator_id  = "lm_att",
    n             = 100,
    seeds         = seeds,
    bootstrap     = FALSE,
    parallel      = FALSE,
    show_progress = FALSE
  )

  expect_warning(
    res_parallel <- cs_run_seeds(
      dgp_id        = "synth_baseline",
      estimator_id  = "lm_att",
      n             = 100,
      seeds         = seeds,
      bootstrap     = FALSE,
      parallel      = TRUE,
      experimental_parallel = TRUE,
      show_progress = FALSE
    ),
    class = "causalstress_experimental_parallel"
  )

  expect_identical(res_serial$est_att, res_parallel$est_att)
})
