skip_on_cran()

# Strict RNG locking should make serial vs parallel identical

test_that("parallel seeds match serial seeds under strict RNG", {
  # ensure future is available
  skip_if_not_installed("future")
  seeds <- 1:2

  future::plan(sequential)
  on.exit(future::plan(sequential), add = TRUE)

  res_serial <- cs_run_seeds(
    dgp_id        = "synth_baseline",
    estimator_id  = "lm_att",
    n             = 100,
    seeds         = seeds,
    bootstrap     = FALSE,
    parallel      = FALSE,
    show_progress = FALSE
  )

  future::plan(multisession, workers = 2)
  res_parallel <- cs_run_seeds(
    dgp_id        = "synth_baseline",
    estimator_id  = "lm_att",
    n             = 100,
    seeds         = seeds,
    bootstrap     = FALSE,
    parallel      = TRUE,
    show_progress = FALSE
  )

  expect_identical(res_serial$est_att, res_parallel$est_att)
})
