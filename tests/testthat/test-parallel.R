skip_if_not_installed("future")
skip_if_not_installed("furrr")
if (!"CausalStress" %in% rownames(utils::installed.packages())) {
  skip("CausalStress must be installed for parallel tests (required for worker sessions).")
}

test_that("cs_run_seeds parallel reproducibility matches serial", {
  # baseline plan
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

  future::plan(future::multisession, workers = 2)
  withr::defer(future::plan(future::sequential))

  parallel_res <- cs_run_seeds(
    dgp_id       = "synth_baseline",
    estimator_id = "lm_att",
    n            = 50,
    seeds        = 1:4,
    bootstrap    = FALSE,
    B            = 0,
    show_progress = FALSE,
    parallel      = TRUE
  )

  expect_equal(serial$est_att, parallel_res$est_att)
})

test_that("parallel execution provides speedup for slow estimators", {
  # Register a slow estimator
  slow_tbl <- .causalstress_estimator_registry_extra$tbl
  withr::defer({ .causalstress_estimator_registry_extra$tbl <- slow_tbl })

  slow_estimator <- function(df, config = list(), tau = cs_tau_oracle, ...) {
    Sys.sleep(0.5)
    list(
      att = list(estimate = mean(df$y)),
      qst = NULL,
      meta = list(estimator_id = "slow_mock")
    )
  }

  cs_register_estimator(
    estimator_id  = "slow_mock",
    type          = "gcomp",
    generator     = slow_estimator,
    oracle        = FALSE,
    supports_qst  = FALSE,
    description   = "Slow mock estimator for parallel timing test",
    source        = "test"
  )

  seeds <- 1:4

  future::plan(future::sequential)
  t_serial <- system.time({
    cs_run_seeds(
      dgp_id       = "synth_baseline",
      estimator_id = "slow_mock",
      n            = 20,
      seeds        = seeds,
      bootstrap    = FALSE,
      B            = 0,
      show_progress = FALSE,
      parallel      = FALSE
    )
  })["elapsed"]

  future::plan(future::multisession, workers = 2)
  withr::defer(future::plan(future::sequential))

  t_parallel <- system.time({
    cs_run_seeds(
      dgp_id       = "synth_baseline",
      estimator_id = "slow_mock",
      n            = 20,
      seeds        = seeds,
      bootstrap    = FALSE,
      B            = 0,
      show_progress = FALSE,
      parallel      = TRUE
    )
  })["elapsed"]

  expect_lt(as.numeric(t_parallel), as.numeric(t_serial))
})
