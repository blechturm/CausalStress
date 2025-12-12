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
