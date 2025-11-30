test_that("cs_run_campaign is an alias for cs_run_seeds", {
  board <- pins::board_temp()

  runs_seeds <- cs_run_seeds(
    dgp_id       = "synth_baseline",
    estimator_id = "lm_att",
    seeds        = 1:2,
    n            = 100,
    bootstrap    = FALSE,
    B            = 0,
    board        = board,
    skip_existing = TRUE,
    show_progress = FALSE
  )

  runs_campaign <- cs_run_campaign(
    dgp_id       = "synth_baseline",
    estimator_id = "lm_att",
    seeds        = 1:2,
    n            = 100,
    bootstrap    = FALSE,
    B            = 0,
    board        = board,
    skip_existing = TRUE,
    show_progress = FALSE
  )

  expect_identical(runs_campaign, runs_seeds)
})
