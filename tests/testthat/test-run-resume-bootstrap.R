test_that("resume refuses cached runs without bootstrap when bootstrap is requested", {
  board <- pins::board_temp()

  # First run without bootstrap (pins persisted)
  cs_run_campaign(
    dgp_id       = "synth_baseline",
    estimator_id = "lm_att",
    n            = 200,
    seeds        = 1:2,
    bootstrap    = FALSE,
    B            = 0,
    board        = board,
    skip_existing = TRUE,
    show_progress = FALSE
  )

  # Now request bootstrap with same seeds/board; should error
  expect_error(
    cs_run_campaign(
      dgp_id       = "synth_baseline",
      estimator_id = "lm_att",
      n            = 200,
      seeds        = 1:2,
      bootstrap    = TRUE,
      B            = 200,
      board        = board,
      skip_existing = TRUE,
      show_progress = FALSE
    ),
    "computed without bootstrap CIs"
  )

  # Force recompute with skip_existing = FALSE
  runs_boot <- cs_run_campaign(
    dgp_id       = "synth_baseline",
    estimator_id = "lm_att",
    n            = 200,
    seeds        = 1:2,
    bootstrap    = TRUE,
    B            = 50,
    board        = board,
    skip_existing = FALSE,
    show_progress = FALSE
  )

  expect_true(any(!is.na(runs_boot$att_ci_lo)))
  expect_true(any(!is.na(runs_boot$att_ci_hi)))
})
