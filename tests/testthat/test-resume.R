test_that("cs_run_seeds can resume from existing pins without recomputing", {
  skip_if_not_installed("pins")

  board <- pins::board_temp()

  runs_initial <- cs_run_seeds(
    dgp_id       = "synth_baseline",
    estimator_id = "lm_att",
    n            = 100,
    seeds        = 1:2,
    bootstrap    = TRUE,
    B            = 30,
    board        = board,
    skip_existing = FALSE
  )

  expect_equal(nrow(runs_initial), 2L)
  expect_true(CausalStress:::cs_pin_exists(board, "synth_baseline", "lm_att", 100, 1))
  expect_true(CausalStress:::cs_pin_exists(board, "synth_baseline", "lm_att", 100, 2))

  runs_resumed <- cs_run_seeds(
    dgp_id       = "synth_baseline",
    estimator_id = "lm_att",
    n            = 100,
    seeds        = 1:4,
    bootstrap    = TRUE,
    B            = 30,
    board        = board,
    skip_existing = TRUE
  )

  expect_equal(nrow(runs_resumed), 4L)

  expect_true(CausalStress:::cs_pin_exists(board, "synth_baseline", "lm_att", 100, 3))
  expect_true(CausalStress:::cs_pin_exists(board, "synth_baseline", "lm_att", 100, 4))

  tidy_res <- cs_tidy(runs_resumed)
  expect_s3_class(tidy_res, "tbl_df")
  expect_equal(nrow(tidy_res), 4L)
})
