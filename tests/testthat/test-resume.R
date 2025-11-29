test_that("cs_run_seeds can resume from existing pins without recomputing", {
  skip_if_not_installed("pins")

  board <- pins::board_temp()

  fake_result <- list(
    att = list(
      estimate     = 999,
      true         = 0,
      error        = 999,
      abs_error    = 999,
      ci_lo        = NA_real_,
      ci_hi        = NA_real_,
      boot_covered = NA,
      ci_width     = NA_real_
    ),
    qst        = NULL,
    boot_draws = NULL,
    meta = list(
      dgp_id         = "synth_baseline",
      estimator_id   = "lm_att",
      n              = 100L,
      seed           = 1L,
      oracle         = FALSE,
      supports_qst   = FALSE,
      estimator_pkgs = "",
      n_boot_ok      = 0L,
      log            = NA_character_
    )
  )

  cs_pin_write(board, fake_result)
  expect_true(CausalStress:::cs_pin_exists(board, "synth_baseline", "lm_att", 100, 1))

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
  # seed 1 should come from cached fake pin
  expect_equal(runs_resumed$est_att[runs_resumed$seed == 1], 999)

  expect_true(CausalStress:::cs_pin_exists(board, "synth_baseline", "lm_att", 100, 3))
  expect_true(CausalStress:::cs_pin_exists(board, "synth_baseline", "lm_att", 100, 4))

  tidy_res <- cs_tidy(runs_resumed)
  expect_s3_class(tidy_res, "tbl_df")
  expect_equal(nrow(tidy_res), 4L)
})
