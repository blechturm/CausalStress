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
      log            = NA_character_,
      config_fingerprint = cs_build_config_fingerprint(
        dgp_id = "synth_baseline",
        estimator_id = "lm_att",
        n = 100L,
        seed = 1L,
        bootstrap = FALSE,
        B = 0L,
        oracle = FALSE,
        estimator_version = cs_estimator_registry()$version[cs_estimator_registry()$estimator_id == "lm_att"],
        config = list(seed = 1L, ci_method = "none"),
        tau = cs_tau_oracle
      )
    )
  )

  cs_pin_write(board, fake_result)
  expect_true(CausalStress:::cs_pin_exists(board, "synth_baseline", "lm_att", 100, 1))

  runs_resumed <- cs_run_seeds(
    dgp_id       = "synth_baseline",
    estimator_id = "lm_att",
    n            = 100,
    seeds        = 1:4,
    bootstrap    = FALSE,
    B            = 0,
    board        = board,
    skip_existing = TRUE,
    show_progress = FALSE,
    quiet = TRUE
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
test_that("fingerprint mismatch triggers error on resume", {
  skip_if_not_installed("pins")

  board <- pins::board_temp()

  # initial run (writes pins)
  cs_run_seeds(
    dgp_id       = "synth_baseline",
    estimator_id = "lm_att",
    n            = 50,
    seeds        = 1:1,
    bootstrap    = FALSE,
    B            = 0,
    board        = board,
    skip_existing = FALSE,
    show_progress = FALSE,
    quiet = TRUE
  )

  # rerun with changed configuration (B) should error due to fingerprint mismatch
  expect_error(
    cs_run_seeds(
      dgp_id       = "synth_baseline",
      estimator_id = "lm_att",
      n            = 50,
      seeds        = 1:1,
    bootstrap    = TRUE,
    B            = 10,
    board        = board,
    skip_existing = TRUE,
    show_progress = FALSE,
    quiet = TRUE
  ),
  "Configuration fingerprint mismatch"
)
})

test_that("force=TRUE overwrites mismatched pins", {
  skip_if_not_installed("pins")

  board <- pins::board_temp()

  # initial run (writes pins)
  cs_run_seeds(
    dgp_id       = "synth_baseline",
    estimator_id = "lm_att",
    n            = 50,
    seeds        = 1:1,
    bootstrap    = FALSE,
    B            = 0,
    board        = board,
    skip_existing = FALSE,
    show_progress = FALSE,
    quiet = TRUE
  )

  # rerun with changed configuration but force overwrite
  runs <- cs_run_seeds(
    dgp_id       = "synth_baseline",
    estimator_id = "lm_att",
    n            = 50,
    seeds        = 1:1,
    bootstrap    = TRUE,
    B            = 10,
    board        = board,
    skip_existing = TRUE,
    force         = TRUE,
    show_progress = FALSE,
    quiet = TRUE
  )

  expect_true(any(!is.na(runs$att_ci_lo)))
  expect_true(any(!is.na(runs$att_ci_hi)))
})
