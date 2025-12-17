test_that("cs_run_campaign supports estimator-specific config overrides", {
  skip_if_not_installed("pins")

  board <- pins::board_temp()

  # Mixed-method: lm_att uses bootstrap; ipw_att uses none.
  res1 <- cs_run_campaign(
    dgp_ids       = "synth_baseline",
    estimator_ids = c("lm_att", "ipw_att"),
    seeds         = 1:2,
    n             = 200,
    config        = list(ci_method = "bootstrap", n_boot = 20),
    config_by_estimator = list(
      ipw_att = list(ci_method = "none")
    ),
    board         = board,
    parallel      = FALSE,
    show_progress = FALSE,
    skip_existing = FALSE
  )

  lm_rows <- dplyr::filter(res1, estimator_id == "lm_att")
  ipw_rows <- dplyr::filter(res1, estimator_id == "ipw_att")

  expect_true(any(!is.na(lm_rows$att_ci_lo)))
  expect_true(any(!is.na(lm_rows$att_ci_hi)))
  expect_true(all(lm_rows$n_boot_ok > 0))

  expect_true(all(is.na(ipw_rows$att_ci_lo)))
  expect_true(all(is.na(ipw_rows$att_ci_hi)))
  expect_true(all(ipw_rows$n_boot_ok == 0))

  # Same config should not error on resume.
  expect_error(
    cs_run_campaign(
      dgp_ids       = "synth_baseline",
      estimator_ids = c("lm_att", "ipw_att"),
      seeds         = 1:2,
      n             = 200,
      config        = list(ci_method = "bootstrap", n_boot = 20),
      config_by_estimator = list(
        ipw_att = list(ci_method = "none")
      ),
      board         = board,
      parallel      = FALSE,
      show_progress = FALSE,
      skip_existing = TRUE
    ),
    NA
  )

  # Changing estimator-specific config should trigger fingerprint mismatch.
  expect_error(
    cs_run_campaign(
      dgp_ids       = "synth_baseline",
      estimator_ids = c("lm_att", "ipw_att"),
      seeds         = 1:2,
      n             = 200,
      config        = list(ci_method = "bootstrap", n_boot = 20),
      config_by_estimator = list(
        ipw_att = list(ci_method = "bootstrap")
      ),
      board         = board,
      parallel      = FALSE,
      show_progress = FALSE,
      skip_existing = TRUE
    ),
    "Configuration fingerprint mismatch"
  )
})

