test_that("cs_audit lists pinned results with provenance", {
  skip_if_not_installed("pins")

  board <- pins::board_temp()

  fake_result1 <- list(
    att = list(
      estimate     = 1,
      true         = 0,
      error        = 1,
      abs_error    = 1,
      ci_lo        = NA_real_,
      ci_hi        = NA_real_,
      boot_covered = NA,
      ci_width     = NA_real_
    ),
    qst = NULL,
    boot_draws = NULL,
    meta = list(
      dgp_id = "synth_baseline",
      estimator_id = "lm_att",
      estimator_version = "0.1.0",
      n = 50L,
      seed = 1L,
      oracle = FALSE,
      supports_qst = FALSE,
      estimator_pkgs = "",
      n_boot_ok = 0L,
      log = NA_character_
    )
  )

  fake_result2 <- fake_result1
  fake_result2$meta$seed <- 2L
  fake_result2$meta$estimator_id <- "ipw_att"

  cs_pin_write(board, fake_result1)
  cs_pin_write(board, fake_result2)

  audit <- cs_audit(board)

  expect_s3_class(audit, "tbl_df")
  expect_equal(nrow(audit), 2L)
  expect_true("git_hash" %in% names(audit))
  expect_true(all(audit$estimator_id %in% c("lm_att", "ipw_att")))
})
