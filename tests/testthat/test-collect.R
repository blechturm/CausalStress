test_that("cs_collect_att returns expected columns and rows with bootstrap", {
  board <- pins::board_temp()

  runs <- cs_run_grid(
    dgp_ids       = "synth_baseline",
    estimator_ids = c("lm_att", "ipw_att"),
    n             = 100,
    seeds         = 1:3,
    bootstrap     = TRUE,
    B             = 30,
    board         = board
  )

  tidy_runs <- cs_tidy(runs)
  att <- cs_collect_att(tidy_runs)

  expect_s3_class(att, "tbl_df")
  expect_equal(nrow(att), 2 * 3) # 2 estimators * 3 seeds

  needed <- c(
    "dgp_id", "estimator_id", "n", "seed",
    "oracle", "supports_qst",
    "true_att", "est_att", "att_error", "att_abs_error",
    "att_ci_lo", "att_ci_hi", "att_covered", "att_ci_width",
    "n_boot_ok"
  )
  expect_true(all(needed %in% names(att)))
  expect_true(any(!is.na(att$att_ci_lo)))
  expect_true(any(!is.na(att$att_ci_hi)))
})

test_that("cs_collect_att handles no-bootstrap case", {
  runs <- cs_run_grid(
    dgp_ids       = "synth_baseline",
    estimator_ids = c("lm_att", "ipw_att"),
    n             = 80,
    seeds         = 1:2,
    bootstrap     = FALSE,
    B             = 0
  )

  tidy_runs <- cs_tidy(runs)
  att <- cs_collect_att(tidy_runs)

  expect_s3_class(att, "tbl_df")
  expect_equal(nrow(att), 2 * 2)
  expect_true(all(c("att_ci_lo", "att_ci_hi") %in% names(att)))
  # with no bootstrap, CIs should be NA
  expect_true(all(is.na(att$att_ci_lo)))
  expect_true(all(is.na(att$att_ci_hi)))
})

test_that("cs_collect_qst subsets QST-like columns", {
  fake <- tibble::tibble(
    dgp_id = "d", estimator_id = "e", n = 1L, seed = 1L, tau = 0.5,
    true_qst = 0.1, est_qst = 0.2, qst_error = 0.1, qst_abs_error = 0.1,
    qst_ci_lo = NA_real_, qst_ci_hi = NA_real_, qst_covered = NA,
    qst_ci_width = NA_real_, n_boot_qst_ok = 0L,
    other = 123
  )

  qst <- cs_collect_qst(fake)
  needed <- c(
    "dgp_id", "estimator_id", "n", "seed", "tau",
    "true_qst", "est_qst", "qst_error", "qst_abs_error",
    "qst_ci_lo", "qst_ci_hi", "qst_covered", "qst_ci_width",
    "n_boot_qst_ok"
  )
  expect_true(all(needed %in% names(qst)))
  expect_false("other" %in% names(qst))
})
