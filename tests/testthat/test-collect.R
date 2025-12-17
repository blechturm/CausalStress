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

test_that("collect helpers work on raw run objects", {
  res <- list(
    att = list(estimate = 1, true = 0.5, error = 0.5, abs_error = 0.5, ci_lo = NA, ci_hi = NA, boot_covered = NA, ci_width = NA),
    qst = tibble::tibble(tau = 0.5, estimate = 1),
    meta = list(dgp_id = "d", estimator_id = "e", n = 10L, seed = 1L, oracle = FALSE, supports_qst = TRUE)
  )

  qst <- cs_collect_qst(res)
  expect_s3_class(qst, "tbl_df")
  expect_true(all(c("dgp_id", "estimator_id", "tau", "estimate") %in% names(qst)))
})

test_that("cs_collect_qst unnests list-columns", {
  df <- tibble::tibble(
    dgp_id = "d",
    estimator_id = "e",
    n = 1L,
    seed = 1L,
    qst = list(
      tibble::tibble(tau = c(0.1, 0.9), estimate = c(-1, 1))
    )
  )

  out <- cs_collect_qst(df)
  expect_s3_class(out, "tbl_df")
  expect_true(nrow(out) > nrow(df))
  expect_true("tau" %in% names(out))
})
