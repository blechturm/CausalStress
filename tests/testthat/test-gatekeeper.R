test_that("gatekeeper flags failing estimators and culprits", {
  dummy <- tibble::tibble(
    dgp_id = c("synth_placebo_tau0", "synth_placebo_tau0", "synth_placebo_heavytail"),
    estimator_id = c("lm_att", "lm_att", "ipw_att"),
    att_covered = c(TRUE, FALSE, FALSE)
  )

  res <- cs_summarise_gatekeeper(dummy, threshold = 0.9)

  verdict <- res$att_verdict
  expect_true("lm_att" %in% verdict$estimator_id)
  expect_true("ipw_att" %in% verdict$estimator_id)

  lm_status <- verdict$status[verdict$estimator_id == "lm_att"]
  ipw_status <- verdict$status[verdict$estimator_id == "ipw_att"]
  expect_equal(lm_status, "FAIL")
  expect_equal(ipw_status, "FAIL")

  culprits <- res$att_culprits
  expect_true(any(culprits$dgp_id == "synth_placebo_tau0"))
  expect_true(any(culprits$dgp_id == "synth_placebo_heavytail"))
})

test_that("Gatekeeper enforces 10/10 rule for QST", {
  make_qst_run <- function(estimator_id, seed, covered_vec) {
    tibble::tibble(
      dgp_id = "synth_placebo_tau0",
      estimator_id = estimator_id,
      seed = seed,
      att_covered = TRUE,
      qst = list(
        tibble::tibble(
          tau = seq_along(covered_vec) / 10,
          estimate = 0,
          true = 0,
          error = 0,
          abs_error = 0,
          ci_lo = -1,
          ci_hi = 1,
          covered = covered_vec,
          ci_width = 2
        )
      )
    )
  }

  # Scenario A: all runs cover zero -> PASS
  pass_runs <- dplyr::bind_rows(
    make_qst_run("est_pass", 1, rep(TRUE, 10)),
    make_qst_run("est_pass", 2, rep(TRUE, 10))
  )

  # Scenario B: one run fails (20% tau uncovered) -> run flagged, estimator FAIL
  fail_run <- make_qst_run("est_one_fail", 1, c(rep(FALSE, 2), rep(TRUE, 8)))
  pass_run <- make_qst_run("est_one_fail", 2, rep(TRUE, 10))
  one_fail_runs <- dplyr::bind_rows(fail_run, pass_run)

  # Scenario C: 3/20 runs fail (15%) -> estimator FAIL
  many_runs <- dplyr::bind_rows(
    lapply(1:20, function(s) {
      covered <- if (s <= 3) c(rep(FALSE, 2), rep(TRUE, 8)) else rep(TRUE, 10)
      make_qst_run("est_many_fail", s, covered)
    })
  )

  suite_results <- dplyr::bind_rows(pass_runs, one_fail_runs, many_runs)

  res <- cs_summarise_gatekeeper(suite_results, threshold = 0.9)

  qverdict <- res$qst_verdict
  expect_equal(qverdict$status[qverdict$estimator_id == "est_pass"], "PASS")
  expect_equal(qverdict$status[qverdict$estimator_id == "est_one_fail"], "FAIL")
  expect_equal(qverdict$status[qverdict$estimator_id == "est_many_fail"], "FAIL")

  qculprits <- res$qst_culprits
  expect_true(any(qculprits$estimator_id == "est_one_fail" & qculprits$seed == 1))
  expect_true(any(qculprits$estimator_id == "est_many_fail" & qculprits$seed <= 3))
})

test_that("gatekeeper marks CI-less ATT paths as UNVERIFIED", {
  dummy <- tibble::tibble(
    dgp_id = c("synth_placebo_tau0", "synth_placebo_heavytail"),
    estimator_id = c("ci_less", "ci_less"),
    att_covered = c(NA, NA)
  )

  res <- cs_summarise_gatekeeper(dummy, threshold = 0.9)

  expect_equal(res$att_verdict$status, "UNVERIFIED")
  expect_true(is.na(res$att_verdict$coverage_rate))
  expect_equal(nrow(res$att_culprits), 0L)
})

test_that("gatekeeper marks CI-less QST paths as UNVERIFIED", {
  suite_results <- tibble::tibble(
    dgp_id = "synth_placebo_tau0",
    estimator_id = "qst_ci_less",
    seed = 1L,
    att_covered = NA,
    qst = list(
      tibble::tibble(
        tau = c(0.1, 0.2),
        estimate = c(0, 0),
        true = c(0, 0),
        error = c(0, 0),
        abs_error = c(0, 0),
        ci_lo = c(NA_real_, NA_real_),
        ci_hi = c(NA_real_, NA_real_),
        covered = c(NA, NA),
        ci_width = c(NA_real_, NA_real_)
      )
    )
  )

  res <- cs_summarise_gatekeeper(suite_results, threshold = 0.9)

  expect_equal(res$qst_verdict$status, "UNVERIFIED")
  expect_true(is.na(res$qst_verdict$run_fail_rate))
  expect_equal(nrow(res$qst_culprits), 0L)
})

test_that("gatekeeper QST failure rate excludes unverified runs from denominator", {
  make_qst <- function(seed, covered_vec) {
    tibble::tibble(
      dgp_id = "synth_placebo_tau0",
      estimator_id = "qst_mixed",
      seed = seed,
      att_covered = NA,
      qst = list(
        tibble::tibble(
          tau = seq_along(covered_vec) / 10,
          estimate = 0,
          true = 0,
          error = 0,
          abs_error = 0,
          ci_lo = NA_real_,
          ci_hi = NA_real_,
          covered = covered_vec,
          ci_width = NA_real_
        )
      )
    )
  }

  suite_results <- dplyr::bind_rows(
    make_qst(1L, c(FALSE, FALSE, rep(TRUE, 8))),
    make_qst(2L, rep(NA, 10)),
    make_qst(3L, rep(NA, 10))
  )

  res <- cs_summarise_gatekeeper(suite_results, threshold = 0.9)

  expect_equal(res$qst_verdict$n_verified, 1L)
  expect_equal(res$qst_verdict$run_fail_rate, 1)
  expect_equal(res$qst_verdict$status, "FAIL")
})

test_that("gatekeeper exposes ATE structure without Wave 1 policy consequences", {
  dummy <- tibble::tibble(
    dgp_id = c("synth_placebo_tau0", "synth_placebo_heavytail"),
    estimator_id = c("lm_att", "oracle_att"),
    att_covered = c(TRUE, TRUE)
  )

  res <- cs_summarise_gatekeeper(dummy, threshold = 0.9)

  expect_true(all(c("ate_verdict", "ate_culprits") %in% names(res)))
  expect_s3_class(res$ate_verdict, "tbl_df")
  expect_equal(sort(res$ate_verdict$estimator_id), c("lm_att", "oracle_att"))
  expect_equal(unique(res$ate_verdict$estimand_target_id), "ate")
  expect_equal(unique(res$ate_verdict$status), "UNVERIFIED")
  expect_equal(unique(res$ate_verdict$policy_status), "deferred_gatekeeper_recalibration")
  expect_true(all(is.na(res$ate_verdict$threshold)))
  expect_true(all(is.na(res$ate_verdict$registry_consequence)))
  expect_equal(nrow(res$ate_culprits), 0L)
  expect_false(any(grepl("Non-Robust", unlist(res$ate_verdict, use.names = FALSE), fixed = TRUE)))
})
