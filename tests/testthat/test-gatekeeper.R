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
