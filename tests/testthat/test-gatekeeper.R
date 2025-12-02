test_that("gatekeeper flags failing estimators and culprits", {
  dummy <- tibble::tibble(
    dgp_id = c("synth_placebo_tau0", "synth_placebo_tau0", "synth_placebo_heavytail"),
    estimator_id = c("lm_att", "lm_att", "ipw_att"),
    att_covered = c(TRUE, FALSE, FALSE)
  )

  res <- cs_summarise_gatekeeper(dummy, threshold = 0.9)

  verdict <- res$verdict
  expect_true("lm_att" %in% verdict$estimator_id)
  expect_true("ipw_att" %in% verdict$estimator_id)

  lm_status <- verdict$status[verdict$estimator_id == "lm_att"]
  ipw_status <- verdict$status[verdict$estimator_id == "ipw_att"]
  expect_equal(lm_status, "FAIL")
  expect_equal(ipw_status, "FAIL")

  culprits <- res$culprits
  expect_true(any(culprits$dgp_id == "synth_placebo_tau0"))
  expect_true(any(culprits$dgp_id == "synth_placebo_heavytail"))
})
