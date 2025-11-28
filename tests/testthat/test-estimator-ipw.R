test_that("est_ipw_att returns a valid estimator object", {
  dgp <- dgp_synth_baseline(n = 1000, seed = 1L)
  df  <- dgp$df

  res <- est_ipw_att(df)

  expect_invisible(cs_check_estimator_output(res, require_qst = FALSE))

  expect_type(res, "list")
  expect_false(is.null(res$att))
  expect_true(is.null(res$qst))

  expect_equal(res$meta$estimator_id, "ipw_att")
  expect_false(res$meta$oracle)
  expect_false(res$meta$supports_qst)

  att <- res$att
  if (is.data.frame(att)) {
    expect_true("estimate" %in% names(att))
  } else {
    expect_true(!is.null(att$estimate))
  }
})

test_that("est_ipw_att has small bias on synth_baseline", {
  runs <- cs_run_seeds(
    dgp_id       = "synth_baseline",
    estimator_id = "ipw_att",
    n            = 1000,
    seeds        = 1:100
  )

  summ <- cs_summarise_runs(runs)

  expect_true(abs(summ$mean_error) < 0.1)
})
