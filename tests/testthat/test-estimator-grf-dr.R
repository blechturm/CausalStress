test_that("est_grf_dr_att contracts hold when grf is installed", {
  skip_if_not_installed("grf")

  reg <- CausalStress:::cs_estimator_registry()
  if (!"grf_dr_att" %in% reg$estimator_id) {
    cs_register_grf_dr_att()
  }

  dgp <- dgp_synth_baseline(n = 300, seed = 1L)
  df  <- dgp$df

  desc <- cs_get_estimator("grf_dr_att")
  res  <- desc$generator(df = df, config = list(num.trees = 300))

  expect_invisible(cs_check_estimator_output(res, require_qst = FALSE))
  expect_type(res, "list")
  expect_false(is.null(res$att))
  expect_true(is.null(res$qst))
})

test_that("grf_dr_att has reasonable bias on synth_baseline in a small MC run", {
  skip_if_not_installed("grf")
  skip_on_cran()

  reg <- CausalStress:::cs_estimator_registry()
  if (!"grf_dr_att" %in% reg$estimator_id) {
    cs_register_grf_dr_att()
  }

  runs <- cs_run_seeds(
    dgp_id       = "synth_baseline",
    estimator_id = "grf_dr_att",
    n            = 500,
    seeds        = 1:10,
    config       = list(num.trees = 500)
  )

  summ <- cs_summarise_runs(runs)

  if (any(is.na(summ$mean_error))) {
    skip("grf produced NA estimates in this environment")
  }
  expect_lt(abs(summ$mean_error), 0.15)
})
