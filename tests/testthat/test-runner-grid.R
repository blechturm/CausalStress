test_that("cs_run_grid returns expected rows and structure", {
  dgp_ids       <- c("synth_baseline", "synth_heavytail")
  estimator_ids <- c("lm_att", "ipw_att")
  seeds         <- 1:3

  runs <- cs_run_grid(
    dgp_ids       = dgp_ids,
    estimator_ids = estimator_ids,
    n             = 200,
    seeds         = seeds
  )

  expect_s3_class(runs, "tbl_df")
  expect_equal(
    nrow(runs),
    length(dgp_ids) * length(estimator_ids) * length(seeds)
  )

  expect_true(all(runs$dgp_id %in% dgp_ids))
  expect_true(all(runs$estimator_id %in% estimator_ids))
  expect_true(all(runs$seed %in% seeds))

  expected_cols <- c(
    "dgp_id", "estimator_id", "n", "seed",
    "oracle", "supports_qst",
    "true_att", "est_att", "att_error", "att_abs_error",
    "att_ci_lo", "att_ci_hi", "att_covered", "att_ci_width", "n_boot_ok",
    "estimator_pkgs"
  )
  expect_true(all(expected_cols %in% names(runs)))
})

test_that("cs_run_grid fails for unknown DGP IDs", {
  expect_error(
    cs_run_grid(
      dgp_ids       = c("synth_baseline", "does_not_exist"),
      estimator_ids = "lm_att",
      n             = 100,
      seeds         = 1:2
    ),
    class = "causalstress_registry_error"
  )
})

test_that("cs_run_grid fails for unknown estimator IDs", {
  expect_error(
    cs_run_grid(
      dgp_ids       = "synth_baseline",
      estimator_ids = c("lm_att", "does_not_exist"),
      n             = 100,
      seeds         = 1:2
    ),
    class = "causalstress_registry_error"
  )
})

test_that("cs_run_grid output works with cs_summarise_runs", {
  runs <- cs_run_grid(
    dgp_ids       = c("synth_baseline", "synth_heavytail"),
    estimator_ids = c("lm_att", "ipw_att"),
    n             = 200,
    seeds         = 1:5
  )

  summary <- cs_summarise_runs(runs)

  expect_s3_class(summary, "tbl_df")
  expect_true(all(c("dgp_id", "estimator_id", "n", "n_runs") %in% names(summary)))
  expect_true(all(summary$n_runs == 5L))
})
