test_that("cs_run_single works for synth_baseline × oracle_att", {
  n    <- 400L
  seed <- 123L

  res <- cs_run_single(
    dgp_id       = "synth_baseline",
    estimator_id = "oracle_att",
    n            = n,
    seed         = seed
  )

  expect_s3_class(res, "tbl_df")
  expect_equal(nrow(res), 1L)

  expect_equal(res$dgp_id, "synth_baseline")
  expect_equal(res$estimator_id, "oracle_att")
  expect_equal(res$n, n)
  expect_equal(res$seed, seed)
  expect_true(res$oracle)
  expect_false(res$supports_qst)

  expect_equal(res$att_error, 0, tolerance = 1e-12)
  expect_equal(res$att_abs_error, 0, tolerance = 1e-12)
})

test_that("cs_run_single errors for unknown DGP or estimator IDs", {
  expect_error(
    cs_run_single(
      dgp_id       = "does_not_exist",
      estimator_id = "oracle_att",
      n            = 100,
      seed         = 1
    ),
    class = "causalstress_registry_error"
  )

  expect_error(
    cs_run_single(
      dgp_id       = "synth_baseline",
      estimator_id = "does_not_exist",
      n            = 100,
      seed         = 1
    ),
    class = "causalstress_registry_error"
  )
})

test_that("cs_run_single errors for non-positive n", {
  expect_error(
    cs_run_single(
      dgp_id       = "synth_baseline",
      estimator_id = "oracle_att",
      n            = 0,
      seed         = 1
    ),
    class = "causalstress_runner_error"
  )

  expect_error(
    cs_run_single(
      dgp_id       = "synth_baseline",
      estimator_id = "oracle_att",
      n            = -10,
      seed         = 1
    ),
    class = "causalstress_runner_error"
  )
})

test_that("cs_run_single includes estimator metadata for core estimators", {
  runs <- cs_run_single(
    dgp_id       = "synth_baseline",
    estimator_id = "lm_att",
    n            = 200,
    seed         = 1L
  )

  expect_true("estimator_pkgs" %in% names(runs))

  expect_type(runs$estimator_pkgs, "character")

  pkgs <- runs$estimator_pkgs[[1L]]
  expect_true(grepl("^CausalStress=", pkgs))
  expect_false(grepl(";", pkgs))
})

test_that("cs_run_single records package versions for GRF estimator", {
  testthat::skip_if_not_installed("grf")

  reg <- CausalStress:::cs_estimator_registry()
  if (!"grf_dr_att" %in% reg$estimator_id) {
    cs_register_grf_dr_att()
  }

  runs <- cs_run_single(
    dgp_id       = "synth_baseline",
    estimator_id = "grf_dr_att",
    n            = 200,
    seed         = 1L
  )

  expect_true("estimator_pkgs" %in% names(runs))
  pkgs <- runs$estimator_pkgs[[1L]]

  expect_type(pkgs, "character")
  expect_true(grepl("CausalStress=", pkgs))
  expect_true(grepl("grf=", pkgs))
})
