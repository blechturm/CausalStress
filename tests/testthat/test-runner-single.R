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

  expected_cols <- c(
    "dgp_id", "estimator_id",
    "n", "seed",
    "oracle", "supports_qst",
    "true_att", "est_att",
    "att_error", "att_abs_error"
  )
  expect_true(all(expected_cols %in% names(res)))

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
      n            = 100
    ),
    class = "causalstress_registry_error"
  )

  expect_error(
    cs_run_single(
      dgp_id       = "synth_baseline",
      estimator_id = "does_not_exist",
      n            = 100
    ),
    class = "causalstress_registry_error"
  )
})

test_that("cs_run_single errors for non-positive n", {
  expect_error(
    cs_run_single(
      dgp_id       = "synth_baseline",
      estimator_id = "oracle_att",
      n            = 0
    ),
    class = "causalstress_runner_error"
  )

  expect_error(
    cs_run_single(
      dgp_id       = "synth_baseline",
      estimator_id = "oracle_att",
      n            = -10
    ),
    class = "causalstress_runner_error"
  )
})
