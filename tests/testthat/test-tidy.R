test_that("cs_tidy works on a single rich run", {
  res <- cs_run_single(
    dgp_id       = "synth_baseline",
    estimator_id = "lm_att",
    n            = 200,
    seed         = 1L,
    bootstrap    = FALSE
  )

  row <- cs_tidy(res)

  expect_s3_class(row, "tbl_df")
  expect_equal(nrow(row), 1L)
  expect_true(all(c("dgp_id", "estimator_id", "true_att", "est_att") %in% names(row)))
})

test_that("cs_tidy returns tibble unchanged for cs_run_seeds output", {
  seeds <- 1:3
  runs <- cs_run_seeds(
    dgp_id       = "synth_baseline",
    estimator_id = "lm_att",
    n            = 100,
    seeds        = seeds
  )

  tidy_runs <- cs_tidy(runs)
  expect_s3_class(tidy_runs, "tbl_df")
  expect_equal(tidy_runs, runs)
})

test_that("cs_tidy handles list of rich runs", {
  res1 <- cs_run_single(
    dgp_id       = "synth_baseline",
    estimator_id = "lm_att",
    n            = 50,
    seed         = 1L,
    bootstrap    = FALSE
  )
  res2 <- cs_run_single(
    dgp_id       = "synth_baseline",
    estimator_id = "lm_att",
    n            = 50,
    seed         = 2L,
    bootstrap    = FALSE
  )

  tidy_runs <- cs_tidy(list(res1, res2))
  expect_s3_class(tidy_runs, "tbl_df")
  expect_equal(nrow(tidy_runs), 2L)
})

test_that("cs_tidy errors on unsupported input", {
  expect_error(
    cs_tidy(123),
    class = "causalstress_type_error"
  )
})
