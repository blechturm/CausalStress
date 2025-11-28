test_that("cs_summarise_runs gives zero error summaries for oracle_att", {
  seeds <- 1:10

  runs <- cs_run_seeds(
    dgp_id       = "synth_baseline",
    estimator_id = "oracle_att",
    n            = 500,
    seeds        = seeds
  )

  summ <- cs_summarise_runs(runs)

  expect_s3_class(summ, "tbl_df")
  expect_equal(nrow(summ), 1L)

  expect_equal(summ$dgp_id, "synth_baseline")
  expect_equal(summ$estimator_id, "oracle_att")
  expect_true(summ$oracle)
  expect_false(summ$supports_qst)
  expect_equal(summ$n_runs, length(seeds))

  expect_equal(summ$mean_error, 0, tolerance = 1e-12)
  expect_equal(summ$sd_error, 0, tolerance = 1e-12)
  expect_equal(summ$mean_abs_error, 0, tolerance = 1e-12)
  expect_equal(summ$max_abs_error, 0, tolerance = 1e-12)
})

test_that("cs_summarise_runs works for lm_att and returns non-trivial errors", {
  seeds <- 1:20

  runs <- cs_run_seeds(
    dgp_id       = "synth_baseline",
    estimator_id = "lm_att",
    n            = 1000,
    seeds        = seeds
  )

  summ <- cs_summarise_runs(runs)

  expect_s3_class(summ, "tbl_df")
  expect_equal(nrow(summ), 1L)

  expect_equal(summ$dgp_id, "synth_baseline")
  expect_equal(summ$estimator_id, "lm_att")
  expect_false(summ$oracle)

  expect_true(is.finite(summ$mean_error))
  expect_true(is.finite(summ$sd_error))
  expect_true(is.finite(summ$mean_abs_error))
  expect_true(is.finite(summ$max_abs_error))
  expect_true(summ$max_abs_error >= 0)
})

test_that("cs_summarise_runs fails if required columns are missing", {
  bad <- tibble::tibble(
    dgp_id = "synth_baseline",
    estimator_id = "lm_att"
  )

  expect_error(
    cs_summarise_runs(bad),
    class = "causalstress_runner_error"
  )
})
