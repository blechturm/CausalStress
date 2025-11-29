test_that("cs_run_seeds works for multiple seeds with oracle_att", {
  seeds <- c(1L, 2L, 3L, 4L, 5L)

  res <- cs_run_seeds(
    dgp_id       = "synth_baseline",
    estimator_id = "oracle_att",
    n            = 500,
    seeds        = seeds
  )

  expect_s3_class(res, "tbl_df")
  expect_equal(nrow(res), length(seeds))

  expect_true(all(res$seed %in% seeds))
  expect_setequal(res$seed, seeds)

  expect_true(all(abs(res$att_error) < 1e-12))
  expect_true(all(abs(res$att_abs_error) < 1e-12))
})

test_that("cs_run_seeds works for lm_att and returns reasonable structure", {
  seeds <- 1:3

  res <- cs_run_seeds(
    dgp_id       = "synth_baseline",
    estimator_id = "lm_att",
    n            = 500,
    seeds        = seeds
  )

  expect_s3_class(res, "tbl_df")
  expect_equal(nrow(res), length(seeds))

  expected_cols <- c(
    "dgp_id", "estimator_id",
    "n", "seed",
    "oracle", "supports_qst",
    "true_att", "est_att",
    "att_error", "att_abs_error",
    "att_ci_lo", "att_ci_hi", "att_covered", "att_ci_width", "n_boot_ok",
    "estimator_pkgs", "log"
  )
  expect_true(all(expected_cols %in% names(res)))

  expect_false(any(res$oracle))
})

test_that("cs_run_seeds computes bootstrap CIs when requested", {
  seeds <- 1:3
  res <- cs_run_seeds(
    dgp_id       = "synth_baseline",
    estimator_id = "lm_att",
    n            = 200,
    seeds        = seeds,
    bootstrap    = TRUE,
    B            = 20
  )

  expect_true(any(!is.na(res$att_ci_lo)))
  expect_true(any(res$n_boot_ok > 0))
})

test_that("cs_run_seeds validates seeds input", {
  expect_error(
    cs_run_seeds(
      dgp_id       = "synth_baseline",
      estimator_id = "oracle_att",
      n            = 100,
      seeds        = numeric(0)
    ),
    class = "causalstress_runner_error"
  )

  expect_error(
    cs_run_seeds(
      dgp_id       = "synth_baseline",
      estimator_id = "oracle_att",
      n            = 100,
      seeds        = c("a", "b")
    ),
    class = "causalstress_runner_error"
  )

  expect_error(
    cs_run_seeds(
      dgp_id       = "synth_baseline",
      estimator_id = "oracle_att",
      n            = 100,
      seeds        = c(1, NA)
    ),
    class = "causalstress_runner_error"
  )
})
