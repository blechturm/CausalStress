test_that("cs_run_single works for synth_baseline × oracle_att", {
  n    <- 400L
  seed <- 123L

  res <- cs_run_single(
    dgp_id       = "synth_baseline",
    estimator_id = "oracle_att",
    n            = n,
    seed         = seed
  )

  expect_type(res, "list")
  expect_true(is.list(res$att))
  expect_true(is.list(res$meta))
  expect_equal(res$meta$dgp_id, "synth_baseline")
  expect_equal(res$meta$estimator_id, "oracle_att")
  expect_equal(res$meta$n, n)
  expect_equal(res$meta$seed, seed)
  expect_equal(res$att$error, res$att$estimate - res$att$true)
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
  res <- cs_run_single(
    dgp_id       = "synth_baseline",
    estimator_id = "lm_att",
    n            = 200,
    seed         = 1L
  )

  expect_true(is.character(res$meta$estimator_pkgs) || is.null(res$meta$estimator_pkgs))
  expect_true(is.na(res$meta$log) || is.character(res$meta$log))

  pkgs <- res$meta$estimator_pkgs
  expect_true(grepl("^CausalStress=", pkgs))
  expect_false(grepl(";", pkgs))
})

test_that("cs_run_single records package versions for GRF estimator", {
  testthat::skip_if_not_installed("grf")

  reg <- CausalStress:::cs_estimator_registry()
  if (!"grf_dr_att" %in% reg$estimator_id) {
    cs_register_grf_dr_att()
  }

  res <- cs_run_single(
    dgp_id       = "synth_baseline",
    estimator_id = "grf_dr_att",
    n            = 200,
    seed         = 1L
  )

  pkgs <- res$meta$estimator_pkgs

  expect_type(pkgs, "character")
  expect_true(grepl("CausalStress=", pkgs))
  expect_true(grepl("grf=", pkgs))
})

test_that("cs_run_single attaches a log field (NA for clean runs)", {
  res <- cs_run_single(
    dgp_id       = "synth_baseline",
    estimator_id = "oracle_att",
    n            = 100,
    seed         = 1,
    bootstrap    = FALSE
  )

  expect_true(is.na(res$meta$log) || is.character(res$meta$log))
})

test_that("cs_run_single bootstrap fields are NA when bootstrap = FALSE", {
  res <- cs_run_single(
    dgp_id       = "synth_baseline",
    estimator_id = "lm_att",
    n            = 100,
    seed         = 1L,
    bootstrap    = FALSE
  )

  expect_true(is.na(res$att$ci_lo))
  expect_true(is.na(res$att$ci_hi))
  expect_true(is.na(res$att$boot_covered))
  expect_true(is.na(res$att$error) || is.finite(res$att$error))
  expect_equal(res$meta$n_boot_ok, 0)
})

test_that("cs_run_single computes bootstrap CIs when requested", {
  res <- cs_run_single(
    dgp_id       = "synth_baseline",
    estimator_id = "lm_att",
    n            = 150,
    seed         = 1L,
    bootstrap    = TRUE,
    B            = 20
  )

  expect_true(is.finite(res$att$ci_lo) || is.na(res$att$ci_lo))
  expect_true(is.finite(res$att$ci_hi) || is.na(res$att$ci_hi))
  expect_true(res$meta$n_boot_ok >= 0)
  boot_draws <- res$boot_draws
  expect_true(is.null(boot_draws) || is.data.frame(boot_draws))
})

test_that("cs_result_to_row converts rich result to expected tibble", {
  res <- cs_run_single(
    dgp_id       = "synth_baseline",
    estimator_id = "oracle_att",
    n            = 50,
    seed         = 1L,
    bootstrap    = FALSE
  )

  row <- cs_result_to_row(res)
  expect_s3_class(row, "tbl_df")
  expect_true(all(c("dgp_id", "estimator_id", "n", "seed", "true_att", "est_att") %in% names(row)))
})

test_that("cs_tidy_run flattens a single run to a one-row tibble", {
  res <- cs_run_single(
    dgp_id       = "synth_baseline",
    estimator_id = "lm_att",
    n            = 200,
    seed         = 1L,
    bootstrap    = FALSE
  )

  row <- cs_tidy_run(res)

  expect_s3_class(row, "tbl_df")
  expect_equal(nrow(row), 1L)
  expect_true(all(c("dgp_id", "estimator_id", "true_att", "est_att") %in% names(row)))
})
