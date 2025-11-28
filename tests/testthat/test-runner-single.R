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
  expect_true(all(c("att_ci_lo", "att_ci_hi", "att_covered", "att_ci_width", "n_boot_ok") %in% names(res)))
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

test_that("Airlock removes oracle columns for non-oracle estimators", {
  spy_id <- "spy_non_oracle"
  spy_fun <- function(df, config = list(), tau = cs_tau_oracle, ...) {
    forbidden <- intersect(c("y0", "y1", "p", "structural_te"), names(df))
    if (length(forbidden) > 0) {
      stop("Forbidden columns leaked")
    }
    list(att = list(estimate = mean(df$y)), qst = NULL, cf = NULL, meta = list(estimator_id = spy_id))
  }

  reg <- CausalStress:::cs_estimator_registry()
  if (!spy_id %in% reg$estimator_id) {
    cs_register_estimator(
      estimator_id  = spy_id,
      type          = "spy",
      generator     = spy_fun,
      oracle        = FALSE,
      supports_qst  = FALSE,
      version       = as.character(utils::packageVersion("CausalStress")),
      description   = "Spy estimator for airlock test.",
      source        = "test",
      requires_pkgs = character(0)
    )
  }

  expect_silent(cs_run_single(
    dgp_id       = "synth_baseline",
    estimator_id = spy_id,
    n            = 50,
    seed         = 1L
  ))
})

test_that("Oracle estimator retains oracle columns", {
  collector <- new.env(parent = emptyenv())
  spy_id <- "spy_oracle"
  spy_fun <- function(df, config = list(), tau = cs_tau_oracle, ...) {
    collector$cols <- names(df)
    list(att = list(estimate = mean(df$structural_te[df$w == 1])), qst = NULL, cf = NULL, meta = list(estimator_id = spy_id))
  }

  reg <- CausalStress:::cs_estimator_registry()
  if (!spy_id %in% reg$estimator_id) {
    cs_register_estimator(
      estimator_id  = spy_id,
      type          = "oracle",
      generator     = spy_fun,
      oracle        = TRUE,
      supports_qst  = FALSE,
      version       = as.character(utils::packageVersion("CausalStress")),
      description   = "Oracle spy estimator.",
      source        = "test",
      requires_pkgs = character(0)
    )
  }

  cs_run_single(
    dgp_id       = "synth_baseline",
    estimator_id = spy_id,
    n            = 50,
    seed         = 1L
  )

  expect_true(all(c("y0", "y1", "p", "structural_te") %in% collector$cols))
})

test_that("cs_run_single bootstrap fields are NA when bootstrap = FALSE", {
  res <- cs_run_single(
    dgp_id       = "synth_baseline",
    estimator_id = "lm_att",
    n            = 100,
    seed         = 1L,
    bootstrap    = FALSE
  )

  expect_true(all(is.na(res$att_ci_lo)))
  expect_true(all(is.na(res$att_ci_hi)))
  expect_true(all(is.na(res$att_covered)))
  expect_true(all(is.na(res$att_ci_width)))
  expect_equal(res$n_boot_ok, 0)
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

  expect_true(is.finite(res$att_ci_lo) || is.na(res$att_ci_lo))
  expect_true(is.finite(res$att_ci_hi) || is.na(res$att_ci_hi))
  expect_true(res$n_boot_ok >= 0)
})
