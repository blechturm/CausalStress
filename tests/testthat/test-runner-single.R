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

test_that("cs_run_single returns NA CI fields when ci_method = 'none'", {
  res <- cs_run_single(
    dgp_id       = "synth_baseline",
    estimator_id = "lm_att",
    n            = 100,
    seed         = 1L,
    bootstrap    = FALSE,
    config       = list(ci_method = "none")
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

make_mock_registry <- function(status_value = "stable") {
  gen_fun <- function(n, seed = NULL) {
    if (!is.null(seed)) cs_set_rng(seed)
    X1 <- stats::rnorm(n)
    p  <- stats::plogis(X1)
    w  <- stats::rbinom(n, size = 1L, prob = p)
    y0 <- X1 + stats::rnorm(n)
    y1 <- y0 + 1
    y  <- ifelse(w == 1, y1, y0)
    tau <- rep(1, n)
    list(
      df = tibble::tibble(y = y, w = w, y0 = y0, y1 = y1, p = p, structural_te = tau, X1 = X1),
      true_att = cs_true_att(structural_te = tau, w = w),
      true_qst = tibble::tibble(tau = cs_tau_oracle, value = rep(1, length(cs_tau_oracle))),
      meta = list(dgp_id = "mock_dgp", type = "synthetic", structural_te = tau)
    )
  }
  tibble::tibble(
    dgp_id = "mock_dgp",
    type = "synthetic",
    generator = list(gen_fun),
    version = "1.0.0",
    description = "mock",
    status = status_value,
    rationale = ifelse(status_value %in% c("deprecated", "invalidated"), "legacy", ""),
    date_status_changed = NA_character_,
    design_spec = "1.0.0"
  )
}

test_that("cs_run_single warns for non-stable DGPs via accessor", {
  with_mocked_bindings(
    cs_dgp_registry = function() make_mock_registry(status_value = "experimental"),
    {
      expect_warning(
        cs_run_single(
          dgp_id       = "mock_dgp",
          estimator_id = "lm_att",
          n            = 20,
          seed         = 1L,
          bootstrap    = FALSE
        ),
        "experimental"
      )
    }
  )
})

test_that("cs_run_single errors when no stable or experimental version", {
  with_mocked_bindings(
    cs_dgp_registry = function() make_mock_registry(status_value = "deprecated"),
    {
      expect_error(
        cs_run_single(
          dgp_id       = "mock_dgp",
          estimator_id = "lm_att",
          n            = 10,
          seed         = 1L,
          quiet        = FALSE
        ),
        "No stable or experimental",
        fixed = TRUE
      )
    }
  )
})

test_that("cs_run_single runs without warnings for stable DGPs", {
  with_mocked_bindings(
    cs_dgp_registry = function() make_mock_registry(status_value = "stable"),
    {
      expect_no_warning(
        cs_run_single(
          dgp_id       = "mock_dgp",
          estimator_id = "lm_att",
          n            = 10,
          seed         = 1L,
          quiet        = FALSE
        )
      )
    }
  )
})

test_that("Running a real DGP does not warn for validated baseline", {
  expect_no_warning(
    cs_run_single("synth_baseline", "lm_att", n = 10, seed = 1, quiet = FALSE)
  )
})

test_that("cs_run_single handles estimator crashes gracefully", {
  crash_est <- function(df, config = list(), tau = cs_tau_oracle, ...) {
    stop("Boom")
  }

  cs_register_estimator(
    estimator_id  = "crash_est",
    type          = "test",
    generator     = crash_est,
    oracle        = FALSE,
    supports_qst  = FALSE,
    version       = "0.0.0",
    description   = "Crashing estimator for robustness test",
    source        = "test",
    requires_pkgs = character(0)
  )

  res <- cs_run_single(
    dgp_id       = "synth_baseline",
    estimator_id = "crash_est",
    n            = 50,
    seed         = 123
  )

  expect_type(res, "list")
  expect_false(res$meta$success)
  expect_true(grepl("Boom", res$meta$error))

  row <- cs_result_to_row(res)
  expect_s3_class(row, "tbl_df")
  expect_equal(nrow(row), 1L)
  expect_false(row$success)
  expect_true(is.na(row$est_att))
})

test_that("cs_run_single times out long-running estimators", {
  slow_est <- function(df, config = list(), tau = cs_tau_oracle, ...) {
    Sys.sleep(2)
    list(
      att = list(estimate = 0),
      qst = NULL,
      meta = list(estimator_id = "slow_est", oracle = FALSE, supports_qst = FALSE)
    )
  }

  cs_register_estimator(
    estimator_id  = "slow_est",
    type          = "test",
    generator     = slow_est,
    oracle        = FALSE,
    supports_qst  = FALSE,
    version       = "0.0.0",
    description   = "Slow estimator for timeout test",
    source        = "test",
    requires_pkgs = character(0)
  )

  res <- cs_run_single(
    dgp_id       = "synth_baseline",
    estimator_id = "slow_est",
    n            = 50,
    seed         = 123,
    max_runtime  = 0.5
  )

  expect_type(res, "list")
  expect_false(res$meta$success)
  expect_true(grepl("Timeout|limit", res$meta$error, ignore.case = TRUE))

  row <- cs_result_to_row(res)
  expect_s3_class(row, "tbl_df")
  expect_equal(nrow(row), 1L)
  expect_false(row$success)
})

test_that("cs_run_single calculates QST metrics and CIs", {
  est_id <- "mock_qst_est"
  mock_qst_est <- function(df, tau = cs_tau_oracle, config = list(), ...) {
    list(
      att = list(estimate = 0),
      qst = tibble::tibble(
        tau = c(0.1, 0.5, 0.9),
        value = c(1, 2, 3)
      ),
      meta = list(estimator_id = est_id, oracle = FALSE, supports_qst = TRUE)
    )
  }

  cs_register_estimator(
    estimator_id  = est_id,
    type          = "test",
    generator     = mock_qst_est,
    oracle        = FALSE,
    supports_qst  = TRUE,
    version       = "0.0.0",
    description   = "Mock QST estimator",
    source        = "test",
    requires_pkgs = character(0)
  )

  res <- cs_run_single(
    dgp_id       = "synth_baseline",
    estimator_id = est_id,
    n            = 150,
    seed         = 123,
    tau          = c(0.1, 0.5, 0.9),
    bootstrap    = TRUE,
    B            = 30
  )

  expect_s3_class(res$qst, "tbl_df")
  expect_true(all(c("ci_lo", "ci_hi", "covered") %in% names(res$qst)))
})
