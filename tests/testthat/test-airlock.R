test_that("airlock strips forbidden columns and attributes for non-oracle estimators", {
  # register spy non-oracle estimator
  spy_non_oracle <- function(df, config = list(), tau = cs_tau_oracle, ...) {
    forbidden <- c("y0", "y1", "p", "structural_te")
    if (any(forbidden %in% names(df))) {
      stop("Airlock failure: forbidden column visible in non-oracle estimator")
    }
    if (!is.null(attr(df, "structural_te")) || !is.null(attr(df, "params"))) {
      stop("Airlock failure: forbidden attributes visible in non-oracle estimator")
    }
    list(
      att = list(estimate = 0),
      qst = NULL,
      meta = list(estimator_id = "spy_non_oracle", oracle = FALSE, supports_qst = FALSE)
    )
  }

  cs_register_estimator(
    estimator_id  = "spy_non_oracle",
    type          = "spy",
    generator     = spy_non_oracle,
    oracle        = FALSE,
    supports_qst  = FALSE,
    version       = "0.0.0",
    description   = "Spy non-oracle estimator for airlock test",
    source        = "test",
    requires_pkgs = character(0)
  )

  expect_no_error(
    cs_run_single(
      dgp_id       = "synth_baseline",
      estimator_id = "spy_non_oracle",
      n            = 100,
      seed         = 1L
    )
  )
})

test_that("airlock grants only requested eligible propensity oracle column", {
  spy_propensity_oracle <- function(df, config = list(), tau = cs_tau_oracle, ...) {
    if (!"p" %in% names(df)) {
      stop("Propensity oracle column not visible")
    }
    forbidden <- c("y0", "y1", "structural_te")
    if (any(forbidden %in% names(df))) {
      stop("Airlock over-granted counterfactual or structural truth")
    }
    list(
      att = list(estimate = 0),
      qst = NULL,
      meta = list(estimator_id = "spy_propensity_oracle", oracle = TRUE, supports_qst = FALSE)
    )
  }

  cs_register_estimator(
    estimator_id  = "spy_propensity_oracle",
    type          = "spy",
    generator     = spy_propensity_oracle,
    oracle        = TRUE,
    oracle_columns = "p",
    supports_qst  = FALSE,
    version       = "0.0.0",
    description   = "Spy propensity oracle estimator for airlock test",
    source        = "test",
    requires_pkgs = character(0)
  )

  res <- cs_run_single(
    dgp_id       = "synth_baseline",
    estimator_id = "spy_propensity_oracle",
    n            = 50,
    seed         = 2L,
    config       = list(use_true_propensity = TRUE)
  )
  expect_identical(res$meta$oracle_columns_granted, "p")
})

test_that("registry oracle flag alone does not grant raw DGP truth columns", {
  spy_oracle_no_grant <- function(df, config = list(), tau = cs_tau_oracle, ...) {
    forbidden <- c("y0", "y1", "p", "structural_te")
    if (any(forbidden %in% names(df))) {
      stop("Airlock over-granted raw DGP truth columns")
    }
    list(
      att = list(estimate = 0),
      qst = NULL,
      meta = list(estimator_id = "spy_oracle_no_grant", oracle = TRUE, supports_qst = FALSE)
    )
  }

  cs_register_estimator(
    estimator_id  = "spy_oracle_no_grant",
    type          = "spy",
    generator     = spy_oracle_no_grant,
    oracle        = TRUE,
    supports_qst  = FALSE,
    version       = "0.0.0",
    description   = "Spy oracle estimator without grant for airlock test",
    source        = "test",
    requires_pkgs = character(0)
  )

  res <- cs_run_single(
    dgp_id       = "synth_baseline",
    estimator_id = "spy_oracle_no_grant",
    n            = 50,
    seed         = 3L
  )
  expect_identical(res$meta$oracle_columns_granted, character(0))
})

test_that("oracle_att receives only structural_te and records grant metadata", {
  res <- cs_run_single(
    dgp_id       = "synth_baseline",
    estimator_id = "oracle_att",
    n            = 50,
    seed         = 4L
  )
  expect_identical(res$meta$oracle_columns_granted, "structural_te")
  expect_true(isTRUE(res$meta$success))
})

test_that("airlock fails closed on ineligible oracle column requests", {
  spy_propensity_only <- function(df, config = list(), tau = cs_tau_oracle, ...) {
    list(
      att = list(estimate = 0),
      qst = NULL,
      meta = list(estimator_id = "spy_propensity_only", oracle = TRUE, supports_qst = FALSE)
    )
  }

  cs_register_estimator(
    estimator_id  = "spy_propensity_only",
    type          = "spy",
    generator     = spy_propensity_only,
    oracle        = TRUE,
    oracle_columns = "p",
    supports_qst  = FALSE,
    version       = "0.0.0",
    description   = "Spy propensity-only oracle estimator for airlock test",
    source        = "test",
    requires_pkgs = character(0)
  )

  expect_error(
    cs_run_single(
      dgp_id       = "synth_baseline",
      estimator_id = "spy_propensity_only",
      n            = 50,
      seed         = 5L,
      config       = list(use_structural_te = TRUE)
    ),
    class = "causalstress_airlock_error"
  )
})

test_that("airlock still enforced when rerunning with pins/board", {
  spy_non_oracle <- function(df, config = list(), tau = cs_tau_oracle, ...) {
    forbidden <- c("y0", "y1", "p", "structural_te")
    if (any(forbidden %in% names(df))) {
      stop("Airlock failure: forbidden column visible in non-oracle estimator (pins)")
    }
    list(
      att = list(estimate = 0),
      qst = NULL,
      meta = list(estimator_id = "spy_non_oracle_pins", oracle = FALSE, supports_qst = FALSE)
    )
  }

  cs_register_estimator(
    estimator_id  = "spy_non_oracle_pins",
    type          = "spy",
    generator     = spy_non_oracle,
    oracle        = FALSE,
    supports_qst  = FALSE,
    version       = "0.0.0",
    description   = "Spy non-oracle estimator for airlock pins test",
    source        = "test",
    requires_pkgs = character(0)
  )

  board <- pins::board_temp()
  # first run (persists)
  expect_no_error(
    cs_run_grid(
      dgp_ids       = "synth_baseline",
      estimator_ids = "spy_non_oracle_pins",
      n             = 50,
      seeds         = 1:2,
      bootstrap     = FALSE,
      board         = board
    )
  )
  # rerun same seeds without skip_existing to force estimator call again
  expect_no_error(
    cs_run_grid(
      dgp_ids       = "synth_baseline",
      estimator_ids = "spy_non_oracle_pins",
      n             = 50,
      seeds         = 1:2,
      bootstrap     = FALSE,
      board         = board,
      skip_existing = FALSE
    )
  )
})
