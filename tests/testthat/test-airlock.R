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

test_that("airlock allows forbidden columns for oracle estimators", {
  spy_oracle <- function(df, config = list(), tau = cs_tau_oracle, ...) {
    if (!all(c("y0", "y1") %in% names(df))) {
      stop("Oracle columns not visible for oracle estimator")
    }
    list(
      att = list(estimate = 0),
      qst = NULL,
      meta = list(estimator_id = "spy_oracle", oracle = TRUE, supports_qst = FALSE)
    )
  }

  cs_register_estimator(
    estimator_id  = "spy_oracle",
    type          = "spy",
    generator     = spy_oracle,
    oracle        = TRUE,
    supports_qst  = FALSE,
    version       = "0.0.0",
    description   = "Spy oracle estimator for airlock test",
    source        = "test",
    requires_pkgs = character(0)
  )

  expect_no_error(
    cs_run_single(
      dgp_id       = "synth_baseline",
      estimator_id = "spy_oracle",
      n            = 50,
      seed         = 2L
    )
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
