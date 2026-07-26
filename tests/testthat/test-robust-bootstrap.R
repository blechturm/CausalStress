skip_on_cran()

# Flaky estimator that fails ~20% of the time in bootstrap calls

test_that("bootstrap instability triggers NA CIs and warning", {
  skip_if_not_installed("future")

  flaky_env <- new.env(parent = emptyenv())
  flaky_env$main_done <- FALSE
  flaky_env$counter <- 0

  flaky_estimator <- function(df, config = list(), tau = cs_tau_oracle, ...) {
    if (is.null(config$ci_method)) config$ci_method <- "bootstrap"
    if (is.null(config$n_boot)) config$n_boot <- 20
    if (is.null(config$seed)) stop("seed required")

    stat_fn <- function(df_boot) {
      if (runif(1) < 0.75) 0 else stop("flaky failure")
    }

    ci_lo <- NA_real_
    ci_hi <- NA_real_
    n_ok <- 0L
    n_fail <- 0L
    ci_fail_code <- NA_character_
    ci_valid <- NA
    if (identical(config$ci_method, "bootstrap")) {
      ci_res <- cs_bootstrap_ci(stat_fn, df, n_boot = config$n_boot, seed = config$seed)
      ci_lo <- if (length(ci_res$ci_lo) > 0) ci_res$ci_lo[1] else NA_real_
      ci_hi <- if (length(ci_res$ci_hi) > 0) ci_res$ci_hi[1] else NA_real_
      n_ok <- if (length(ci_res$meta$n_boot_ok) > 0) ci_res$meta$n_boot_ok[1] else 0L
      n_fail <- if (length(ci_res$meta$n_boot_fail) > 0) ci_res$meta$n_boot_fail[1] else 0L
      ci_fail_code <- ci_res$meta$ci_fail_code %||% NA_character_
      ci_valid <- ci_res$meta$ci_valid %||% NA
    }

    list(
      att = list(estimate = 0, ci_lo = ci_lo, ci_hi = ci_hi),
      qst = NULL,
      meta = list(
        estimator_id = "flaky_boot",
        oracle = FALSE,
        supports_qst = FALSE,
        ci_method = config$ci_method,
        n_boot_ok = n_ok,
        n_boot_fail = n_fail,
        ci_valid = ci_valid,
        ci_fail_code = ci_fail_code
      )
    )
  }

  # Register temporarily
  cs_register_estimator(
    estimator_id  = "flaky_boot",
    type          = "gcomp",
    generator     = flaky_estimator,
    oracle        = FALSE,
    supports_qst  = FALSE,
    description   = "Flaky bootstrap estimator",
    source        = "test",
    requires_pkgs = character(0)
  )

  # serial plan for reproducibility
  old_plan <- future::plan()
  on.exit(future::plan(old_plan), add = TRUE)
  future::plan(future::sequential)

  expect_warning(
    res <- cs_run_single(
      dgp_id        = "synth_baseline",
      estimator_id  = "flaky_boot",
      n             = 100,
      seed          = 1,
      bootstrap     = TRUE,
      B             = 20,
      parallel      = FALSE,
      show_progress = FALSE
    ),
    class = "causalstress_ci_warning"
  )

  expect_false(res$meta$success)
  expect_match(res$meta$error, "Bootstrap CI failed validation")
  expect_true(is.na(res$att$ci_lo))
  expect_true(res$meta$n_boot_ok < 18)  # below 90% of 20
})
