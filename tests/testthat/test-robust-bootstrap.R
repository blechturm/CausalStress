skip_on_cran()

# Flaky estimator that fails ~20% of the time in bootstrap calls

test_that("bootstrap instability triggers NA CIs and warning", {
  skip_if_not_installed("future")

  flaky_env <- new.env(parent = emptyenv())
  flaky_env$main_done <- FALSE
  flaky_env$counter <- 0

  flaky_estimator <- function(df, config = list(), tau = cs_tau_oracle, ...) {
    # ensure main (first) call always succeeds
    if (isTRUE(flaky_env$main_done)) {
      flaky_env$counter <- flaky_env$counter + 1
      if (flaky_env$counter <= 15) {
        stop("flaky failure")
      }
    } else {
      flaky_env$main_done <- TRUE
    }
    list(
      att = list(estimate = 0),
      qst = NULL,
      meta = list(estimator_id = "flaky_boot", oracle = FALSE, supports_qst = FALSE)
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
  future::plan(future::sequential)
  on.exit(future::plan(future::sequential), add = TRUE)

  res <- cs_run_single(
    dgp_id        = "synth_baseline",
    estimator_id  = "flaky_boot",
    n             = 100,
    seed          = 1,
    bootstrap     = TRUE,
    B             = 20,
    parallel      = FALSE,
    show_progress = FALSE
  )

  expect_true(res$meta$success)
  expect_true(is.na(res$att$ci_lo))
  expect_true(any(grepl("Bootstrap instability", res$meta$warnings)))
  expect_true(res$meta$n_boot_ok < 18)  # below 90% of 20
})
