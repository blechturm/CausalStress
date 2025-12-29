test_that("experimental parallel applies thread caps inside estimator body", {
  skip_if_not_installed("future")
  skip_if_not_installed("furrr")

  old_plan <- future::plan()
  on.exit(future::plan(old_plan), add = TRUE)
  # Note: sequential plan only exercises the experimental-parallel code path;
  # it does not create true multiprocessing workers.
  future::plan(future::sequential)

  keys <- c("OMP_NUM_THREADS", "MKL_NUM_THREADS", "OPENBLAS_NUM_THREADS", "VECLIB_MAXIMUM_THREADS")

  spy_seen <- new.env(parent = emptyenv())
  spy_seen$val <- NULL

  spy <- function(df, config = list(), tau = cs_tau_oracle, ...) {
    spy_seen$val <- list(
      num_threads = config$num_threads %||% NA_integer_,
      env = Sys.getenv(keys, unset = "__UNSET__")
    )
    list(
      att = list(estimate = 0),
      qst = NULL,
      meta = list(
        estimator_id = "threadcap_spy",
        oracle = FALSE,
        supports_qst = FALSE
      )
    )
  }

  cs_register_estimator(
    estimator_id  = "threadcap_spy",
    type          = "test",
    generator     = spy,
    oracle        = FALSE,
    supports_qst  = FALSE,
    version       = "0.0.0",
    description   = "Thread caps spy estimator",
    source        = "test",
    requires_pkgs = character(0)
  )

  warnings_seen <- list()
  withCallingHandlers(
    cs_run_seeds(
      dgp_id       = "synth_baseline",
      estimator_id = "threadcap_spy",
      n            = 30,
      seeds        = 1:1,
      parallel     = TRUE,
      experimental_parallel = TRUE,
      config       = list(num_threads = 8L),
      show_progress = FALSE,
      quiet = TRUE
    ),
    warning = function(w) {
      warnings_seen <<- c(warnings_seen, list(w))
      invokeRestart("muffleWarning")
    }
  )

  expect_true(any(vapply(warnings_seen, inherits, logical(1), "causalstress_experimental_parallel")))
  expect_true(is.list(spy_seen$val))
  expect_identical(as.integer(spy_seen$val$num_threads), 1L)
  expect_identical(spy_seen$val$env, setNames(rep("1", length(keys)), keys))
})

test_that("serial runs do not mutate thread cap env vars", {
  keys <- c("OMP_NUM_THREADS", "MKL_NUM_THREADS", "OPENBLAS_NUM_THREADS", "VECLIB_MAXIMUM_THREADS")
  before <- Sys.getenv(keys, unset = "__UNSET__")

  spy_seen <- new.env(parent = emptyenv())
  spy_seen$val <- NULL

  spy_serial <- function(df, config = list(), tau = cs_tau_oracle, ...) {
    spy_seen$val <- list(
      num_threads = config$num_threads %||% NA_integer_,
      env = Sys.getenv(keys, unset = "__UNSET__")
    )
    list(
      att = list(estimate = 0),
      qst = NULL,
      meta = list(
        estimator_id = "threadcap_spy_serial",
        oracle = FALSE,
        supports_qst = FALSE
      )
    )
  }

  cs_register_estimator(
    estimator_id  = "threadcap_spy_serial",
    type          = "test",
    generator     = spy_serial,
    oracle        = FALSE,
    supports_qst  = FALSE,
    version       = "0.0.0",
    description   = "Thread caps serial spy estimator",
    source        = "test",
    requires_pkgs = character(0)
  )

  cs_run_seeds(
    dgp_id       = "synth_baseline",
    estimator_id = "threadcap_spy_serial",
    n            = 30,
    seeds        = 1:1,
    parallel     = FALSE,
    experimental_parallel = FALSE,
    config       = list(num_threads = 8L),
    show_progress = FALSE,
    quiet = TRUE
  )

  expect_true(is.list(spy_seen$val))
  expect_identical(as.integer(spy_seen$val$num_threads), 8L)
  expect_identical(spy_seen$val$env, before)

  after <- Sys.getenv(keys, unset = "__UNSET__")
  expect_identical(after, before)
})
