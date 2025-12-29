test_that("parallel=TRUE requires experimental_parallel=TRUE (campaign and seeds)", {
  expect_error(
    cs_run_campaign(
      dgp_ids       = "synth_baseline",
      estimator_ids = "lm_att",
      seeds         = 1:1,
      n             = 30,
      parallel      = TRUE,
      experimental_parallel = FALSE,
      show_progress = FALSE
    ),
    "experimental_parallel"
  )

  expect_error(
    cs_run_seeds(
      dgp_id       = "synth_baseline",
      estimator_id = "lm_att",
      n            = 30,
      seeds        = 1:1,
      parallel     = TRUE,
      experimental_parallel = FALSE,
      show_progress = FALSE
    ),
    "experimental_parallel"
  )
})

test_that("experimental parallel warning is emitted exactly once per call", {
  skip_if_not_installed("future")
  skip_if_not_installed("furrr")
  skip_if_not_installed("pins")

  old_plan <- future::plan()
  on.exit(future::plan(old_plan), add = TRUE)
  # Note: sequential plan only exercises the experimental-parallel code path;
  # it does not create true multiprocessing workers.
  future::plan(future::sequential)

  warnings_seen <- list()
  res_campaign <- withCallingHandlers(
    cs_run_campaign(
      dgp_ids       = "synth_baseline",
      estimator_ids = "lm_att",
      seeds         = 1:2,
      n             = 30,
      parallel      = TRUE,
      experimental_parallel = TRUE,
      show_progress = FALSE
    ),
    warning = function(w) {
      warnings_seen <<- c(warnings_seen, list(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_true(inherits(warnings_seen[[1]], "causalstress_experimental_parallel"))
  expect_equal(sum(vapply(warnings_seen, inherits, logical(1), "causalstress_experimental_parallel")), 1L)
  expect_s3_class(warnings_seen[[1]], "causalstress_experimental_parallel")
  expect_true(is.data.frame(res_campaign))

  board <- pins::board_temp()
  staging_dir <- file.path(tempdir(), "cs_stage_v018_warn_once")
  dir.create(staging_dir, showWarnings = FALSE, recursive = TRUE)

  warnings_seen <- list()
  res_seeds <- withCallingHandlers(
    cs_run_seeds(
      dgp_id       = "synth_baseline",
      estimator_id = "lm_att",
      n            = 30,
      seeds        = 1:1,
      board        = board,
      staging_dir  = staging_dir,
      parallel     = TRUE,
      experimental_parallel = TRUE,
      show_progress = FALSE,
      quiet = TRUE
    ),
    warning = function(w) {
      warnings_seen <<- c(warnings_seen, list(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_true(inherits(warnings_seen[[1]], "causalstress_experimental_parallel"))
  expect_equal(sum(vapply(warnings_seen, inherits, logical(1), "causalstress_experimental_parallel")), 1L)
  expect_true(is.data.frame(res_seeds))

  name <- "results__dgp=synth_baseline__est=lm_att__n=30__seed=1"
  pin <- pins::pin_read(board, name)
  expect_true(isTRUE(pin$provenance$experimental_parallel))
  expect_true(isTRUE(pin$provenance$parallel_warning_emitted))
  expect_type(pin$provenance$parallel_backend, "character")
  expect_length(pin$provenance$parallel_backend, 1L)
})
