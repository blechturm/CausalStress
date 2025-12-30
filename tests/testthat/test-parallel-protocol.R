test_that("parallel persistence requires staging_dir (seeds + campaign)", {
  skip_if_not_installed("pins")

  board <- pins::board_temp()

  expect_error(
    cs_run_seeds(
      dgp_id       = "synth_baseline",
      estimator_id = "lm_att",
      n            = 50,
      seeds        = 1:1,
      board        = board,
      parallel     = TRUE,
      experimental_parallel = TRUE,
      staging_dir  = NULL,
      show_progress = FALSE,
      quiet = TRUE
    ),
    "staging directory"
  )

  expect_error(
    cs_run_campaign(
      dgp_ids       = "synth_baseline",
      estimator_ids = "lm_att",
      seeds         = 1:1,
      n             = 50,
      board         = board,
      parallel      = TRUE,
      experimental_parallel = TRUE,
      staging_dir   = NULL,
      show_progress = FALSE,
      quiet = TRUE
    ),
    "staging directory"
  )
})

test_that("parallel + staging_dir uses stage-and-gather persistence", {
  skip_if_not_installed("pins")
  skip_if_not_installed("qs")

  old_plan <- future::plan()
  on.exit(future::plan(old_plan), add = TRUE)
  future::plan(future::sequential)

  board <- pins::board_temp()
  staging_dir <- file.path(tempdir(), "cs_stage_parallel_protocol")
  dir.create(staging_dir, showWarnings = FALSE, recursive = TRUE)

  expect_warning(
    runs <- cs_run_seeds(
      dgp_id       = "synth_baseline",
      estimator_id = "lm_att",
      n            = 50,
      seeds        = 1:2,
      board        = board,
      parallel     = TRUE,
      experimental_parallel = TRUE,
      staging_dir  = staging_dir,
      show_progress = FALSE,
      quiet = TRUE
    ),
    class = "causalstress_experimental_parallel"
  )

  expect_equal(nrow(runs), 2L)
  expect_true(cs_pin_exists(board, "synth_baseline", "lm_att", 50, 1))
  expect_true(cs_pin_exists(board, "synth_baseline", "lm_att", 50, 2))
})

