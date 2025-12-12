test_that("cs_run_seeds persists rich results to a pins board", {
  skip_if_not_installed("pins")

  board <- pins::board_temp()

  runs <- cs_run_seeds(
    dgp_id       = "synth_baseline",
    estimator_id = "lm_att",
    n            = 100,
    seeds        = 1:3,
    bootstrap    = FALSE,
    B            = 10,
    board        = board
  )

  expect_s3_class(runs, "tbl_df")
  expect_equal(nrow(runs), 3L)

  pin_names <- pins::pin_list(board)
  expect_gt(length(pin_names), 0L)

  res <- pins::pin_read(board, pin_names[1])
  expect_true(is.list(res))
  expect_true(all(c("att", "qst", "boot_draws", "meta") %in% names(res)))
  expect_true(is.list(res$att))
  expect_true(is.list(res$meta))

  meta <- pins::pin_meta(board, pin_names[1])
  md <- if (!is.null(meta$metadata)) meta$metadata else meta$user
  expect_true("git_hash" %in% names(md))
})

test_that("Stage & Gather workflow", {
  skip_if_not_installed("pins")
  skip_if_not_installed("qs")

  board <- pins::board_temp()
  staging_dir <- file.path(tempdir(), "cs_stage_demo")
  dir.create(staging_dir, showWarnings = FALSE, recursive = TRUE)

  res <- cs_run_single(
    dgp_id       = "synth_baseline",
    estimator_id = "lm_att",
    n            = 50,
    seed         = 123,
    bootstrap    = FALSE,
    board        = NULL
  )

  path <- CausalStress:::cs_stage_result(res, staging_dir)
  expect_true(file.exists(path))

  gathered <- CausalStress:::cs_gather_results(board, staging_dir)
  expect_equal(gathered, 1L)
  expect_false(file.exists(path))
  expect_true(cs_pin_exists(board, res$meta$dgp_id, res$meta$estimator_id, res$meta$n, res$meta$seed))
})
