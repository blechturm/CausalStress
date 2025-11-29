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
