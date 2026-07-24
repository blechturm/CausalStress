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
  expect_true(all(c(
    "git_hash", "fit_fingerprint", "score_fingerprints",
    "score_row_fingerprints"
  ) %in% names(md)))
  expect_identical(md$score_fingerprints, res$meta$score_fingerprints)
  expect_identical(md$score_row_fingerprints, res$meta$score_row_fingerprints)

  audit <- cs_audit(board)
  audit_row <- audit[audit$pin_name == pin_names[1], , drop = FALSE]
  expect_equal(nrow(audit_row), 1L)
  expect_identical(audit_row$fit_fingerprints[[1L]], res$meta$fit_fingerprint)
  expect_identical(
    audit_row$score_fingerprints[[1L]],
    res$meta$score_fingerprints
  )
  expect_identical(
    audit_row$score_row_fingerprints[[1L]],
    res$meta$score_row_fingerprints
  )
  expect_identical(
    audit_row$scores[[1L]]$score_row_fingerprint,
    res$scores$score_row_fingerprint
  )
})

test_that("Stage & Gather workflow", {
  skip_if_not_installed("pins")

  staging_dir <- tempfile("cs_stage_demo_")
  dir.create(staging_dir, showWarnings = FALSE, recursive = TRUE)
  board <- pins::board_folder(file.path(staging_dir, "board"))

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
  expect_match(path, "\\.rds$")
  expect_identical(readRDS(path), res)

  gathered <- CausalStress:::cs_gather_results(board, staging_dir)
  expect_equal(gathered, 1L)
  expect_false(file.exists(path))
  expect_true(cs_pin_exists(board, res$meta$dgp_id, res$meta$estimator_id, res$meta$n, res$meta$seed))
})
