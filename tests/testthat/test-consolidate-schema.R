test_that("cs_consolidate rejects new artifacts with missing result fingerprints", {
  skip_if_not_installed("pins")
  skip_if_not_installed("qs")

  staging_dir <- tempfile("cs_stage_schema_")
  dir.create(staging_dir, recursive = TRUE, showWarnings = FALSE)

  res <- cs_run_single(
    dgp_id = "synth_baseline",
    estimator_id = "lm_att",
    n = 30,
    seed = 1,
    bootstrap = FALSE,
    config = list(ci_method = "none")
  )
  res$meta$task_fingerprint <- NA_character_
  res$meta$config_fingerprint_schema <- 4L

  batch_obj <- list(
    schema_version = "v1.0.0",
    meta = list(batch_id = 1L, n_tasks = 1L, n_results = 1L, n_errors = 0L),
    results = list(res),
    errors = tibble::tibble(
      seed = integer(),
      dgp_id = character(),
      estimator_id = character(),
      message = character(),
      error_class = character(),
      traceback = character(),
      task_fingerprint = character(),
      config_fingerprint_schema = integer(),
      timestamp = character()
    )
  )
  qs::qsave(batch_obj, file.path(staging_dir, "batch_1_bad.qs"))

  board <- pins::board_temp()
  expect_warning(
    n_ok <- cs_consolidate(staging_dir, board),
    "missing required task fingerprint"
  )
  expect_equal(n_ok, 0L)
  expect_false(pins::pin_exists(board, "batch_1"))
})

test_that("cs_result_to_row propagates batch schema fields", {
  res <- cs_run_single(
    dgp_id = "synth_baseline",
    estimator_id = "lm_att",
    n = 30,
    seed = 1,
    bootstrap = FALSE,
    config = list(ci_method = "none")
  )
  res$meta$task_fingerprint <- "task_fp"
  row <- cs_result_to_row(res)

  expect_true(all(c(
    "dgp_version",
    "estimator_version",
    "config_fingerprint",
    "config_fingerprint_schema",
    "task_fingerprint"
  ) %in% names(row)))
  expect_equal(row$config_fingerprint_schema[[1]], 4L)
  expect_equal(row$task_fingerprint[[1]], "task_fp")
})
