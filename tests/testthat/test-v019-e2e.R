test_that("v0.1.9 plan -> batch -> consolidate -> tidy works", {
  skip_on_cran()
  skip_if_not_installed("GenGC")

  plan <- cs_plan_campaign(
    dgp_list = "synth_baseline",
    estimator_list = "gengc",
    n_seeds = 4,
    batch_size = 2,
    campaign_seed = 123,
    strategy_map = list(defaults = list(n = 100, ci_method = "none"))
  )

  staging_dir <- tempfile("cs_v019_staging_")
  dir.create(staging_dir, recursive = TRUE, showWarnings = FALSE)

  cs_run_batch(plan$batch_id[[1]], plan, staging_dir)
  cs_run_batch(plan$batch_id[[2]], plan, staging_dir)

  board <- pins::board_folder(file.path(staging_dir, "board"))
  n_ok <- cs_consolidate(staging_dir, board)
  expect_equal(n_ok, 2L)

  batch_results <- cs_read_batch(board, plan$batch_id[[1]])
  tidy_res <- cs_tidy_batch(batch_results)
  expect_equal(nrow(tidy_res), 2L)

  qst_df <- cs_collect_qst(tidy_res)
  expect_true("tau_id" %in% names(qst_df))

  audit <- cs_audit(board)
  expect_equal(nrow(audit), 2L)
  expect_true("n_tasks" %in% names(audit))
  expect_true(all(c(
    "fit_fingerprints", "score_fingerprints", "score_row_fingerprints", "scores"
  ) %in% names(audit)))
  expect_true(all(vapply(audit$scores, function(scores) {
    all(c("score_fingerprint", "score_row_fingerprint") %in% names(scores))
  }, logical(1))))
})
