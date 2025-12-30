test_that("cs_run_batch writes staged artifact with results and errors", {
  staging_dir <- tempfile("cs_stage_")
  dir.create(staging_dir, recursive = TRUE, showWarnings = FALSE)

  tasks <- tibble::tibble(
    dgp_id = c("synth_baseline", "synth_baseline"),
    estimator_id = c("lm_att", "lm_att"),
    seed = c(1L, 2L),
    task_config = list(
      list(n = 50, ci_method = "none"),
      list()
    ),
    task_fingerprint = c("ok_fingerprint", "bad_fingerprint"),
    config_fingerprint_schema = c(2L, 2L)
  )

  plan <- tibble::tibble(batch_id = 1L, tasks = list(tasks))

  path <- cs_run_batch(batch_id = 1L, plan = plan, staging_dir = staging_dir)

  files <- list.files(staging_dir, pattern = "\\.qs$", full.names = TRUE)
  expect_true(length(files) == 1L)

  obj <- qs::qread(files[[1L]])
  expect_true(is.list(obj))
  expect_true(is.list(obj$results))
  expect_true(is.data.frame(obj$errors))

  expect_equal(length(obj$results), 1L)
  expect_equal(nrow(obj$errors), 1L)

  res <- obj$results[[1L]]
  expect_true(is.list(res$att))
  expect_true(is.list(res$meta))
  expect_true(is.null(res$qst) || "tau_id" %in% names(res$qst))
  expect_true(all(c("seed", "dgp_id", "estimator_id", "message", "traceback") %in% names(obj$errors)))
})
