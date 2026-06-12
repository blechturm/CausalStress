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
  expect_equal(obj$meta$n_tasks, 2L)
  expect_equal(obj$meta$n_results, 1L)
  expect_equal(obj$meta$n_errors, 1L)
  expect_true(obj$meta$task_count_reconciled)

  res <- obj$results[[1L]]
  expect_true(is.list(res$att))
  expect_true(is.list(res$meta))
  expect_true(is.null(res$qst) || "tau_id" %in% names(res$qst))
  expect_equal(res$meta$task_fingerprint, "ok_fingerprint")
  expect_equal(res$meta$config_fingerprint_schema, 2L)
  expect_true(all(c(
    "seed", "dgp_id", "estimator_id", "message", "error_class",
    "traceback", "task_fingerprint", "config_fingerprint_schema"
  ) %in% names(obj$errors)))
  expect_equal(obj$errors$task_fingerprint[[1]], "bad_fingerprint")
  expect_equal(obj$errors$error_class[[1]], "causalstress_task_config_error")
})

test_that("cs_run_batch preserves escaping task errors", {
  staging_dir <- tempfile("cs_stage_")
  dir.create(staging_dir, recursive = TRUE, showWarnings = FALSE)

  tasks <- tibble::tibble(
    dgp_id = "does_not_exist",
    estimator_id = "lm_att",
    seed = 1L,
    task_config = list(list(n = 50, ci_method = "none")),
    task_fingerprint = "unknown_dgp_fingerprint",
    config_fingerprint_schema = 2L
  )

  plan <- tibble::tibble(batch_id = 1L, tasks = list(tasks))

  cs_run_batch(batch_id = 1L, plan = plan, staging_dir = staging_dir)

  files <- list.files(staging_dir, pattern = "\\.qs$", full.names = TRUE)
  obj <- qs::qread(files[[1L]])

  expect_equal(length(obj$results), 0L)
  expect_equal(nrow(obj$errors), 1L)
  expect_equal(obj$meta$n_tasks, 1L)
  expect_equal(obj$meta$n_results, 0L)
  expect_equal(obj$meta$n_errors, 1L)
  expect_true(obj$meta$task_count_reconciled)
  expect_match(obj$errors$message[[1]], "Unknown dgp_id")
  expect_equal(obj$errors$error_class[[1]], "causalstress_registry_error")
  expect_equal(obj$errors$task_fingerprint[[1]], "unknown_dgp_fingerprint")
})
