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

test_that("cs_run_batch forwards strategy tau from task_config", {
  staging_dir <- tempfile("cs_stage_")
  dir.create(staging_dir, recursive = TRUE, showWarnings = FALSE)

  tau_est <- function(df, config = list(), tau = cs_tau_oracle, ...) {
    list(
      att = list(estimate = 0),
      qst = tibble::tibble(tau = tau, value = seq_along(tau)),
      meta = list(estimator_id = "batch_tau_est", oracle = FALSE, supports_qst = TRUE)
    )
  }

  cs_register_estimator(
    estimator_id  = "batch_tau_est",
    type          = "test",
    generator     = tau_est,
    oracle        = FALSE,
    supports_qst  = TRUE,
    version       = "0.0.0",
    description   = "Batch tau forwarding estimator",
    source        = "test",
    requires_pkgs = character(0)
  )

  tasks <- tibble::tibble(
    dgp_id = "synth_baseline",
    estimator_id = "batch_tau_est",
    seed = 1L,
    task_config = list(list(n = 50, tau = c(0.25, 0.75), ci_method = "none")),
    task_fingerprint = "tau_fingerprint",
    config_fingerprint_schema = 3L
  )
  plan <- tibble::tibble(batch_id = 1L, tasks = list(tasks))

  cs_run_batch(batch_id = 1L, plan = plan, staging_dir = staging_dir)

  files <- list.files(staging_dir, pattern = "\\.qs$", full.names = TRUE)
  obj <- qs::qread(files[[1L]])
  expect_equal(obj$results[[1L]]$qst$tau, c(0.25, 0.75))
})

test_that("cs_run_batch records parallel provenance and restores thread env", {
  staging_dir <- tempfile("cs_stage_")
  dir.create(staging_dir, recursive = TRUE, showWarnings = FALSE)

  old_threads <- Sys.getenv("OMP_NUM_THREADS", unset = NA_character_)
  Sys.setenv(OMP_NUM_THREADS = "7")
  on.exit({
    if (is.na(old_threads)) {
      Sys.unsetenv("OMP_NUM_THREADS")
    } else {
      Sys.setenv(OMP_NUM_THREADS = old_threads)
    }
  }, add = TRUE)

  tasks <- tibble::tibble(
    dgp_id = "synth_baseline",
    estimator_id = "lm_att",
    seed = 1L,
    task_config = list(list(n = 50, ci_method = "none")),
    task_fingerprint = "parallel_fingerprint",
    config_fingerprint_schema = 3L
  )
  plan <- tibble::tibble(batch_id = 1L, tasks = list(tasks))

  cs_run_batch(
    batch_id = 1L,
    plan = plan,
    staging_dir = staging_dir,
    parallel = TRUE,
    experimental_parallel = TRUE,
    parallel_backend = "sequential",
    parallel_warning_emitted = TRUE
  )

  expect_equal(Sys.getenv("OMP_NUM_THREADS", unset = NA_character_), "7")

  files <- list.files(staging_dir, pattern = "\\.qs$", full.names = TRUE)
  obj <- qs::qread(files[[1L]])
  expect_true(isTRUE(obj$meta$experimental_parallel))
  expect_true(isTRUE(obj$meta$parallel_warning_emitted))
  expect_equal(obj$meta$parallel_backend, "sequential")
  expect_true(isTRUE(obj$meta$thread_caps_applied))
  expect_true(isTRUE(obj$results[[1L]]$provenance$thread_caps_applied))
  expect_equal(obj$results[[1L]]$provenance$effective_num_threads, 1L)
})
