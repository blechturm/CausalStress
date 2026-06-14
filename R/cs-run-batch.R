#' Run a single batch from a campaign plan
#'
#' Executes all tasks for a given batch id and writes a batch artifact to the
#' staging directory. Workers never touch pins.
#'
#' @param batch_id Integer batch id to run.
#' @param plan A tibble from `cs_plan_campaign()`.
#' @param staging_dir Directory to write the batch artifact.
#'
#' @param parallel Logical; `TRUE` when invoked from a parallel planned campaign.
#' @param experimental_parallel Logical; must be `TRUE` when `parallel = TRUE`.
#' @param parallel_backend Character backend label recorded in batch metadata.
#' @param parallel_warning_emitted Logical provenance flag from the parent runner.
#'
#' @return The path to the staged `.qs` file (invisible).
#' @export
cs_run_batch <- function(batch_id,
                         plan,
                         staging_dir,
                         parallel = FALSE,
                         experimental_parallel = FALSE,
                         parallel_backend = NA_character_,
                         parallel_warning_emitted = FALSE) {
  if (is.null(staging_dir) || !nzchar(staging_dir)) {
    stop("staging_dir must be provided.")
  }
  if (!dir.exists(staging_dir)) {
    dir.create(staging_dir, recursive = TRUE, showWarnings = FALSE)
  }

  idx <- which(plan$batch_id == batch_id)
  if (length(idx) != 1L) {
    stop("batch_id not found in plan.")
  }
  cs_require_experimental_parallel(parallel = parallel, experimental_parallel = experimental_parallel)

  tasks <- plan$tasks[[idx]]
  results <- list()
  error_rows <- list()
  n_tasks <- nrow(tasks)

  for (i in seq_len(nrow(tasks))) {
    task <- tasks[i, , drop = FALSE]
    dgp_id <- task[["dgp_id"]][[1]]
    dgp_version <- if ("dgp_version" %in% names(task)) task[["dgp_version"]][[1]] else NULL
    estimator_id <- task[["estimator_id"]][[1]]
    seed <- task[["seed"]][[1]]
    task_config <- task[["task_config"]][[1]]
    task_fingerprint <- if ("task_fingerprint" %in% names(task)) {
      task[["task_fingerprint"]][[1]]
    } else {
      NA_character_
    }
    config_fingerprint_schema <- if ("config_fingerprint_schema" %in% names(task)) {
      task[["config_fingerprint_schema"]][[1]]
    } else {
      NA_integer_
    }

    n_val <- NULL
    if ("n" %in% names(task)) {
      n_val <- task[["n"]][[1]]
    }
    if (is.null(n_val) && !is.null(task_config)) {
      n_val <- task_config$n %||% NULL
    }
    tau_val <- if (!is.null(task_config)) task_config$tau %||% cs_tau_oracle else cs_tau_oracle
    if (is.null(n_val)) {
      error_rows[[length(error_rows) + 1L]] <- tibble::tibble(
        seed = seed,
        dgp_id = dgp_id,
        estimator_id = estimator_id,
        message = "Task missing n (provide task$n or task_config$n).",
        error_class = "causalstress_task_config_error",
        traceback = NA_character_,
        task_fingerprint = task_fingerprint,
        config_fingerprint_schema = config_fingerprint_schema,
        timestamp = as.character(Sys.time())
      )
      next
    }

    tryCatch(
      {
        CausalStress::cs_set_rng(seed)

        run_task_once <- function() {
          cs_get_dgp(dgp_id, version = dgp_version, quiet = FALSE)
          cs_get_estimator(estimator_id)

          boot_flag <- if (!is.null(task_config)) task_config$bootstrap %||% FALSE else FALSE
          B_val <- if (!is.null(task_config)) task_config$B %||% 0L else 0L

          cs_run_single(
            dgp_id       = dgp_id,
            estimator_id = estimator_id,
            n            = n_val,
            seed         = seed,
            version      = dgp_version,
            quiet        = FALSE,
            tau          = tau_val,
            bootstrap    = boot_flag,
            B            = B_val,
            config       = task_config %||% list()
          )
        }

        res <- if (isTRUE(parallel) && isTRUE(experimental_parallel)) {
          cs_with_envvar(cs_thread_caps_env(), run_task_once())
        } else {
          run_task_once()
        }

        if (!is.null(res$qst) && !"tau_id" %in% names(res$qst)) {
          res$qst$tau_id <- cs_tau_id(res$qst$tau)
        }
        res$meta$task_fingerprint <- task_fingerprint
        res$meta$config_fingerprint_schema <- config_fingerprint_schema
        if (!is.null(res$provenance) && isTRUE(parallel) && isTRUE(experimental_parallel)) {
          res$provenance$experimental_parallel <- TRUE
          res$provenance$parallel_warning_emitted <- isTRUE(parallel_warning_emitted)
          res$provenance$parallel_backend <- parallel_backend
          res$provenance$thread_caps_applied <- TRUE
          res$provenance$thread_caps_env <- cs_thread_caps_env()
          res$provenance$effective_num_threads <- 1L
          res$provenance$staging_dir_used <- TRUE
        }

        results[[length(results) + 1L]] <- res
      },
      error = function(e) {
        tb <- tryCatch(
          paste(utils::capture.output(rlang::trace_back()), collapse = "\n"),
          error = function(...) NA_character_
        )
        error_rows[[length(error_rows) + 1L]] <<- tibble::tibble(
          seed = seed,
          dgp_id = dgp_id,
          estimator_id = estimator_id,
          message = conditionMessage(e),
          error_class = class(e)[[1]] %||% NA_character_,
          traceback = tb,
          task_fingerprint = task_fingerprint,
          config_fingerprint_schema = config_fingerprint_schema,
          timestamp = as.character(Sys.time())
        )
        NULL
      }
    )
  }

  errors_tbl <- if (length(error_rows) > 0L) {
    dplyr::bind_rows(error_rows)
  } else {
    tibble::tibble(
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
  }

  n_results <- length(results)
  n_errors <- nrow(errors_tbl)
  if (!identical(n_results + n_errors, n_tasks)) {
    rlang::abort(
      glue::glue(
        "Batch task count reconciliation failed: {n_tasks} planned, ",
        "{n_results} results, {n_errors} errors."
      ),
      class = "causalstress_batch_reconciliation_error"
    )
  }

  git_hash <- tryCatch(
    system("git rev-parse HEAD", intern = TRUE, ignore.stderr = TRUE),
    warning = function(w) NA_character_,
    error   = function(e) NA_character_
  )

  batch_obj <- list(
    schema_version = "v1.0.0",
    meta = list(
      batch_id = batch_id,
      timestamp = as.character(Sys.time()),
      node_info = Sys.info(),
      session_info = list(utils::sessionInfo()),
      git_hash = git_hash,
      n_tasks = n_tasks,
      n_results = n_results,
      n_errors = n_errors,
      task_count_reconciled = TRUE,
      experimental_parallel = isTRUE(experimental_parallel),
      parallel_warning_emitted = isTRUE(parallel_warning_emitted),
      parallel_backend = parallel_backend,
      thread_caps_applied = isTRUE(parallel) && isTRUE(experimental_parallel),
      thread_caps_env = if (isTRUE(parallel) && isTRUE(experimental_parallel)) cs_thread_caps_env() else character(0L)
    ),
    results = results,
    errors = errors_tbl
  )

  uuid <- paste0(
    format(Sys.time(), "%Y%m%d%H%M%OS3"),
    "_",
    Sys.getpid(),
    "_",
    sample.int(1000000L, 1L)
  )
  tmp_path <- file.path(staging_dir, paste0("batch_", batch_id, "_", uuid, ".tmp"))
  final_path <- file.path(staging_dir, paste0("batch_", batch_id, "_", uuid, ".qs"))

  qs::qsave(batch_obj, tmp_path)
  file.rename(tmp_path, final_path)
  invisible(final_path)
}
