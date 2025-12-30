#' Run a single batch from a campaign plan
#'
#' Executes all tasks for a given batch id and writes a batch artifact to the
#' staging directory. Workers never touch pins.
#'
#' @param batch_id Integer batch id to run.
#' @param plan A tibble from `cs_plan_campaign()`.
#' @param staging_dir Directory to write the batch artifact.
#'
#' @return The path to the staged `.qs` file (invisible).
#' @export
cs_run_batch <- function(batch_id, plan, staging_dir) {
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

  tasks <- plan$tasks[[idx]]
  results <- list()
  error_rows <- list()

  for (i in seq_len(nrow(tasks))) {
    task <- tasks[i, , drop = FALSE]
    dgp_id <- task[["dgp_id"]][[1]]
    estimator_id <- task[["estimator_id"]][[1]]
    seed <- task[["seed"]][[1]]
    task_config <- task[["task_config"]][[1]]

    n_val <- NULL
    if ("n" %in% names(task)) {
      n_val <- task[["n"]][[1]]
    }
    if (is.null(n_val) && !is.null(task_config)) {
      n_val <- task_config$n %||% NULL
    }
    if (is.null(n_val)) {
      error_rows[[length(error_rows) + 1L]] <- tibble::tibble(
        seed = seed,
        dgp_id = dgp_id,
        estimator_id = estimator_id,
        message = "Task missing n (provide task$n or task_config$n).",
        traceback = NA_character_,
        timestamp = as.character(Sys.time())
      )
      next
    }

    tryCatch(
      {
        CausalStress::cs_set_rng(seed)
        cs_enforce_threads(1L)

        cs_get_dgp(dgp_id)
        cs_get_estimator(estimator_id)

        boot_flag <- if (!is.null(task_config)) task_config$bootstrap %||% FALSE else FALSE
        B_val <- if (!is.null(task_config)) task_config$B %||% 0L else 0L

        res <- cs_run_single(
          dgp_id       = dgp_id,
          estimator_id = estimator_id,
          n            = n_val,
          seed         = seed,
          tau          = cs_tau_oracle,
          bootstrap    = boot_flag,
          B            = B_val,
          config       = task_config %||% list()
        )

        if (!is.null(res$qst) && !"tau_id" %in% names(res$qst)) {
          res$qst$tau_id <- cs_tau_id(res$qst$tau)
        }
        if ("task_fingerprint" %in% names(task)) {
          res$meta$task_fingerprint <- task[["task_fingerprint"]][[1]]
        }
        if ("config_fingerprint_schema" %in% names(task)) {
          res$meta$config_fingerprint_schema <- task[["config_fingerprint_schema"]][[1]]
        }

        results[[length(results) + 1L]] <- res
      },
      error = function(e) {
        tb <- tryCatch(
          paste(utils::capture.output(rlang::trace_back()), collapse = "\n"),
          error = function(...) NA_character_
        )
        error_rows[[length(error_rows) + 1L]] <- tibble::tibble(
          seed = seed,
          dgp_id = dgp_id,
          estimator_id = estimator_id,
          message = conditionMessage(e),
          traceback = tb,
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
      traceback = character(),
      timestamp = character()
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
      git_hash = git_hash
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
