#' Run a planned campaign with batching
#'
#' Executes the remaining batches from a campaign plan, writing batch artifacts
#' to a staging directory. Supports resume by skipping batch ids already staged
#' or pinned.
#'
#' @param plan A tibble from `cs_plan_campaign()` (one row per batch).
#' @param staging_dir Directory to write batch artifacts.
#' @param board Optional pins board to check for completed batch pins.
#' @param workers Number of parallel workers to use.
#'
#' @return Invisibly, a vector of batch ids executed (may be empty).
#' @noRd
cs_run_campaign_plan <- function(plan,
                                 staging_dir,
                                 board = NULL,
                                 workers = parallel::detectCores() - 1L,
                                 show_progress = TRUE,
                                 experimental_parallel = FALSE) {
  if (is.null(staging_dir) || !nzchar(staging_dir)) {
    stop("staging_dir must be provided.")
  }
  if (!dir.exists(staging_dir)) {
    dir.create(staging_dir, recursive = TRUE, showWarnings = FALSE)
  }
  if (!is.data.frame(plan) || !"batch_id" %in% names(plan) || !"tasks" %in% names(plan)) {
    stop("plan must be a tibble from cs_plan_campaign().")
  }

  staged <- cs_staged_batch_index(staging_dir)
  for (i in seq_along(staged$paths)) {
    path <- staged$paths[[i]]
    batch_id <- staged$ids[[i]]
    plan_index <- which(as.integer(plan$batch_id) == batch_id)
    if (length(plan_index) != 1L) {
      rlang::abort(
        glue::glue("Staged batch is not present exactly once in the campaign plan: {path}"),
        class = "causalstress_batch_artifact_error"
      )
    }
    tasks <- plan$tasks[[plan_index]]
    if (!"task_fingerprint" %in% names(tasks)) {
      rlang::abort(
        "Campaign plan tasks are missing `task_fingerprint`.",
        class = "causalstress_batch_artifact_error"
      )
    }
    batch_obj <- cs_read_rds(path, error_class = "causalstress_batch_artifact_error")
    cs_validate_batch_artifact(
      batch_obj,
      path,
      expected_batch_id = batch_id,
      expected_task_fingerprints = tasks$task_fingerprint
    )
  }
  staged_ids <- staged$ids

  board_ids <- integer(0)
  if (!is.null(board)) {
    pin_names <- pins::pin_list(board)
    batch_pins <- pin_names[grepl("^batch_[0-9]+$", pin_names)]
    if (length(batch_pins) > 0L) {
      board_ids <- as.integer(gsub("^batch_([0-9]+)$", "\\1", batch_pins))
      board_ids <- board_ids[!is.na(board_ids)]
    }
  }

  all_done <- unique(c(staged_ids, board_ids))
  todo_ids <- setdiff(plan$batch_id, all_done)

  message(glue::glue(
    "Total: {length(plan$batch_id)}, Done: {length(all_done)}, Remaining: {length(todo_ids)}"
  ))

  if (length(todo_ids) == 0L) {
    return(invisible(integer(0)))
  }

  if (!is.numeric(workers) || length(workers) != 1L || !is.finite(workers)) {
    stop("workers must be a finite numeric scalar.")
  }
  workers <- as.integer(workers)
  if (workers < 1L) {
    workers <- 1L
  }

  cs_require_experimental_parallel(parallel = TRUE, experimental_parallel = experimental_parallel)
  rlang::warn(
    "Experimental parallel execution enabled for this planned campaign call.",
    class = "causalstress_experimental_parallel"
  )

  old_plan <- future::plan()
  on.exit(future::plan(old_plan), add = TRUE)
  future::plan(future::multisession, workers = workers)
  parallel_backend <- cs_parallel_backend_string()

  # If the caller dynamically registered estimators (e.g., benchmarking variants),
  # propagate those registrations to each worker session.
  registry_extra <- .causalstress_estimator_registry_extra$tbl

  run_batches <- function() {
    p <- if (isTRUE(show_progress)) progressr::progressor(along = todo_ids) else NULL
    furrr::future_walk(
      todo_ids,
      function(id) {
        if (nrow(registry_extra) > 0L) {
          reg <- cs_estimator_registry()
          missing <- registry_extra$estimator_id[!registry_extra$estimator_id %in% reg$estimator_id]
          if (length(missing) > 0L) {
            for (i in seq_len(nrow(registry_extra))) {
              est_id <- registry_extra$estimator_id[[i]]
              if (est_id %in% missing) {
                register_args <- list(
                  estimator_id  = est_id,
                  type          = registry_extra$type[[i]],
                  generator     = registry_extra$generator[[i]],
                  oracle        = registry_extra$oracle[[i]],
                  oracle_columns = registry_extra$oracle_columns[[i]],
                  oracle_default_columns = registry_extra$oracle_default_columns[[i]],
                  supports_qst  = registry_extra$supports_qst[[i]],
                  version       = registry_extra$version[[i]],
                  description   = registry_extra$description[[i]],
                  source        = registry_extra$source[[i]],
                  requires_pkgs = registry_extra$requires_pkgs[[i]]
                )
                register_args <- register_args[names(register_args) %in% names(formals(cs_register_estimator))]
                do.call(cs_register_estimator, register_args)
              }
            }
          }
        }
        batch_args <- list(
          batch_id = id,
          plan = plan,
          staging_dir = staging_dir,
          parallel = TRUE,
          experimental_parallel = experimental_parallel,
          parallel_backend = parallel_backend,
          parallel_warning_emitted = TRUE
        )
        batch_args <- batch_args[names(batch_args) %in% names(formals(cs_run_batch))]
        do.call(cs_run_batch, batch_args)
        if (!is.null(p)) {
          p(message = glue::glue("batch {id} done"))
        }
      },
      .options = furrr::furrr_options(seed = TRUE)
    )
  }

  if (isTRUE(show_progress)) {
    current_handlers <- progressr::handlers(default = NA)
    if (length(current_handlers) == 0L) {
      if (requireNamespace("cli", quietly = TRUE)) {
        progressr::handlers(
          progressr::handler_cli(
            intrusiveness = getOption("progressr.intrusiveness.gui", 1)
          )
        )
      } else {
        progressr::handlers(progressr::handler_txtprogressbar(style = 3))
      }
    }
    progressr::with_progress(run_batches())
  } else {
    run_batches()
  }

  invisible(todo_ids)
}
