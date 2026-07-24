cs_validate_batch_artifact <- function(batch_obj,
                                       path,
                                       expected_batch_id = NULL,
                                       expected_task_fingerprints = NULL) {
  abort_invalid <- function(message) {
    rlang::abort(
      paste0(message, ": ", path),
      class = "causalstress_batch_artifact_error"
    )
  }

  if (!is.list(batch_obj) || !identical(batch_obj$schema_version, "v1.0.0")) {
    abort_invalid("Batch artifact has an unsupported or missing schema_version")
  }
  if (!is.list(batch_obj$meta)) {
    abort_invalid("Batch artifact metadata is missing or invalid")
  }

  batch_id <- batch_obj$meta$batch_id
  if (!is.numeric(batch_id) || length(batch_id) != 1L || is.na(batch_id)) {
    abort_invalid("Batch artifact has no valid batch_id")
  }
  batch_id <- as.integer(batch_id)
  if (!is.null(expected_batch_id) && !identical(batch_id, as.integer(expected_batch_id))) {
    abort_invalid("Batch artifact identity does not match its expected batch_id")
  }
  if (!is.list(batch_obj$results)) {
    abort_invalid("Batch results are missing or invalid")
  }
  if (!is.data.frame(batch_obj$errors)) {
    abort_invalid("Batch errors are missing or invalid")
  }

  n_results <- length(batch_obj$results)
  n_errors <- nrow(batch_obj$errors)
  n_tasks <- batch_obj$meta$n_tasks
  if (!is.numeric(n_tasks) || length(n_tasks) != 1L || is.na(n_tasks)) {
    abort_invalid("Batch artifact has no valid task count")
  }
  n_tasks <- as.integer(n_tasks)
  if (!identical(as.integer(n_results + n_errors), n_tasks) ||
      !identical(as.integer(batch_obj$meta$n_results), as.integer(n_results)) ||
      !identical(as.integer(batch_obj$meta$n_errors), as.integer(n_errors)) ||
      !isTRUE(batch_obj$meta$task_count_reconciled)) {
    abort_invalid("Batch task count reconciliation failed")
  }

  result_fingerprints <- vapply(
    batch_obj$results,
    function(res) {
      meta <- res$meta %||% list()
      task_fp <- meta$task_fingerprint %||% NA_character_
      schema <- suppressWarnings(as.integer(meta$config_fingerprint_schema %||% NA_integer_))
      if (length(task_fp) != 1L || is.na(task_fp) || !nzchar(task_fp) ||
          length(schema) != 1L || is.na(schema)) {
        return(NA_character_)
      }
      as.character(task_fp)
    },
    character(1)
  )
  if (anyNA(result_fingerprints)) {
    abort_invalid("Batch results are missing required task identity metadata")
  }

  required_error_cols <- c(
    "task_fingerprint", "config_fingerprint_schema", "error_class", "message"
  )
  missing_error_cols <- setdiff(required_error_cols, names(batch_obj$errors))
  if (length(missing_error_cols) > 0L) {
    abort_invalid(paste(
      "Batch errors are missing required schema columns",
      paste(missing_error_cols, collapse = ", ")
    ))
  }
  error_fingerprints <- as.character(batch_obj$errors$task_fingerprint)
  if (n_errors > 0L) {
    error_schema <- suppressWarnings(as.integer(batch_obj$errors$config_fingerprint_schema))
    if (anyNA(error_fingerprints) || any(!nzchar(error_fingerprints)) || anyNA(error_schema)) {
      abort_invalid("Batch errors contain missing task identity metadata")
    }
  }

  task_fingerprints <- c(result_fingerprints, error_fingerprints)
  if (length(task_fingerprints) != n_tasks || anyDuplicated(task_fingerprints)) {
    abort_invalid("Batch artifact contains missing or duplicate task identities")
  }
  if (!is.null(expected_task_fingerprints)) {
    expected_task_fingerprints <- as.character(expected_task_fingerprints)
    if (length(expected_task_fingerprints) != n_tasks ||
        anyNA(expected_task_fingerprints) ||
        any(!nzchar(expected_task_fingerprints)) ||
        !identical(sort(task_fingerprints), sort(expected_task_fingerprints))) {
      abort_invalid("Batch task identities do not match the campaign plan")
    }
  }

  has_tau_id <- vapply(
    batch_obj$results,
    function(res) is.null(res$qst) || "tau_id" %in% names(res$qst),
    logical(1)
  )
  if (any(!has_tau_id)) {
    abort_invalid("Batch results are missing tau_id for QST rows")
  }
  invisible(batch_obj)
}

cs_staged_batch_index <- function(staging_dir) {
  cs_abort_legacy_staging(staging_dir)
  paths <- sort(list.files(
    staging_dir,
    pattern = "^batch_[0-9]+.*\\.rds$",
    full.names = TRUE
  ))
  if (length(paths) == 0L) {
    return(list(paths = character(), ids = integer()))
  }

  ids <- vapply(
    paths,
    function(path) as.integer(sub(
      "^batch_([0-9]+).*\\.rds$",
      "\\1",
      basename(path)
    )),
    integer(1)
  )

  duplicate_ids <- unique(ids[duplicated(ids)])
  if (length(duplicate_ids) > 0L) {
    rlang::abort(
      glue::glue(
        "Multiple staged RDS artifacts claim the same batch id(s): {paste(duplicate_ids, collapse = ', ')}"
      ),
      class = "causalstress_batch_artifact_error"
    )
  }
  list(paths = paths, ids = ids)
}

#' Consolidate staged batch artifacts into a pins board
#'
#' Scans a staging directory for batch artifacts written by `cs_run_batch()`,
#' validates the schema, and writes each batch into the pins board as a
#' `batch_{id}` pin. Idempotent: if a batch pin already exists, it is skipped.
#'
#' @param staging_dir Directory containing staged RDS batch artifacts.
#' @param board A pins board where batch pins should be written.
#'
#' @return Invisibly, the number of batches consolidated.
#' @export
cs_consolidate <- function(staging_dir, board) {
  if (is.null(staging_dir) || !nzchar(staging_dir)) {
    stop("staging_dir must be provided.")
  }
  if (!dir.exists(staging_dir)) {
    warning("staging_dir does not exist.")
    return(invisible(0L))
  }
  if (is.null(board)) {
    stop("board must be provided.")
  }

  staged <- cs_staged_batch_index(staging_dir)
  if (length(staged$paths) == 0L) {
    return(invisible(0L))
  }

  n_ok <- 0L
  for (i in seq_along(staged$paths)) {
    path <- staged$paths[[i]]
    batch_id <- staged$ids[[i]]
    batch_obj <- cs_read_rds(path, error_class = "causalstress_batch_artifact_error")
    cs_validate_batch_artifact(batch_obj, path, expected_batch_id = batch_id)

    pin_name <- paste0("batch_", batch_id)
    if (pins::pin_exists(board, pin_name)) {
      warning("Batch pin already exists, skipping: ", pin_name)
      next
    }

    n_tasks <- batch_obj$meta$n_tasks

    node_info <- batch_obj$meta$node_info %||% NULL
    node_name <- NA_character_
    if (!is.null(node_info)) {
      if (is.list(node_info)) {
        node_name <- node_info$nodename %||% NA_character_
      } else if (is.atomic(node_info)) {
        node_name <- node_info[["nodename"]] %||% NA_character_
      }
    }
    suppressMessages(
      pins::pin_write(
        board = board,
        x = batch_obj,
        name = pin_name,
        type = "rds",
        metadata = list(
          type = "batch",
          batch_id = batch_id,
          n_tasks = n_tasks,
          node_name = node_name,
          schema_version = batch_obj$schema_version %||% NA_character_,
          timestamp = as.character(Sys.time())
        )
      )
    )

    file.remove(path)
    n_ok <- n_ok + 1L
  }

  invisible(n_ok)
}
