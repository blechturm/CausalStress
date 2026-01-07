#' Consolidate staged batch artifacts into a pins board
#'
#' Scans a staging directory for batch artifacts written by `cs_run_batch()`,
#' validates the schema, and writes each batch into the pins board as a
#' `batch_{id}` pin. Idempotent: if a batch pin already exists, it is skipped.
#'
#' @param staging_dir Directory containing staged `.qs` batch artifacts.
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

  files <- list.files(staging_dir, pattern = "\\.qs$", full.names = TRUE)
  if (length(files) == 0L) {
    return(invisible(0L))
  }

  n_ok <- 0L
  for (path in files) {
    batch_obj <- try(qs::qread(path), silent = TRUE)
    if (inherits(batch_obj, "try-error")) {
      warning("Failed to read batch artifact: ", path)
      next
    }
    if (is.null(batch_obj$schema_version)) {
      warning("Missing schema_version in batch artifact: ", path)
      next
    }
    batch_id <- batch_obj$meta$batch_id %||% NA_integer_
    if (is.na(batch_id)) {
      warning("Missing batch_id in batch artifact: ", path)
      next
    }

    pin_name <- paste0("batch_", batch_id)
    if (pins::pin_exists(board, pin_name)) {
      warning("Batch pin already exists, skipping: ", pin_name)
      next
    }

    if (!is.list(batch_obj$results)) {
      warning("Batch results missing or invalid: ", path)
      next
    }
    if (length(batch_obj$results) > 0L) {
      has_tau_id <- vapply(
        batch_obj$results,
        function(res) {
          if (is.null(res$qst)) {
            return(TRUE)
          }
          "tau_id" %in% names(res$qst)
        },
        logical(1)
      )
      if (any(!has_tau_id)) {
        warning("Batch results missing tau_id for qst: ", path)
        next
      }
    }

    node_info <- batch_obj$meta$node_info %||% NULL
    node_name <- NA_character_
    if (!is.null(node_info)) {
      if (is.list(node_info)) {
        node_name <- node_info$nodename %||% NA_character_
      } else if (is.atomic(node_info)) {
        node_name <- node_info[["nodename"]] %||% NA_character_
      }
    }
    n_tasks <- length(batch_obj$results)

    suppressMessages(
      pins::pin_write(
        board = board,
        x = batch_obj,
        name = pin_name,
        type = "qs",
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
