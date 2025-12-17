#' Stage a run result to the filesystem (atomic persistence)
#'
#' @param result A result list produced by cs_run_single().
#' @param staging_dir Directory where staged files are written.
#' @return Invisibly, the file path written.
cs_stage_result <- function(result, staging_dir) {
  meta <- result$meta %||% list()
  fp <- meta$config_fingerprint %||% "nofp"
  fname <- glue::glue(
    "result__dgp={meta$dgp_id}__est={meta$estimator_id}__n={meta$n}__seed={meta$seed}__fp={fp}.qs"
  )
  path <- file.path(staging_dir, fname)

  if (file.exists(path)) {
    return(invisible(path))
  }

  dir.create(staging_dir, recursive = TRUE, showWarnings = FALSE)
  tmp <- tempfile(pattern = paste0(fname, "."), tmpdir = staging_dir, fileext = ".tmp")

  qs::qsave(result, tmp)
  ok <- file.rename(tmp, path)
  if (!isTRUE(ok)) {
    # best-effort cleanup; on Windows rename can fail if file exists
    if (file.exists(tmp)) unlink(tmp)
    cli::cli_abort("Failed to atomically stage result to {path}.")
  }
  invisible(path)
}

#' Gather staged results and pin them to a board
#'
#' Reads all staged files (`.qs` format) from a directory (produced by workers
#' using `cs_stage_result`) and writes them to the central board. This
#' implements the "Reduce" step of the "Map-Reduce" parallel pattern.
#'
#' @param board The target pins board.
#' @param staging_dir Path to the directory containing worker outputs.
#'
#' @return Invisibly, the number of results gathered.
#' @export
cs_gather_results <- function(board, staging_dir) {
  files <- list.files(staging_dir, pattern = "\\.qs$", full.names = TRUE)
  if (length(files) == 0L) return(0L)
  files <- sort(files)

  for (f in files) {
    res <- tryCatch(
      qs::qread(f),
      error = function(e) {
        cli::cli_abort(
          c(
            "Failed to read staged result file: {f}",
            "i" = "Leaving the file in place for manual inspection/retry.",
            "x" = conditionMessage(e)
          )
        )
      }
    )

    tryCatch(
      {
        cs_pin_write(board, res)
        unlink(f)
      },
      error = function(e) {
        cli::cli_abort(
          c(
            "Failed to pin staged result from file: {f}",
            "i" = "Leaving the file in place for retry.",
            "x" = conditionMessage(e)
          )
        )
      }
    )
  }
  invisible(length(files))
}
