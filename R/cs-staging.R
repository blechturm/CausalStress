#' Stage a run result to the filesystem (atomic persistence)
#'
#' @param result A result list produced by cs_run_single().
#' @param staging_dir Directory where staged files are written.
#' @return Invisibly, the file path written.
cs_stage_result <- function(result, staging_dir) {
  meta <- result$meta %||% list()
  fname <- glue::glue(
    "result_{meta$dgp_id}_{meta$estimator_id}_{meta$n}_seed{meta$seed}.qs"
  )
  path <- file.path(staging_dir, fname)
  qs::qsave(result, path)
  invisible(path)
}

#' Gather staged results and pin them to a board
#'
#' @param board A pins board.
#' @param staging_dir Directory containing staged .qs files.
#' @return Number of files gathered and pinned.
cs_gather_results <- function(board, staging_dir) {
  files <- list.files(staging_dir, pattern = "\\.qs$", full.names = TRUE)
  if (length(files) == 0L) return(0L)

  for (f in files) {
    res <- qs::qread(f)
    cs_pin_write(board, res)
    unlink(f)
  }
  length(files)
}
