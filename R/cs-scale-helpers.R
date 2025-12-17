#' Scale/readiness helpers (internal)
#' @noRd

cs_require_staging_for_parallel_persistence <- function(parallel, board, staging_dir) {
  if (isTRUE(parallel) && !is.null(board) && is.null(staging_dir)) {
    cli::cli_abort(
      c(
        "Parallel execution with persistence requires a staging directory.",
        "i" = "Set `staging_dir` when using `parallel = TRUE` with a non-NULL `board`."
      )
    )
  }
  invisible(NULL)
}

cs_pin_meta_user_or_metadata <- function(meta_obj) {
  meta_obj$metadata %||% meta_obj$user %||% list()
}

cs_has_boot_ci_meta <- function(md) {
  n_ok <- suppressWarnings(as.integer(md$n_boot_ok %||% 0L))
  lo <- md$att_ci_lo %||% NA_real_
  hi <- md$att_ci_hi %||% NA_real_
  if (!is.finite(n_ok) || n_ok <= 0L) return(FALSE)
  if (!is.finite(lo) || !is.finite(hi)) return(FALSE)
  if (lo > hi) return(FALSE)
  TRUE
}

