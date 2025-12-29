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

cs_require_experimental_parallel <- function(parallel, experimental_parallel) {
  if (isTRUE(parallel) && !isTRUE(experimental_parallel)) {
    rlang::abort(
      message = "`parallel = TRUE` requires `experimental_parallel = TRUE` (v0.1.x experimental parallel mode).",
      class   = "causalstress_experimental_parallel_error"
    )
  }
  invisible(NULL)
}

cs_parallel_backend_string <- function() {
  strategy <- tryCatch(future::plan(), error = function(...) NULL)
  if (is.null(strategy)) return("unknown")

  first <- strategy
  if (is.list(strategy) && length(strategy) > 0L) {
    first <- strategy[[1L]]
  }
  cls <- class(first)
  if (is.null(cls) || length(cls) == 0L) return("unknown")
  if ("sequential" %in% cls) return("sequential")
  if ("multisession" %in% cls) return("multisession")
  if ("multicore" %in% cls) return("multicore")
  "unknown"
}

cs_thread_caps_env <- function() {
  c(
    OMP_NUM_THREADS = "1",
    MKL_NUM_THREADS = "1",
    OPENBLAS_NUM_THREADS = "1",
    VECLIB_MAXIMUM_THREADS = "1"
  )
}

cs_with_envvar <- function(env, expr) {
  old <- Sys.getenv(names(env), unset = NA_character_)
  # Track which were unset so we can restore accurately.
  was_unset <- is.na(old)
  do.call(Sys.setenv, as.list(env))
  on.exit({
    for (nm in names(env)) {
      if (isTRUE(was_unset[[nm]])) {
        Sys.unsetenv(nm)
      } else {
        do.call(Sys.setenv, as.list(structure(old[[nm]], names = nm)))
      }
    }
  }, add = TRUE)
  force(expr)
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

