# Internal RDS persistence boundary. It is deliberately not a configurable
# codec layer: all package-owned runtime artifacts use base-R serialization.
cs_read_rds <- function(path, error_class = "causalstress_persistence_error") {
  if (!is.character(path) || length(path) != 1L || is.na(path) || !nzchar(path)) {
    rlang::abort("`path` must be a non-empty character scalar.", class = error_class)
  }
  if (!file.exists(path)) {
    rlang::abort(
      glue::glue("RDS artifact does not exist: {path}"),
      class = c(error_class, "causalstress_persistence_error")
    )
  }

  tryCatch(
    readRDS(path),
    error = function(e) {
      rlang::abort(
        glue::glue("Failed to read RDS artifact: {path}"),
        class = c(error_class, "causalstress_persistence_error"),
        parent = e
      )
    }
  )
}

cs_write_rds_atomic <- function(object,
                                path,
                                validate,
                                error_class = "causalstress_persistence_error") {
  if (!is.function(validate)) {
    rlang::abort("`validate` must be a function.", class = error_class)
  }
  validate(object, path)

  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(dirname(path))) {
    rlang::abort(
      glue::glue("Failed to create artifact directory: {dirname(path)}"),
      class = c(error_class, "causalstress_persistence_error")
    )
  }

  validate_existing <- function() {
    existing <- cs_read_rds(path, error_class = error_class)
    validate(existing, path)
    invisible(path)
  }
  if (file.exists(path)) {
    return(validate_existing())
  }

  # Serialize publication by destination so file.rename() never replaces an
  # existing artifact on platforms where rename-overwrite is otherwise allowed.
  lock_dir <- paste0(path, ".lock")
  acquired <- dir.create(lock_dir, showWarnings = FALSE)
  if (!isTRUE(acquired)) {
    for (attempt in seq_len(100L)) {
      if (file.exists(path)) {
        return(validate_existing())
      }
      if (!dir.exists(lock_dir)) {
        acquired <- dir.create(lock_dir, showWarnings = FALSE)
        if (isTRUE(acquired)) break
      }
      Sys.sleep(0.05)
    }
  }
  if (!isTRUE(acquired)) {
    rlang::abort(
      c(
        glue::glue("Could not acquire the artifact write lock: {lock_dir}"),
        "i" = "Another writer may still be active; otherwise inspect the stale lock directory."
      ),
      class = c(error_class, "causalstress_persistence_error")
    )
  }
  on.exit(unlink(lock_dir, recursive = TRUE, force = TRUE), add = TRUE)

  if (file.exists(path)) {
    return(validate_existing())
  }

  tmp <- tempfile(
    pattern = paste0(basename(path), "."),
    tmpdir = dirname(path),
    fileext = ".tmp"
  )
  on.exit(if (file.exists(tmp)) unlink(tmp, force = TRUE), add = TRUE)

  tryCatch(
    saveRDS(object, tmp, version = 3),
    error = function(e) {
      rlang::abort(
        glue::glue("Failed to write temporary RDS artifact for: {path}"),
        class = c(error_class, "causalstress_persistence_error"),
        parent = e
      )
    }
  )
  size <- file.info(tmp)$size
  if (!file.exists(tmp) || length(size) != 1L || is.na(size) || size <= 0L) {
    rlang::abort(
      glue::glue("Temporary RDS artifact is missing or empty for: {path}"),
      class = c(error_class, "causalstress_persistence_error")
    )
  }

  if (!isTRUE(file.rename(tmp, path))) {
    if (file.exists(path)) {
      return(validate_existing())
    }
    rlang::abort(
      glue::glue("Failed to atomically publish RDS artifact: {path}"),
      class = c(error_class, "causalstress_persistence_error")
    )
  }
  invisible(path)
}

cs_abort_legacy_staging <- function(staging_dir) {
  if (!dir.exists(staging_dir)) return(invisible(NULL))

  legacy <- list.files(
    staging_dir,
    pattern = "\\.qs$",
    full.names = TRUE,
    ignore.case = TRUE
  )
  if (length(legacy) > 0L) {
    legacy <- legacy[!file.info(legacy)$isdir]
  }
  if (length(legacy) == 0L) return(invisible(NULL))

  legacy <- sort(legacy)
  rlang::abort(
    c(
      "Legacy `.qs` staging artifacts are incompatible with this release.",
      "x" = paste("Incompatible file(s):", paste(legacy, collapse = ", ")),
      "i" = paste(
        "Preserve the files for historical evidence and rerun the campaign",
        "in a clean staging directory. CausalStress will not read, convert,",
        "delete, or overwrite them."
      )
    ),
    class = "causalstress_legacy_persistence_error"
  )
}

cs_validate_staged_result <- function(result, path, expected = NULL) {
  if (!is.list(result) || !is.list(result$meta)) {
    rlang::abort(
      glue::glue("Staged result has no valid metadata: {path}"),
      class = "causalstress_staging_error"
    )
  }

  identity_fields <- c(
    "dgp_id", "dgp_version", "estimator_id", "estimator_version", "n", "seed",
    "config_fingerprint", "config_fingerprint_schema", "fit_fingerprint",
    "score_fingerprints", "score_row_fingerprints"
  )
  missing <- setdiff(identity_fields, names(result$meta))
  if (length(missing) > 0L) {
    rlang::abort(
      glue::glue(
        "Staged result is missing persistence identity fields ({paste(missing, collapse = ', ')}): {path}"
      ),
      class = "causalstress_staging_error"
    )
  }

  if (!is.null(expected)) {
    if (!is.list(expected) || !is.list(expected$meta)) {
      rlang::abort("Expected staged result metadata is invalid.", class = "causalstress_staging_error")
    }
    actual_identity <- result$meta[identity_fields]
    expected_identity <- expected$meta[identity_fields]
    if (!identical(actual_identity, expected_identity)) {
      rlang::abort(
        glue::glue("Existing staged result has the wrong logical identity: {path}"),
        class = "causalstress_staging_error"
      )
    }
  }
  invisible(result)
}

#' Stage a run result to the filesystem (atomic persistence)
#'
#' @param result A result list produced by cs_run_single().
#' @param staging_dir Directory where staged files are written.
#' @return Invisibly, the file path written.
cs_stage_result <- function(result, staging_dir) {
  meta <- result$meta %||% list()
  fp <- meta$config_fingerprint %||% "nofp"
  fname <- glue::glue(
    "result__dgp={meta$dgp_id}__est={meta$estimator_id}__n={meta$n}__seed={meta$seed}__fp={fp}.rds"
  )
  path <- file.path(staging_dir, fname)

  cs_write_rds_atomic(
    result,
    path,
    validate = function(candidate, candidate_path) {
      cs_validate_staged_result(candidate, candidate_path, expected = result)
    },
    error_class = "causalstress_staging_error"
  )
}

#' Gather staged results and pin them to a board
#'
#' Reads all staged RDS files from a directory (produced by workers using
#' `cs_stage_result`) and writes them to the central board through the
#' controlling process. Successfully pinned files are removed; a failed file
#' remains available for retry.
#'
#' @param board The target pins board.
#' @param staging_dir Path to the directory containing worker outputs.
#'
#' @return Invisibly, the number of results gathered.
#' @export
cs_gather_results <- function(board, staging_dir) {
  cs_abort_legacy_staging(staging_dir)
  files <- list.files(
    staging_dir,
    pattern = "^result__.*\\.rds$",
    full.names = TRUE
  )
  if (length(files) == 0L) return(0L)
  files <- sort(files)

  for (f in files) {
    res <- cs_read_rds(f, error_class = "causalstress_staging_error")
    cs_validate_staged_result(res, f)

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
