#' Collect ATT-level results from tidy runs
#'
#' Thin helper to subset ATT-level columns from a tibble produced by
#' [cs_tidy()]. Keeps key identification fields, ATT metrics, and
#' bootstrap diagnostics if present.
#'
#' @param tidy A tibble produced by [cs_tidy()].
#'
#' @return A tibble with ATT-level metrics only.
#' @export
cs_collect_att <- function(tidy) {
  if (!is.data.frame(tidy)) {
    tidy <- cs_tidy(tidy)
  }
  dplyr::select(
    tidy,
    dplyr::any_of(c(
      "dgp_id", "estimator_id", "n", "seed",
      "oracle", "oracle_columns_granted", "supports_qst",
      "true_att", "est_att", "att_error", "att_abs_error",
      "att_ci_lo", "att_ci_hi", "att_covered", "att_ci_width",
      "att_ci_method", "att_ci_type", "att_ci_level", "att_ci_valid",
      "att_ci_fail_code", "att_ci_collapsed", "att_ci_valid_by_dim",
      "n_boot_ok", "n_boot_fail"
    ))
  )
}

#' Collect canonical typed score records
#'
#' Converts run results or tidy run rows into the v0.2.0 long-form typed score
#' surface. The legacy ATT/QST collection helpers remain compatibility
#' projections; this helper is the canonical typed score surface.
#'
#' @param x A run result, list of run results, or tibble produced by [cs_tidy()].
#'
#' @return A tibble with one row per scalar score or QST point coordinate.
#' @export
cs_collect_scores <- function(x) {
  if (is.list(x) && !is.null(x$scores) && !is.data.frame(x)) {
    scores <- x$scores
    if (is.null(scores)) {
      return(tibble::tibble())
    }
    return(tibble::as_tibble(scores))
  }

  if (is.list(x) && length(x) > 0L &&
      is.list(x[[1L]]) && !is.null(x[[1L]]$scores)) {
    return(dplyr::bind_rows(lapply(x, cs_collect_scores)))
  }

  if (!is.data.frame(x)) {
    x <- cs_tidy(x)
  } else {
    x <- tibble::as_tibble(x)
  }

  if ("estimand_target_id" %in% names(x) && "score_status" %in% names(x)) {
    return(x)
  }

  if (!"scores" %in% names(x)) {
    return(tibble::tibble())
  }

  dplyr::bind_rows(lapply(x$scores, function(scores) {
    if (is.null(scores)) {
      return(tibble::tibble())
    }
    tibble::as_tibble(scores)
  }))
}

#' Collect QST-level results from tidy runs
#'
#' Thin helper to subset QST-level columns from a tibble produced by
#' [cs_tidy()]. If QST columns are absent, returns an empty tibble with
#' whichever requested columns exist.
#'
#' @param tidy A tibble produced by [cs_tidy()].
#'
#' @return A tibble with QST-level metrics only (one row per tau grid point).
#' @export
cs_collect_qst <- function(tidy) {
  if (!is.data.frame(tidy)) {
    tidy <- cs_tidy(tidy)
  }

  if ("qst" %in% names(tidy)) {
    tidy <- tidyr::unnest(tidy, cols = c(qst), names_repair = "unique")
    dup_idx <- which(grepl("\\.{2}\\d+$", names(tidy)))
    if (length(dup_idx) > 0L) {
      new_names <- names(tidy)
      new_names[dup_idx] <- paste0(
        "qst_",
        seq_along(dup_idx),
        "_",
        sub("\\.{2}\\d+$", "", new_names[dup_idx])
      )
      names(tidy) <- new_names
    }
  }
  if (!"tau_id" %in% names(tidy) && "tau" %in% names(tidy)) {
    tidy <- dplyr::mutate(tidy, tau_id = cs_tau_id(.data$tau))
  }

  dplyr::select(
    tidy,
    dplyr::any_of(c(
      "dgp_id", "estimator_id", "n", "seed", "tau_id", "tau",
      # Standardized column names produced by the runner
      "estimate", "value", "true", "error", "abs_error",
      "ci_lo", "ci_hi", "covered", "ci_width",
      # QST CI provenance (run-level, stored in meta and surfaced by cs_tidy)
      "qst_ci_method", "qst_ci_type", "qst_ci_level", "qst_ci_valid", "qst_ci_fail_code",
      "qst_ci_collapsed", "qst_ci_valid_by_dim",
      # Legacy column names retained for backward compatibility
      "true_qst", "est_qst", "qst_error", "qst_abs_error",
      "qst_ci_lo", "qst_ci_hi", "qst_covered", "qst_ci_width",
      "qst_n_boot_ok", "qst_n_boot_fail", "n_boot_qst_ok"
    ))
  )
}
