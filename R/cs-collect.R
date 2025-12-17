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
      "oracle", "supports_qst",
      "true_att", "est_att", "att_error", "att_abs_error",
      "att_ci_lo", "att_ci_hi", "att_covered", "att_ci_width",
      "att_ci_method", "att_ci_type", "att_ci_level", "att_ci_valid",
      "att_ci_fail_code", "att_ci_collapsed", "att_ci_valid_by_dim",
      "n_boot_ok", "n_boot_fail"
    ))
  )
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
