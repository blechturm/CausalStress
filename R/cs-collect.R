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
      "n_boot_ok"
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
    tidy <- tidyr::unnest(tidy, cols = c(qst))
  }

  dplyr::select(
    tidy,
    dplyr::any_of(c(
      "dgp_id", "estimator_id", "n", "seed", "tau",
      # Standardized column names produced by the runner
      "estimate", "value", "true", "error", "abs_error",
      "ci_lo", "ci_hi", "covered", "ci_width",
      # Legacy column names retained for backward compatibility
      "true_qst", "est_qst", "qst_error", "qst_abs_error",
      "qst_ci_lo", "qst_ci_hi", "qst_covered", "qst_ci_width",
      "n_boot_qst_ok"
    ))
  )
}
