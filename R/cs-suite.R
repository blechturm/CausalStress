#' Suite registry
#'
#' Returns the suite registry as a tibble. Suites are defined as collections of
#' DGP identifiers; some suites (e.g., placebo) are computed dynamically from
#' the DGP registry.
#'
#' @return A tibble with columns `suite_id`, `dgp_ids` (list column),
#'   and `description`.
#' @export
cs_suite_registry <- function() {
  reg <- cs_dgp_registry()
  all_dgps <- reg$dgp_id

  placebo_ids <- reg$dgp_id[grepl("placebo", reg$dgp_id, fixed = FALSE)]

  tibble::tibble(
    suite_id = c(
      "placebo",
      "heavytail",
      "signal",
      "stress",
      "all"
    ),
    dgp_ids = list(
      placebo_ids,
      c("synth_heavytail", "synth_placebo_heavytail"),
      c("synth_baseline", "synth_qte1", "synth_nonlinear_heteroskedastic"),
      c("synth_overlap_stressed", "synth_tilt_mild"),
      all_dgps
    ),
    description = c(
      "All placebo (sharp-null) DGPs",
      "Heavy-tail signal and placebo heavy-tail DGPs",
      "Signal-focused DGPs (baseline, QTE heterogeneity, nonlinear heteroskedastic)",
      "Stress-test DGPs (overlap and tilt stressors)",
      "All available synthetic DGPs"
    )
  )
}

#' Get a suite definition
#'
#' @param suite_id Character scalar identifying the suite.
#'
#' @return A character vector of DGP IDs contained in the suite.
#' @export
cs_get_suite <- function(suite_id) {
  reg <- cs_suite_registry()
  row <- reg[reg$suite_id == suite_id, , drop = FALSE]
  if (nrow(row) == 0L) {
    rlang::abort(
      message = glue::glue("Unknown suite_id: {suite_id}"),
      class = "causalstress_registry_error"
    )
  }
  unlist(row$dgp_ids[[1L]], use.names = FALSE)
}

#' Run a suite of DGPs with given estimators
#'
#' @param suite_id Character scalar identifying the suite.
#' @param estimator_ids Character vector of estimator IDs.
#' @param ... Passed through to [cs_run_grid()].
#'
#' @return The tibble returned by [cs_run_grid()].
#' @export
cs_run_suite <- function(suite_id, estimator_ids, seeds = 1:50, n = 2000, ...) {
  dgp_ids <- cs_get_suite(suite_id)
  cs_run_grid(
    dgp_ids       = dgp_ids,
    estimator_ids = estimator_ids,
    seeds         = seeds,
    n             = n,
    ...
  )
}
