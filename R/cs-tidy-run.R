#' Convert a single CausalStress run to a one-row tibble
#'
#' `cs_tidy_run()` is a convenience helper that flattens the rich
#' list returned by [cs_run_single()] into a one-row tibble, using
#' the same column structure as [cs_run_seeds()] and [cs_run_grid()].
#'
#' This is useful when:
#' - you call [cs_run_single()] interactively for debugging or small experiments,
#' - and want results that are directly comparable to multi-seed campaigns.
#'
#' @param result A list as returned by [cs_run_single()].
#'
#' @return A tibble with a single row and the same columns as
#'   [cs_run_seeds()], e.g.:
#'   `dgp_id`, `estimator_id`, `n`, `seed`, `oracle`, `supports_qst`,
#'   `true_att`, `est_att`, `att_error`, `att_abs_error`,
#'   `att_ci_lo`, `att_ci_hi`, `boot_covered`, `n_boot_ok`,
#'   `estimator_pkgs`, `log`, etc.
#'
#' @export
cs_tidy_run <- function(result) {
  cs_result_to_row(result)
}
