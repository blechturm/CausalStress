#' Gatekeeper summary for placebo suites
#'
#' Summarise placebo performance for each estimator and flag failures against
#' a coverage threshold. This implements the Article IV gatekeeper protocol so
#' users do not have to hand-roll dplyr chains.
#'
#' @param suite_results Result object from [cs_run_suite()] or [cs_run_grid()].
#' @param threshold Minimum required coverage rate (default 0.90).
#'
#' @return A list with two tibbles:
#' \describe{
#'   \item{verdict}{Estimator-level pass/fail with coverage_rate.}
#'   \item{culprits}{DGP \eqn{\times} estimator pairs that failed the threshold.}
#' }
#' The function also prints a short summary via cli.
#'
#' @export
cs_summarise_gatekeeper <- function(suite_results, threshold = 0.90) {
  tidy <- cs_tidy(suite_results)

  placebo <- tidy %>%
    dplyr::filter(grepl("placebo", .data$dgp_id))

  if (nrow(placebo) == 0) {
    cli::cli_warn("No placebo DGPs found in the provided results.")
    return(list(
      verdict = tibble::tibble(),
      culprits = tibble::tibble()
    ))
  }

  verdict <- placebo %>%
    dplyr::group_by(.data$estimator_id) %>%
    dplyr::summarise(
      coverage_rate = mean(.data$att_covered, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      status = ifelse(.data$coverage_rate >= threshold, "PASS", "FAIL")
    )

  culprits <- placebo %>%
    dplyr::group_by(.data$dgp_id, .data$estimator_id) %>%
    dplyr::summarise(
      dgp_coverage = mean(.data$att_covered, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::filter(.data$dgp_coverage < threshold)

  # Console summary
  purrr::pwalk(verdict, function(estimator_id, coverage_rate, status) {
    msg <- sprintf(
      "%s: coverage = %.2f (threshold %.2f)",
      estimator_id, coverage_rate, threshold
    )
    if (identical(status, "PASS")) {
      cli::cli_alert_success(msg)
    } else {
      cli::cli_alert_danger(msg)
    }
  })

  if (nrow(culprits) > 0) {
    cli::cli_alert_info("Culprits (below threshold):")
    culprits %>%
      dplyr::arrange(.data$estimator_id, .data$dgp_id) %>%
      purrr::pwalk(function(dgp_id, estimator_id, dgp_coverage) {
        cli::cli_text(
          "  - {estimator_id} on {dgp_id}: coverage = {sprintf('%.2f', dgp_coverage)}"
        )
      })
  }

  list(verdict = verdict, culprits = culprits)
}
