#' Gatekeeper summary for placebo suites
#'
#' Summarise placebo performance for each estimator and flag failures against
#' a coverage threshold. This implements the Article IV gatekeeper protocol so
#' users do not have to hand-roll dplyr chains.
#'
#' @param suite_results Result object from [cs_run_suite()] or [cs_run_grid()].
#' @param threshold Minimum required coverage rate (default 0.90).
#'
#' @return A list with ATT and QST verdicts/culprits. The function also prints a
#'   short summary via cli.
#'
#' @export
cs_summarise_gatekeeper <- function(suite_results, threshold = 0.90) {
  tidy <- cs_tidy(suite_results)

  placebo <- tidy %>%
    dplyr::filter(grepl("placebo", .data$dgp_id))

  if (nrow(placebo) == 0) {
    cli::cli_warn("No placebo DGPs found in the provided results.")
    return(list(
      att_verdict = tibble::tibble(),
      att_culprits = tibble::tibble(),
      qst_verdict = tibble::tibble(),
      qst_culprits = tibble::tibble()
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

  # QST Gatekeeper (10/10 rule)
  has_qst <- "qst" %in% names(suite_results)
  qst_verdict <- tibble::tibble()
  qst_culprits <- tibble::tibble()

  if (has_qst) {
    qst_placebo <- suite_results %>%
      dplyr::filter(grepl("placebo", .data$dgp_id)) %>%
      dplyr::filter(!purrr::map_lgl(.data$qst, is.null))

    if (nrow(qst_placebo) > 0) {
      qst_placebo <- tidyr::unnest(qst_placebo, "qst")

      run_failures <- qst_placebo %>%
        dplyr::group_by(.data$dgp_id, .data$estimator_id, .data$seed) %>%
        dplyr::summarise(
          n_tau = sum(!is.na(.data$covered)),
          n_fail = sum(!is.na(.data$covered) & .data$covered == FALSE),
          null_rejection_rate = dplyr::if_else(n_tau > 0, n_fail / n_tau, NA_real_),
          run_fail = !is.na(null_rejection_rate) & null_rejection_rate > 0.10,
          .groups = "drop"
        )

      qst_culprits <- run_failures %>%
        dplyr::filter(.data$run_fail)

      qst_verdict <- run_failures %>%
        dplyr::group_by(.data$estimator_id) %>%
        dplyr::summarise(
          run_fail_rate = mean(.data$run_fail, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        dplyr::mutate(
          status = dplyr::case_when(
            is.na(.data$run_fail_rate) ~ "UNVERIFIED",
            .data$run_fail_rate > 0.10 ~ "FAIL",
            TRUE ~ "PASS"
          )
        )
    } else {
      qst_verdict <- tibble::tibble(
        estimator_id = unique(suite_results$estimator_id),
        run_fail_rate = NA_real_,
        status = "UNVERIFIED"
      )
    }
  }

  # Console summary for QST
  if (nrow(qst_verdict) == 0) {
    cli::cli_alert_info("QST Gatekeeper: no QST-capable placebo runs found.")
  } else {
    cli::cli_alert_info("QST Gatekeeper (10/10 rule):")
    purrr::pwalk(qst_verdict, function(estimator_id, run_fail_rate, status) {
      msg <- sprintf(
        "%s: run failure rate = %.2f (threshold 0.10)",
        estimator_id, ifelse(is.na(run_fail_rate), NA_real_, run_fail_rate)
      )
      if (identical(status, "PASS")) {
        cli::cli_alert_success(msg)
      } else if (identical(status, "UNVERIFIED")) {
        cli::cli_alert_warning(paste0(msg, " [UNVERIFIED]"))
      } else {
        cli::cli_alert_danger(msg)
      }
    })

    if (nrow(qst_culprits) > 0) {
      cli::cli_alert_info("QST culprits (runs exceeding 10% null rejection):")
      qst_culprits %>%
        dplyr::arrange(.data$estimator_id, .data$dgp_id, .data$seed) %>%
        purrr::pwalk(function(dgp_id, estimator_id, seed, null_rejection_rate, ...) {
          cli::cli_text(
            "  - {estimator_id} on {dgp_id}, seed {seed}: null rejection rate = {sprintf('%.2f', null_rejection_rate)}"
          )
        })
    }
  }

  list(
    att_verdict = verdict,
    att_culprits = culprits,
    qst_verdict = qst_verdict,
    qst_culprits = qst_culprits
  )
}
