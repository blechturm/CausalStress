#' Plot ATT errors by estimator and DGP
#'
#' Produces a boxplot of `att_error` by `estimator_id`, faceted by `dgp_id`.
#' Includes a dashed zero line. Returns a ggplot object.
#'
#' @param tidy_results A tibble produced by [cs_tidy()] or a compatible table
#'   with columns `att_error`, `estimator_id`, and `dgp_id`.
#'
#' @return A ggplot object.
#' @export
cs_plot_att_error <- function(tidy_results) {
  if (nrow(tidy_results) == 0L) {
    rlang::abort("No rows to plot.", class = "causalstress_plot_error")
  }
  ggplot2::ggplot(tidy_results, ggplot2::aes(x = estimator_id, y = att_error)) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    ggplot2::geom_boxplot(alpha = 0.5) +
    ggplot2::facet_wrap(~dgp_id, scales = "free_x") +
    ggplot2::labs(x = "Estimator", y = "ATT error") +
    ggplot2::theme_minimal()
}

#' Plot placebo results (sharp-null checks)
#'
#' Filters placebo DGPs and plots `est_att` with confidence intervals, colored
#' by coverage, faceted by estimator. Returns a ggplot object.
#'
#' @param tidy_results A tibble produced by [cs_tidy()] or similar, containing
#'   `dgp_id`, `estimator_id`, `est_att`, `att_ci_lo`, `att_ci_hi`,
#'   `att_covered`.
#'
#' @return A ggplot object.
#' @export
cs_plot_placebo <- function(tidy_results) {
  placebo <- tidy_results[grepl("placebo", tidy_results$dgp_id, fixed = FALSE), ]
  if (nrow(placebo) == 0L) {
    rlang::abort("No placebo rows to plot.", class = "causalstress_plot_error")
  }
  ggplot2::ggplot(
    placebo,
    ggplot2::aes(
      x = dgp_id,
      y = est_att,
      ymin = att_ci_lo,
      ymax = att_ci_hi,
      color = att_covered
    )
  ) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    ggplot2::geom_pointrange(alpha = 0.7, position = ggplot2::position_dodge(width = 0.5)) +
    ggplot2::facet_wrap(~estimator_id, scales = "free_y") +
    ggplot2::coord_flip() +
    ggplot2::scale_color_manual(values = c("FALSE" = "red", "TRUE" = "blue")) +
    ggplot2::labs(x = "Placebo DGP", y = "Estimated ATT", color = "Covered") +
    ggplot2::theme_minimal()
}

#' Plot QST curves with confidence bands and truth overlay
#'
#' @param tidy_results Tibble (e.g., from [cs_collect_qst()]) with columns
#'   `tau`, `estimate`, optional `ci_lo`/`ci_hi`, optional `true` or `true_qst`,
#'   and identifiers `dgp_id`, `estimator_id`.
#'
#' @return A ggplot object.
#' @export
cs_plot_qst <- function(tidy_results) {
  if (!"tau" %in% names(tidy_results) || !"estimate" %in% names(tidy_results)) {
    rlang::abort("Input must contain columns `tau` and `estimate`.", class = "causalstress_plot_error")
  }

  df <- tidy_results
  if ("true_qst" %in% names(df) && !"true" %in% names(df)) {
    df <- dplyr::rename(df, true = .data$true_qst)
  }
  has_truth <- "true" %in% names(df)
  has_ci <- all(c("ci_lo", "ci_hi") %in% names(df)) && any(!is.na(df$ci_lo) | !is.na(df$ci_hi))

  p <- ggplot2::ggplot(df, ggplot2::aes(x = tau, y = estimate, color = estimator_id))

  if (has_ci) {
    p <- p +
      ggplot2::geom_ribbon(
        ggplot2::aes(ymin = ci_lo, ymax = ci_hi, fill = estimator_id),
        alpha = 0.2,
        color = NA
      )
  }

  p <- p + ggplot2::geom_line(linewidth = 1)

  if (has_truth) {
    p <- p +
      ggplot2::geom_line(
        ggplot2::aes(x = tau, y = true),
        color = "black",
        linetype = "dashed",
        inherit.aes = FALSE
      )
  }

  p +
    ggplot2::facet_grid(dgp_id ~ estimator_id, scales = "free_y") +
    ggplot2::labs(x = "Tau", y = "QST", color = "Estimator", fill = "Estimator") +
    ggplot2::theme_minimal()
}
