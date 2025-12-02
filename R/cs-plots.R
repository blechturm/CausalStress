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
