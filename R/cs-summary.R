#' Summarise Monte Carlo runs for a DGP × estimator combination
#'
#' This helper takes the output of [cs_run_seeds()] (or any tibble with the
#' same columns) and computes Monte Carlo summary statistics for the ATT
#' error. The input is expected to contain one row per seed.
#'
#' @param runs A tibble or data.frame, typically the output of
#'   [cs_run_seeds()], with columns including:
#'   `dgp_id`, `estimator_id`, `n`, `seed`, `oracle`, `supports_qst`,
#'   `true_att`, `est_att`, `att_error`, `att_abs_error`.
#'
#' @return A tibble with one row and summary columns:
#'   - dgp_id, estimator_id, n, oracle, supports_qst
#'   - n_runs: number of seeds / runs
#'   - mean_true_att, mean_est_att
#'   - mean_error, sd_error
#'   - mean_abs_error, max_abs_error
#'
#'   If multiple (dgp_id, estimator_id, n) combinations are present, the
#'   function summarises within each group and returns one row per group.
#'
#' @export
cs_summarise_runs <- function(runs) {
  if (!is.data.frame(runs)) {
    rlang::abort(
      message = "`runs` must be a data.frame or tibble.",
      class   = "causalstress_runner_error"
    )
  }

  required_cols <- c(
    "dgp_id", "estimator_id", "n",
    "oracle", "supports_qst",
    "true_att", "est_att",
    "att_error", "att_abs_error"
  )

  missing <- setdiff(required_cols, names(runs))
  if (length(missing) > 0L) {
    rlang::abort(
      message = glue::glue(
        "Input to `cs_summarise_runs()` is missing required columns: {toString(missing)}"
      ),
      class   = "causalstress_runner_error"
    )
  }

  runs |>
    dplyr::group_by(
      dgp_id,
      estimator_id,
      n,
      oracle,
      supports_qst
    ) |>
    dplyr::summarise(
      n_runs         = dplyr::n(),
      mean_true_att  = mean(true_att),
      mean_est_att   = mean(est_att),
      mean_error     = mean(att_error),
      sd_error       = stats::sd(att_error),
      mean_abs_error = mean(att_abs_error),
      max_abs_error  = max(att_abs_error),
      .groups = "drop"
    )
}
