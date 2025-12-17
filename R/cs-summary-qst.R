#' Summarise QST performance across runs
#'
#' Aggregates distributional metrics over seeds for each DGP/estimator/tau.
#'
#' @param runs Tibble of runs (e.g., from [cs_run_seeds()]) containing a `qst`
#'   list-column with per-tau estimates, truth, errors, and coverage.
#'
#' @return Tibble with one row per `dgp_id`/`estimator_id`/`n`/`tau` containing
#'   mean bias, absolute bias, coverage, and mean CI width.
#' @export
cs_summarise_qst <- function(runs) {
  if (!"qst" %in% names(runs)) {
    cli::cli_abort("Input `runs` must contain a `qst` list-column.")
  }

  df <- runs
  if ("qst" %in% names(df)) {
    df <- tidyr::unnest(df, .data$qst, names_repair = "unique")
    dup_idx <- which(grepl("\\.{2}\\d+$", names(df)))
    if (length(dup_idx) > 0L) {
      new_names <- names(df)
      new_names[dup_idx] <- paste0(
        "qst_",
        seq_along(dup_idx),
        "_",
        sub("\\.{2}\\d+$", "", new_names[dup_idx])
      )
      names(df) <- new_names
    }
  }

  if (!"tau_id" %in% names(df) && "tau" %in% names(df)) {
    df$tau_id <- cs_tau_id(df$tau)
  }

  df %>%
    dplyr::group_by(.data$dgp_id, .data$estimator_id, .data$n, .data$tau_id) %>%
    dplyr::summarise(
      tau            = dplyr::first(.data$tau),
      mean_bias      = mean(.data$error, na.rm = TRUE),
      mean_abs_bias  = mean(.data$abs_error, na.rm = TRUE),
      coverage       = mean(.data$covered, na.rm = TRUE),
      mean_ci_width  = mean(.data$ci_width, na.rm = TRUE),
      .groups = "drop"
    )
}
