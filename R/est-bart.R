#' BART ATT estimator (bartCause)
#'
#' Soft-dependency wrapper around `bartCause::bartc` targeting the ATT.
#'
#' @param df Data frame with outcome `y`, treatment `w`, and covariates.
#' @param config Optional list of parameters passed through to `bartCause::bartc`.
#'   Recognised fields: `seed` (numeric scalar).
#' @param tau Unused (included for signature compatibility).
#' @param ... Additional arguments passed to `bartCause::bartc`.
#'
#' @return A list compatible with `cs_check_estimator_output()`.
#' @export
est_bart_att <- function(df, config = list(), tau = cs_tau_oracle, ...) {
  if (!requireNamespace("bartCause", quietly = TRUE)) {
    stop("Package 'bartCause' needed for this estimator.")
  }

  threads <- config$num_threads %||% 1L

  y   <- df$y
  trt <- df$w
  w   <- df[, setdiff(names(df), c("y", "w")), drop = FALSE]

  seed_val <- config$seed
  if (is.null(seed_val)) seed_val <- NA

  fit <- bartCause::bartc(
    response    = y,
    treatment   = trt,
    confounders = w,
    estimand    = "att",
    n.threads   = threads,
    n.chains    = 1L,
    seed        = seed_val,
    ...
  )

  est_tab <- summary(fit)$estimates
  if ("estimand" %in% names(est_tab)) {
    est_row <- est_tab[est_tab$estimand == "att", , drop = FALSE]
  } else {
    est_row <- est_tab[1, , drop = FALSE]
  }

  estimate <- est_row$est %||% est_row$estimate %||% as.numeric(est_row[1])
  ci_lo <- est_row$ci.025 %||% est_row$ci.lower %||% est_row$ci.low %||% NA_real_
  ci_hi <- est_row$ci.975 %||% est_row$ci.upper %||% est_row$ci.high %||% NA_real_

  list(
    att = list(
      estimate = as.numeric(estimate),
      ci_lo    = as.numeric(ci_lo),
      ci_hi    = as.numeric(ci_hi)
    ),
    qst = NULL,
    meta = list(
      estimator_id   = "bart_att",
      oracle         = FALSE,
      supports_qst   = FALSE,
      estimator_pkgs = c("bartCause"),
      num_threads    = threads
    )
  )
}
