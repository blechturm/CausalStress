#' TMLE estimator (ATE target)
#'
#' Soft-dependency wrapper around `tmle::tmle`. Note: the tmle package targets
#' the ATE; when used in CausalStress (ATT benchmarking), the result may be
#' biased under treatment effect heterogeneity. This wrapper records the
#' estimand target in the metadata and adds a warning.
#'
#' @param df Data frame with outcome `y`, treatment `w`, and covariates.
#' @param config Optional list of parameters forwarded to `tmle::tmle`.
#' @param tau Unused (signature compatibility).
#' @param ... Additional arguments passed to `tmle::tmle`.
#'
#' @return A list compatible with `cs_check_estimator_output()`.
#' @export
est_tmle_att <- function(df, config = list(), tau = cs_tau_oracle, ...) {
  if (!requireNamespace("tmle", quietly = TRUE)) {
    stop("Package 'tmle' needed for this estimator.")
  }
  if (!requireNamespace("SuperLearner", quietly = TRUE)) {
    stop("Package 'SuperLearner' needed for tmle estimator.")
  }

  threads <- config$num_threads %||% 1L
  old_mc <- getOption("mc.cores")
  on.exit(options(mc.cores = old_mc), add = TRUE)
  options(mc.cores = threads)

  Y <- df$y
  A <- df$w
  W <- df[, setdiff(names(df), c("y", "w")), drop = FALSE]

  fit <- tmle::tmle(
    Y      = Y,
    A      = A,
    W      = W,
    family = "gaussian",
    ...
  )

  est <- fit$estimates$ATE$psi %||% NA_real_
  ci   <- fit$estimates$ATE$CI %||% c(NA_real_, NA_real_)

  list(
    att = list(
      estimate = as.numeric(est),
      ci_lo    = as.numeric(ci[1]),
      ci_hi    = as.numeric(ci[2])
    ),
    qst = NULL,
    meta = list(
      estimator_id      = "tmle_att",
      oracle            = FALSE,
      supports_qst      = FALSE,
      estimator_pkgs    = c("tmle", "SuperLearner"),
      estimand_target   = "ATE",
      warnings          = "TMLE package targets ATE; result may be biased for ATT if treatment effect is heterogeneous.",
      num_threads       = threads
    )
  )
}
