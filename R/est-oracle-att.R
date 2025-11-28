#' Oracle ATT estimator using structural treatment effects
#'
#' This estimator uses the structural treatment effect `structural_te`
#' (τ(X)) to compute the ATT on treated units. It is intended only for
#' internal testing and validation, not for real benchmarking.
#'
#' @param df A data.frame or tibble containing at least `w` and
#'   `structural_te`. Typically this is `dgp$df` from a synthetic DGP.
#' @param tau Numeric vector of quantile levels. Ignored here but kept for
#'   interface compatibility with the estimator contract.
#' @param config List of estimator-specific settings. Ignored here except
#'   for being stored in the returned metadata.
#'
#' @return A list with components `att`, `qst`, `cf`, and `meta`, satisfying
#'   the estimator contract used in CausalStress.
#'
#' @export
est_oracle_att <- function(df, tau = cs_tau_oracle, config = list()) {
  if (!is.data.frame(df)) {
    rlang::abort(
      message = "`df` must be a data.frame with columns `w` and `structural_te`.",
      class   = "causalstress_estimator_error"
    )
  }
  if (!all(c("w", "structural_te") %in% names(df))) {
    rlang::abort(
      message = "`df` must contain columns `w` and `structural_te`.",
      class   = "causalstress_estimator_error"
    )
  }

  w <- df$w
  tau_x <- df$structural_te

  if (!(is.numeric(w) || is.integer(w)) || !all(w %in% c(0, 1))) {
    rlang::abort(
      message = "`w` must be numeric/integer and contain only 0/1.",
      class   = "causalstress_estimator_error"
    )
  }
  if (!is.numeric(tau_x) || anyNA(tau_x)) {
    rlang::abort(
      message = "`structural_te` must be numeric and contain no missing values.",
      class   = "causalstress_estimator_error"
    )
  }

  att_hat <- mean(tau_x[w == 1])

  res <- list(
    att = list(
      estimate = att_hat
    ),
    qst = NULL,
    cf  = NULL,
    meta = list(
      estimator_id = "oracle_att",
      version      = "0.0.1",
      capabilities = c("att"),
      target_level = "population",
      config       = config,
      warnings     = character(),
      errors       = character(),
      oracle       = TRUE
    )
  )

  cs_check_estimator_output(res, require_qst = FALSE)

  res
}
