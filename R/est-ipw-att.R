#' Inverse-probability weighted ATT estimator (IPW-ATT)
#'
#' Implements a simple inverse-probability weighting estimator for the
#' average treatment effect on the treated (ATT). The propensity score
#' is estimated via logistic regression of `w` on covariates, and the
#' ATT is computed using standard ATT-IPW weights.
#'
#' @param df A data.frame or tibble containing at least `y`, `w`, and
#'   covariates whose names start with `"X"`.
#' @param config Optional list of configuration options (currently unused
#'   but included for API symmetry and future extensions).
#' @param tau Numeric vector of quantile levels (ignored; included for
#'   API compatibility).
#' @param ... Ignored. Allows for future extensibility.
#'
#' @return A list with components:
#'   - att: list with an `estimate` field (scalar ATT estimate).
#'   - qst: `NULL` (this estimator does not provide QST).
#'   - meta: list with metadata about the estimator.
#'
#' @export
est_ipw_att <- function(df, config = list(), tau = cs_tau_oracle, ...) {
  if (!is.data.frame(df)) {
    rlang::abort(
      message = "`df` must be a data.frame.",
      class   = "causalstress_estimator_error"
    )
  }
  if (!all(c("y", "w") %in% names(df))) {
    rlang::abort(
      message = "`df` must contain columns `y` and `w`.",
      class   = "causalstress_estimator_error"
    )
  }

  x_cols <- grep("^X", names(df), value = TRUE)
  if (length(x_cols) == 0L) {
    rlang::abort(
      message = "No covariate columns starting with 'X' found in `df`.",
      class   = "causalstress_estimator_error"
    )
  }

  if (!(is.numeric(df$w) || is.integer(df$w)) || !all(df$w %in% c(0, 1))) {
    rlang::abort(
      message = "`w` must be numeric/integer and contain only 0/1.",
      class   = "causalstress_estimator_error"
    )
  }
  if (!is.numeric(df$y)) {
    rlang::abort(
      message = "`y` must be numeric.",
      class   = "causalstress_estimator_error"
    )
  }
  if (anyNA(df[, c("y", "w", x_cols), drop = FALSE])) {
    rlang::abort(
      message = "`y`, `w`, and covariates `X*` must not contain missing values.",
      class   = "causalstress_estimator_error"
    )
  }

  treated <- df$w == 1L
  control <- !treated
  if (!any(control)) {
    rlang::abort(
      message = "No control units available to estimate propensity.",
      class   = "causalstress_estimator_error"
    )
  }
  if (!any(treated)) {
    rlang::abort(
      message = "No treated units available to estimate ATT.",
      class   = "causalstress_estimator_error"
    )
  }

  form <- stats::as.formula(
    paste("w ~", paste(x_cols, collapse = " + "))
  )
  glm_fit <- stats::glm(form, family = stats::binomial(), data = df)
  e_hat <- stats::predict(glm_fit, type = "response")

  y_treated <- df$y[treated]
  y_control <- df$y[control]
  w_control <- e_hat[control] / (1 - e_hat[control])

  mu1_hat <- mean(y_treated)
  mu0_hat <- sum(w_control * y_control) / sum(w_control)
  att_hat <- mu1_hat - mu0_hat

  res <- list(
    att = list(
      estimate = att_hat
    ),
    qst = NULL,
    cf  = NULL,
    meta = list(
      estimator_id = "ipw_att",
      version      = "0.0.1",
      capabilities = c("att"),
      target_level = "population",
      config       = config,
      warnings     = character(),
      errors       = character(),
      oracle       = FALSE,
      supports_qst = FALSE
    )
  )

  cs_check_estimator_output(res, require_qst = FALSE)

  res
}
