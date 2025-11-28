#' Linear outcome regression ATT estimator
#'
#' This estimator fits a linear model for the outcome on control units
#' and then uses g-computation to estimate the ATT. It only uses observed
#' outcomes `y`, treatment indicator `w`, and covariates (columns whose
#' names start with `"X"`).
#'
#' @param df A data.frame or tibble containing at least `y`, `w`, and at
#'   least one covariate column whose name starts with `"X"`. Typically
#'   this is `dgp$df` from a synthetic DGP.
#' @param tau Numeric vector of quantile levels. Included for interface
#'   compatibility; ignored by this estimator.
#' @param config List of estimator-specific settings. Currently unused
#'   but stored in the returned metadata.
#'
#' @return A list with components `att`, `qst`, `cf`, and `meta`,
#'   satisfying the estimator contract used in CausalStress.
#'
#' @export
est_lm_att <- function(df, tau = cs_tau_oracle, config = list()) {
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

  df_ctrl <- df[df$w == 0, , drop = FALSE]
  if (nrow(df_ctrl) < 2L) {
    rlang::abort(
      message = "Not enough control units to fit the outcome model.",
      class   = "causalstress_estimator_error"
    )
  }

  form <- stats::as.formula(
    paste("y ~", paste(x_cols, collapse = " + "))
  )
  fit <- stats::lm(form, data = df_ctrl)

  df_trt <- df[df$w == 1, , drop = FALSE]
  if (nrow(df_trt) == 0L) {
    rlang::abort(
      message = "No treated units available to estimate ATT.",
      class   = "causalstress_estimator_error"
    )
  }

  y1_obs <- df_trt$y
  y0_hat <- stats::predict(fit, newdata = df_trt)
  att_hat <- mean(y1_obs - y0_hat)

  res <- list(
    att = list(
      estimate = att_hat
    ),
    qst = NULL,
    cf  = NULL,
    meta = list(
      estimator_id = "lm_att",
      version      = "0.0.1",
      capabilities = c("att"),
      target_level = "population",
      config       = config,
      warnings     = character(),
      errors       = character(),
      oracle       = FALSE
    )
  )

  cs_check_estimator_output(res, require_qst = FALSE)

  res
}
