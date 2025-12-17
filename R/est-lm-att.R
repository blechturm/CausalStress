#' Linear outcome regression ATT estimator
#'
#' Fits a linear model on controls and uses g-computation to estimate ATT.
#'
#' @export
est_lm_att <- function(df, tau = cs_tau_oracle, config = list()) {
  ci_method <- if (is.null(config$ci_method)) "bootstrap" else config$ci_method
  n_boot <- if (is.null(config$n_boot)) 200 else config$n_boot
  dgp_id <- if (is.null(config$dgp_id)) "unk" else config$dgp_id
  task_seed <- config$seed

  if (!is.data.frame(df)) {
    rlang::abort("`df` must be a data.frame.", class = "causalstress_estimator_error")
  }
  if (!all(c("y", "w") %in% names(df))) {
    rlang::abort("`df` must contain columns `y` and `w`.", class = "causalstress_estimator_error")
  }

  x_cols <- grep("^X", names(df), value = TRUE)
  if (length(x_cols) == 0L) {
    rlang::abort("No covariate columns starting with 'X' found in `df`.", class = "causalstress_estimator_error")
  }
  if (!(is.numeric(df$w) || is.integer(df$w)) || !all(df$w %in% c(0, 1))) {
    rlang::abort("`w` must be numeric/integer and contain only 0/1.", class = "causalstress_estimator_error")
  }
  if (!is.numeric(df$y)) {
    rlang::abort("`y` must be numeric.", class = "causalstress_estimator_error")
  }
  if (anyNA(df[, c("y", "w", x_cols), drop = FALSE])) {
    rlang::abort("`y`, `w`, and covariates `X*` must not contain missing values.", class = "causalstress_estimator_error")
  }

  df_ctrl <- df[df$w == 0, , drop = FALSE]
  if (nrow(df_ctrl) < 2L) {
    rlang::abort("Not enough control units to fit the outcome model.", class = "causalstress_estimator_error")
  }
  df_trt <- df[df$w == 1, , drop = FALSE]
  if (nrow(df_trt) == 0L) {
    rlang::abort("No treated units available to estimate ATT.", class = "causalstress_estimator_error")
  }

  form <- stats::as.formula(paste("y ~", paste(x_cols, collapse = " + ")))

  calc_point <- function(df_boot) {
    treated_b <- df_boot$w == 1
    ctrl_b <- df_boot$w == 0
    if (!any(ctrl_b) || !any(treated_b)) stop("insufficient groups")
    fit_b <- stats::lm(form, data = df_boot[ctrl_b, , drop = FALSE])
    y1_obs_b <- df_boot$y[treated_b]
    y0_hat_b <- stats::predict(fit_b, newdata = df_boot[treated_b, , drop = FALSE])
    mean(y1_obs_b - y0_hat_b)
  }

  # point estimate
  fit <- stats::lm(form, data = df_ctrl)
  y1_obs <- df_trt$y
  y0_hat <- stats::predict(fit, newdata = df_trt)
  att_hat <- mean(y1_obs - y0_hat)

  ci_lo <- NA_real_
  ci_hi <- NA_real_
  ci_meta <- list(
    n_boot_ok = 0L,
    n_boot_fail = 0L,
    ci_valid_by_dim = logical(0),
    collapsed = logical(0),
    ci_valid = NA,
    ci_fail_code = NA_character_,
    ci_method = ci_method,
    ci_type = "percentile",
    ci_level = 0.95
  )
  warnings_vec <- character()

  if (identical(ci_method, "bootstrap")) {
    if (is.null(task_seed)) {
      warning("Bootstrap CI requested but config$seed is missing; CIs set to NA.")
      ci_meta$ci_method <- "none"
      ci_meta$ci_fail_code <- "missing_seed"
    } else {
      salt <- paste("est_lm_att", dgp_id, sep = "|")
      boot_seed <- cs_derive_seed(task_seed, salt)
      ci_res <- cs_bootstrap_ci(calc_point, df, n_boot = n_boot, seed = boot_seed, alpha = 0.05)
      ci_lo <- if (length(ci_res$ci_lo) > 0) ci_res$ci_lo[1] else NA_real_
      ci_hi <- if (length(ci_res$ci_hi) > 0) ci_res$ci_hi[1] else NA_real_
      ci_meta <- ci_res$meta
    }
  } else if (!identical(ci_method, "none")) {
    warning("Unsupported ci_method; falling back to none.")
    ci_method <- "none"
    ci_meta$ci_method <- "none"
    ci_meta$ci_fail_code <- "unsupported_ci_method"
  }

  res <- list(
    att = list(
      estimate = att_hat,
      ci_lo = ci_lo,
      ci_hi = ci_hi
    ),
    qst = NULL,
    cf  = NULL,
    meta = list(
      estimator_id = "lm_att",
      version      = "0.0.1",
      capabilities = c("att"),
      target_level = "population",
      config       = config,
      warnings     = warnings_vec,
      errors       = character(),
      oracle       = FALSE,
      supports_qst = FALSE,
      n_boot_ok    = ci_meta$n_boot_ok,
      n_boot_fail  = ci_meta$n_boot_fail,
      ci_method    = ci_meta$ci_method,
      ci_valid     = ci_meta$ci_valid,
      ci_fail_code = ci_meta$ci_fail_code,
      ci_valid_by_dim = ci_meta$ci_valid_by_dim,
      collapsed    = ci_meta$collapsed,
      ci_type      = ci_meta$ci_type,
      ci_level     = ci_meta$ci_level
    )
  )

  cs_check_estimator_output(res, require_qst = FALSE)
  res
}
