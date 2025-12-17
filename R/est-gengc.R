#' GenGC ATT + QST estimator (soft dependency)
#'
#' @export
est_gengc <- function(df, tau = cs_tau_oracle, config = list()) {
  if (!requireNamespace("GenGC", quietly = TRUE)) {
    rlang::abort(
      "Estimator 'gengc' requires the 'GenGC' package. Please install it.",
      class = "causalstress_dependency_error"
    )
  }

  if (!is.data.frame(df)) {
    rlang::abort("`df` must be a data.frame.", class = "causalstress_estimator_error")
  }
  if (!all(c("y", "w") %in% names(df))) {
    rlang::abort("`df` must contain columns `y` and `w`.", class = "causalstress_estimator_error")
  }

  drop_cols <- intersect(c("y0", "y1", "p", "structural_te"), names(df))
  keep_cols <- setdiff(names(df), drop_cols)
  df_run <- df[, keep_cols, drop = FALSE]

  n_draws    <- if (!is.null(config$n_draws)) config$n_draws else 300
  num_trees  <- if (!is.null(config$num_trees)) config$num_trees else 800
  num_threads <- if (!is.null(config$num_threads)) config$num_threads else 1L

  formula <- stats::as.formula("y ~ . - w")

  fit <- GenGC::gengc(
    formula    = formula,
    data       = df_run,
    treatment  = "w",
    target     = "both",
    tau_grid   = tau,
    n_draws    = n_draws,
    num_trees  = num_trees,
    num_threads = num_threads
  )

  att_hat <- as.numeric(fit$att)
  qst_tbl <- tibble::tibble(
    tau   = fit$tau_grid,
    value = fit$qst
  )

  ci_method <- if (is.null(config$ci_method)) "bootstrap" else config$ci_method
  n_boot <- if (is.null(config$n_boot)) 200 else config$n_boot
  dgp_id <- if (is.null(config$dgp_id)) "unk" else config$dgp_id
  task_seed <- config$seed

  stat_fn <- function(boot_df) {
    x_cols <- if (!is.null(config$covariates)) {
      config$covariates
    } else {
      setdiff(names(boot_df), c("y", "w", "p", "y0", "y1", "id", "dgp_id", "seed"))
    }
    fit_b <- GenGC::gengc(
      y = boot_df$y,
      w = boot_df$w,
      x = as.matrix(boot_df[, x_cols, drop = FALSE]),
      tau_grid   = tau,
      n_draws    = n_draws,
      num_trees  = num_trees,
      num_threads = num_threads
    )
    fit_b$att
  }

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
      salt <- paste("est_gengc", dgp_id, sep = "|")
      boot_seed <- cs_derive_seed(task_seed, salt)
      ci_res <- cs_bootstrap_ci(stat_fn, df_run, n_boot = n_boot, seed = boot_seed, alpha = 0.05)
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
    qst = qst_tbl,
    cf  = NULL,
    meta = list(
      estimator_id = "gengc",
      version      = as.character(utils::packageVersion("GenGC")),
      capabilities = c("att", "qst"),
      target_level = "population",
      config       = list(
        n_draws    = n_draws,
        num_trees  = num_trees,
        num_threads = num_threads,
        ci_method  = ci_method,
        n_boot     = n_boot
      ),
      warnings = warnings_vec,
      errors   = character(),
      oracle       = FALSE,
      supports_qst = TRUE,
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

  cs_check_estimator_output(res, require_qst = TRUE, tau = tau)
  res
}
