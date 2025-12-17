#' Doubly robust GenGC estimator (soft dependency)
#'
#' Adapter around GenGC's doubly robust estimator. Mirrors `est_gengc` but
#' uses `GenGC::gengc_dr` with cross-fitting while respecting the airlock and
#' single-thread contract.
#'
#' @param df Data frame with `y`, `w`, and covariates; oracle columns (`y0`,
#'   `y1`, `p`, `structural_te`) are dropped before calling GenGC.
#' @param tau Numeric vector of quantile levels passed to `tau_grid`.
#' @param config Optional list: `n_draws` (default 300), `num_trees` (default
#'   800), `crossfit_folds` (default 5), `num_threads` (default 1, enforced).
#'
#' @return List with `att`, `qst`, `cf = NULL`, and `meta` satisfying
#'   `cs_check_estimator_output(require_qst = TRUE)`.
#' @export
est_gengc_dr <- function(df, tau = cs_tau_oracle, config = list()) {
  if (!requireNamespace("GenGC", quietly = TRUE)) {
    rlang::abort(
      "Estimator 'gengc_dr' requires the 'GenGC' package. Please install it.",
      class = "causalstress_dependency_error"
    )
  }

  if (!is.data.frame(df)) {
    rlang::abort("`df` must be a data.frame.", class = "causalstress_estimator_error")
  }
  if (!all(c("y", "w") %in% names(df))) {
    rlang::abort("`df` must contain columns `y` and `w`.", class = "causalstress_estimator_error")
  }

  # Airlock: remove oracle-only columns
  drop_cols <- intersect(c("y0", "y1", "p", "structural_te"), names(df))
  df_run <- df[, setdiff(names(df), drop_cols), drop = FALSE]

  n_draws        <- if (!is.null(config$n_draws)) config$n_draws else 300
  num_trees      <- if (!is.null(config$num_trees)) config$num_trees else 800
  crossfit_folds <- if (!is.null(config$crossfit_folds)) config$crossfit_folds else 5
  num_threads    <- if (!is.null(config$num_threads)) config$num_threads else 1L

  formula <- stats::as.formula("y ~ . - w")

  fit <- GenGC::gengc_dr(
    formula        = formula,
    data           = df_run,
    treatment      = "w",
    target         = "both",
    tau_grid       = tau,
    n_draws        = n_draws,
    num_trees      = num_trees,
    crossfit_folds = crossfit_folds,
    num_threads    = num_threads
  )

  att_hat <- as.numeric(fit$att_dr)
  qst_tbl <- tibble::tibble(
    tau_id = cs_tau_id(fit$tau_grid),
    tau   = fit$tau_grid,
    value = fit$qst
  )

  ci_method <- if (is.null(config$ci_method)) "bootstrap" else config$ci_method
  n_boot <- if (is.null(config$n_boot)) 200 else config$n_boot
  dgp_id <- if (is.null(config$dgp_id)) "unk" else config$dgp_id
  task_seed <- config$seed

  stat_fn <- function(boot_df) {
    fit_b <- GenGC::gengc_dr(
      formula        = formula,
      data           = boot_df,
      treatment      = "w",
      target         = "both",
      tau_grid       = tau,
      n_draws        = n_draws,
      num_trees      = num_trees,
      crossfit_folds = crossfit_folds,
      num_threads    = num_threads
    )
    c(as.numeric(fit_b$att_dr), as.numeric(fit_b$qst))
  }

  ci_lo <- NA_real_
  ci_hi <- NA_real_
  ci_meta <- list(
    n_boot_ok       = 0L,
    n_boot_fail     = 0L,
    ci_valid_by_dim = logical(0),
    collapsed       = logical(0),
    ci_valid        = NA,
    ci_fail_code    = NA_character_,
    ci_method       = ci_method,
    ci_type         = "percentile",
    ci_level        = 0.95
  )
  qst_ci_meta <- list(
    ci_method       = ci_method,
    ci_type         = NA_character_,
    ci_level        = NA_real_,
    ci_valid        = NA,
    ci_fail_code    = NA_character_,
    ci_valid_by_dim = logical(0),
    collapsed       = logical(0)
  )
  warnings_vec <- character()

  if (identical(ci_method, "bootstrap")) {
    if (is.null(task_seed)) {
      warning("Bootstrap CI requested but config$seed is missing; CIs set to NA.")
      ci_meta$ci_method <- "none"
      ci_meta$ci_fail_code <- "missing_seed"
      qst_ci_meta$ci_method <- "none"
      qst_ci_meta$ci_fail_code <- "missing_seed"
    } else {
      salt <- paste("est_gengc_dr", dgp_id, sep = "|")
      boot_seed <- cs_derive_seed(task_seed, salt)
      ci_res <- cs_bootstrap_ci(stat_fn, df_run, n_boot = n_boot, seed = boot_seed, alpha = 0.05)
      if (length(ci_res$ci_lo) > 0L) {
        k_expected <- 1L + length(tau)
        if (length(ci_res$ci_lo) != k_expected || length(ci_res$ci_hi) != k_expected) {
          warning("Bootstrap CI internal error (unexpected statistic length); CIs set to NA.")
          ci_meta$ci_fail_code <- "invalid_stat_length"
          qst_ci_meta$ci_fail_code <- "invalid_stat_length"
        } else {
          # ATT CI (dimension 1)
          ci_lo <- ci_res$ci_lo[1]
          ci_hi <- ci_res$ci_hi[1]

          att_ok <- as.integer(ci_res$meta$n_boot_ok[1])
          att_fail <- as.integer(ci_res$meta$n_boot_fail[1])
          att_dim_gated <- att_ok >= (n_boot * 0.9)
          att_valid_by_dim <- isTRUE(ci_res$meta$ci_valid_by_dim[1])
          att_ci_valid <- isTRUE(att_dim_gated) && isTRUE(att_valid_by_dim)
          att_fail_code <- NA_character_
          if (!att_dim_gated) {
            att_fail_code <- "low_boot_success"
          } else if (!att_ci_valid) {
            att_fail_code <- "invalid_bounds"
          }
          ci_meta <- list(
            n_boot_ok       = att_ok,
            n_boot_fail     = att_fail,
            ci_valid_by_dim = ci_res$meta$ci_valid_by_dim[1],
            collapsed       = ci_res$meta$collapsed[1],
            ci_valid        = att_ci_valid,
            ci_fail_code    = att_fail_code,
            ci_method       = ci_res$meta$ci_method,
            ci_type         = ci_res$meta$ci_type,
            ci_level        = ci_res$meta$ci_level
          )

          # QST CI (dimensions 2..k)
          qst_lo <- ci_res$ci_lo[-1]
          qst_hi <- ci_res$ci_hi[-1]
          qst_ok <- as.integer(ci_res$meta$n_boot_ok[-1])
          qst_fail <- as.integer(ci_res$meta$n_boot_fail[-1])
          qst_dim_gated <- qst_ok >= (n_boot * 0.9)
          qst_valid_by_dim <- ci_res$meta$ci_valid_by_dim[-1]
          qst_collapsed <- ci_res$meta$collapsed[-1]

          qst_ci_valid <- any(qst_dim_gated) && all(qst_valid_by_dim[qst_dim_gated])
          qst_fail_code <- NA_character_
          if (any(!qst_dim_gated)) {
            qst_fail_code <- "low_boot_success"
          } else if (!qst_ci_valid) {
            qst_fail_code <- "invalid_bounds"
          }

          qst_tbl$qst_ci_lo <- qst_lo
          qst_tbl$qst_ci_hi <- qst_hi
          qst_tbl$qst_n_boot_ok <- qst_ok
          qst_tbl$qst_n_boot_fail <- qst_fail

          qst_ci_meta <- list(
            ci_method       = ci_res$meta$ci_method,
            ci_type         = ci_res$meta$ci_type,
            ci_level        = ci_res$meta$ci_level,
            ci_valid        = qst_ci_valid,
            ci_fail_code    = qst_fail_code,
            ci_valid_by_dim = qst_valid_by_dim,
            collapsed       = qst_collapsed
          )
        }
      } else {
        ci_meta <- ci_res$meta
        qst_ci_meta <- ci_res$meta
      }
    }
  } else if (!identical(ci_method, "none")) {
    warning("Unsupported ci_method; falling back to none.")
    ci_method <- "none"
    ci_meta$ci_method <- "none"
    ci_meta$ci_fail_code <- "unsupported_ci_method"
    qst_ci_meta$ci_method <- "none"
    qst_ci_meta$ci_fail_code <- "unsupported_ci_method"
  }

  res <- list(
    att = list(estimate = att_hat, ci_lo = ci_lo, ci_hi = ci_hi),
    qst = qst_tbl,
    cf  = NULL,
    meta = list(
      estimator_id = "gengc_dr",
      version      = as.character(utils::packageVersion("GenGC")),
      capabilities = c("att", "qst"),
      target_level = "population",
      config       = list(
        n_draws        = n_draws,
        num_trees      = num_trees,
        crossfit_folds = crossfit_folds,
        num_threads    = num_threads,
        ci_method      = ci_method,
        n_boot         = n_boot
      ),
      warnings     = warnings_vec,
      errors       = character(),
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
      ci_level     = ci_meta$ci_level,
      qst_ci_method       = qst_ci_meta$ci_method,
      qst_ci_type         = qst_ci_meta$ci_type,
      qst_ci_level        = qst_ci_meta$ci_level,
      qst_ci_valid        = qst_ci_meta$ci_valid,
      qst_ci_fail_code    = qst_ci_meta$ci_fail_code,
      qst_ci_valid_by_dim = qst_ci_meta$ci_valid_by_dim,
      qst_ci_collapsed    = qst_ci_meta$collapsed
    )
  )

  cs_check_estimator_output(res, require_qst = TRUE, tau = tau)
  res
}
