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
    tau   = fit$tau_grid,
    value = fit$qst
  )

  res <- list(
    att = list(estimate = att_hat),
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
        num_threads    = num_threads
      ),
      warnings     = character(),
      errors       = character(),
      oracle       = FALSE,
      supports_qst = TRUE
    )
  )

  cs_check_estimator_output(res, require_qst = TRUE, tau = tau)
  res
}

