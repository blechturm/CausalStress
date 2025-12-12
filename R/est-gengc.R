#' GenGC ATT + QST estimator (soft dependency)
#'
#' Adapter around the external GenGC package that estimates ATT and QST while
#' respecting the CausalStress airlock and contracts. Uses a soft dependency
#' pattern: GenGC is suggested, not imported.
#'
#' @param df Data frame with columns `y`, `w`, and covariates; oracle columns
#'   (`y0`, `y1`, `p`, `structural_te`) will be stripped before calling GenGC.
#' @param tau Numeric vector of quantile levels passed to GenGC via `tau_grid`.
#' @param config Optional list with entries `n_draws` (default 300) and
#'   `num_trees` (default 800) forwarded to GenGC.
#'
#' @return A list with `att`, `qst`, `cf = NULL`, and `meta` satisfying
#'   `cs_check_estimator_output(require_qst = TRUE)`.
#' @export
est_gengc <- function(df, tau = cs_tau_oracle, config = list()) {
  if (!requireNamespace("GenGC", quietly = TRUE)) {
    rlang::abort(
      "Estimator 'gengc' requires the 'GenGC' package. Please install it.",
      class = "causalstress_dependency_error"
    )
  }

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

  # Airlock: drop oracle-only columns before handing data to GenGC
  drop_cols <- intersect(c("y0", "y1", "p", "structural_te"), names(df))
  keep_cols <- setdiff(names(df), drop_cols)
  df_run <- df[, keep_cols, drop = FALSE]

  n_draws    <- if (!is.null(config$n_draws)) config$n_draws else 300
  num_trees  <- if (!is.null(config$num_trees)) config$num_trees else 800
  num_threads <- 1L  # enforce single-threaded execution

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

  res <- list(
    att = list(
      estimate = att_hat
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
        num_threads = num_threads
      ),
      warnings = character(),
      errors   = character(),
      oracle       = FALSE,
      supports_qst = TRUE
    )
  )

  cs_check_estimator_output(res, require_qst = TRUE, tau = tau)

  res
}
