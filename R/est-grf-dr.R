#' GRF-based doubly-robust ATT estimator (causal forest)
#'
#' This estimator wraps the `grf::causal_forest()` implementation to estimate
#' the average treatment effect on the treated (ATT). It is treated as
#' "doubly-robust" in the sense of GRF: the causal forest internally uses
#' orthogonalization with nuisance estimates for propensity and outcome.
#'
#' This estimator is **optional** and requires the `grf` package. It is not
#' part of the core estimator registry by default; instead, it can be
#' registered via `cs_register_grf_dr_att()`.
#'
#' @param df Data frame as returned in `dgp$df`, containing at least
#'   `y`, `w`, and covariates. Covariates are taken as all non-core columns
#'   (i.e., excluding `y`, `w`, `y0`, `y1`, `p`, `structural_te`).
#' @param config Optional list of GRF configuration (e.g., `num.trees`,
#'   `min.node.size`). If not supplied, defaults are used.
#' @param tau Ignored. Included for API compatibility with other estimators.
#' @param ... Ignored. Reserved for future extensions.
#'
#' @return A list with components:
#'   - att: tibble with columns `estimand` and `estimate`.
#'   - qst: `NULL` (this estimator does not provide QST).
#'   - meta: list with fields `estimator_id`, `oracle`, `supports_qst`.
#'
#' @export
est_grf_dr_att <- function(df, config = list(), tau = cs_tau_oracle, ...) {
  if (!requireNamespace("grf", quietly = TRUE)) {
    rlang::abort(
      "Estimator 'grf_dr_att' requires the 'grf' package. Please install it.",
      class = "causalstress_dependency_error"
    )
  }

  required <- c("y", "w")
  missing <- setdiff(required, names(df))
  if (length(missing) > 0L) {
    rlang::abort(
      glue::glue("Input data frame is missing required columns: {toString(missing)}"),
      class = "causalstress_estimator_error"
    )
  }

  y <- df$y
  w <- df$w

  core_cols <- c("y", "w", "y0", "y1", "p", "structural_te")
  x_cols <- setdiff(names(df), core_cols)
  if (length(x_cols) == 0L) {
    rlang::abort(
      "No covariate columns found for GRF (after excluding y, w, y0, y1, p, structural_te).",
      class = "causalstress_estimator_error"
    )
  }

  X <- as.matrix(df[, x_cols, drop = FALSE])

  args <- c(
    list(
      X = X,
      Y = y,
      W = w
    ),
    config
  )

  forest <- do.call(grf::causal_forest, args)

  ate <- grf::average_treatment_effect(
    forest,
    target.sample = "treated"
  )

  att_hat <- as.numeric(ate[["estimate"]])

  att <- tibble::tibble(
    estimand = "ATT",
    estimate = att_hat
  )

  res <- list(
    att = att,
    qst = NULL,
    cf  = NULL,
    meta = list(
      estimator_id = "grf_dr_att",
      version      = "0.1.0",
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

#' Register GRF-based ATT estimator in the estimator registry
#'
#' This helper registers `est_grf_dr_att` under the id `grf_dr_att` in the
#' estimator registry, marking it as an optional estimator that requires the
#' `grf` package. It can be called from user code or from an extension package.
#'
#' @return Invisibly, the updated estimator registry.
#' @export
cs_register_grf_dr_att <- function() {
  if (!requireNamespace("grf", quietly = TRUE)) {
    rlang::abort(
      "Cannot register 'grf_dr_att': the 'grf' package is not installed.",
      class = "causalstress_dependency_error"
    )
  }

  pkg_ver <- as.character(utils::packageVersion("CausalStress"))

  cs_register_estimator(
    estimator_id  = "grf_dr_att",
    type          = "dr",
    generator     = est_grf_dr_att,
    oracle        = FALSE,
    supports_qst  = FALSE,
    version       = pkg_ver,
    description   = "Doubly-robust GRF ATT estimator (generalized random forest).",
    source        = "R:grf",
    requires_pkgs = "grf"
  )
}
