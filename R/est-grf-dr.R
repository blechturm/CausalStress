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

  grf_config <- config

  # Enforce single-threaded execution per Constitution (wide & shallow)
  # Translate num_threads -> num.threads for grf API and drop the original key.
  if (!is.null(grf_config$num_threads)) {
    grf_config$num.threads <- grf_config$num_threads
    grf_config$num_threads <- NULL
  }
  if (is.null(grf_config$num.threads)) {
    grf_config$num.threads <- 1L
  }
  grf_config$ci_method <- NULL
  grf_config$n_boot <- NULL
  grf_config$dgp_id <- NULL
  grf_config$seed <- NULL

  ci_method <- if (is.null(config$ci_method)) "native" else config$ci_method
  n_boot <- if (is.null(config$n_boot)) 200 else config$n_boot
  dgp_id <- if (is.null(config$dgp_id)) "unk" else config$dgp_id
  task_seed <- config$seed

  args <- c(
    list(
      X = X,
      Y = y,
      W = w
    ),
    grf_config
  )

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
    ci_type = NA_character_,
    ci_level = 0.95
  )

  att_hat <- NA_real_

  if (identical(ci_method, "native")) {
    forest <- do.call(grf::causal_forest, args)
    ate <- grf::average_treatment_effect(
      forest,
      target.sample = "treated"
    )
    att_hat <- as.numeric(ate[["estimate"]])
    se_hat <- as.numeric(ate[["std.err"]])
    ci_lo <- att_hat - 1.96 * se_hat
    ci_hi <- att_hat + 1.96 * se_hat
    valid <- is.finite(ci_lo) && is.finite(ci_hi) && ci_lo <= ci_hi
    ci_meta$ci_valid <- valid
    ci_meta$ci_fail_code <- if (valid) NA_character_ else "invalid_bounds"
    ci_meta$collapsed <- !is.na(ci_lo) && !is.na(ci_hi) && abs(ci_hi - ci_lo) < 1e-8
    ci_meta$ci_valid_by_dim <- valid
    ci_meta$ci_type <- "wald"
  } else if (identical(ci_method, "bootstrap")) {
    if (is.null(task_seed)) stop("config$seed is required for bootstrap CI")
    stat_fn <- function(boot_df) {
      x_cols_b <- setdiff(names(boot_df), core_cols)
      Xb <- as.matrix(boot_df[, x_cols_b, drop = FALSE])
      args_b <- c(
        list(
          X = Xb,
          Y = boot_df$y,
          W = boot_df$w
        ),
        grf_config
      )
      forest_b <- do.call(grf::causal_forest, args_b)
      ate_b <- grf::average_treatment_effect(
        forest_b,
        target.sample = "treated"
      )
      as.numeric(ate_b[["estimate"]])
    }
    salt <- paste("est_grf_dr_att", dgp_id, sep = "|")
    boot_seed <- cs_derive_seed(task_seed, salt)
    ci_res <- cs_bootstrap_ci(stat_fn, df, n_boot = n_boot, seed = boot_seed, alpha = 0.05)
    ci_lo <- if (length(ci_res$ci_lo) > 0) ci_res$ci_lo[1] else NA_real_
    ci_hi <- if (length(ci_res$ci_hi) > 0) ci_res$ci_hi[1] else NA_real_
    ci_meta <- ci_res$meta
    att_hat <- stat_fn(df)
  } else {
    warning("Unsupported ci_method; falling back to none.")
    ci_meta$ci_method <- "none"
    ci_meta$ci_fail_code <- "unsupported_ci_method"
    forest <- do.call(grf::causal_forest, args)
    ate <- grf::average_treatment_effect(
      forest,
      target.sample = "treated"
    )
    att_hat <- as.numeric(ate[["estimate"]])
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
      estimator_id = "grf_dr_att",
      version      = "0.1.0",
      capabilities = c("att"),
      target_level = "population",
      config       = config,
      warnings     = character(),
      errors       = character(),
      oracle       = FALSE,
      supports_qst = FALSE,
      ci_method    = ci_meta$ci_method,
      ci_valid     = ci_meta$ci_valid,
      ci_fail_code = ci_meta$ci_fail_code,
      ci_valid_by_dim = ci_meta$ci_valid_by_dim,
      collapsed    = ci_meta$collapsed,
      ci_type      = ci_meta$ci_type,
      ci_level     = ci_meta$ci_level,
      n_boot_ok    = ci_meta$n_boot_ok,
      n_boot_fail  = ci_meta$n_boot_fail
    )
  )

  cs_check_estimator_output(res, require_qst = FALSE)

  res
}

#' @export
est_grf_dr <- function(df, config = list(), tau = cs_tau_oracle, ...) {
  est_grf_dr_att(df = df, config = config, tau = tau, ...)
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
