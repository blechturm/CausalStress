#' TMLE estimator (ATE target)
#'
#' Soft-dependency wrapper around `tmle::tmle`. Note: the tmle package targets
#' the ATE; when used in CausalStress (ATT benchmarking), the result may be
#' biased under treatment effect heterogeneity. This wrapper records the
#' estimand target in the metadata and adds a warning.
#'
#' @param df Data frame with outcome `y`, treatment `w`, and covariates.
#' @param config Optional list of parameters forwarded to `tmle::tmle`.
#' @param tau Unused (signature compatibility).
#' @param ... Additional arguments passed to `tmle::tmle`.
#'
#' @return A list compatible with `cs_check_estimator_output()`.
#' @export
est_tmle_att <- function(df, config = list(), tau = cs_tau_oracle, ...) {
  if (!requireNamespace("tmle", quietly = TRUE)) {
    stop("Package 'tmle' needed for this estimator.")
  }
  if (!requireNamespace("SuperLearner", quietly = TRUE)) {
    stop("Package 'SuperLearner' needed for tmle estimator.")
  }

  ci_method <- if (is.null(config$ci_method)) "native" else config$ci_method
  n_boot <- if (is.null(config$n_boot)) 200 else config$n_boot
  dgp_id <- if (is.null(config$dgp_id)) "unk" else config$dgp_id
  task_seed <- config$seed

  threads <- if (is.null(config$num_threads)) 1L else config$num_threads
  old_mc <- getOption("mc.cores")
  on.exit(options(mc.cores = old_mc), add = TRUE)
  options(mc.cores = threads)

  Y <- df$y
  A <- df$w
  W <- df[, setdiff(names(df), c("y", "w")), drop = FALSE]

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

  est <- NA_real_

  if (identical(ci_method, "native")) {
    fit <- tmle::tmle(
      Y      = Y,
      A      = A,
      W      = W,
      family = "gaussian",
      ...
    )
    est <- fit$estimates$ATE$psi
    ci   <- fit$estimates$ATE$CI
    ci_lo <- as.numeric(ci[1])
    ci_hi <- as.numeric(ci[2])
    valid <- is.finite(ci_lo) && is.finite(ci_hi) && ci_lo <= ci_hi
    ci_meta$ci_valid <- valid
    ci_meta$ci_fail_code <- if (valid) NA_character_ else "invalid_bounds"
    ci_meta$collapsed <- valid && abs(ci_hi - ci_lo) < 1e-8
    ci_meta$ci_valid_by_dim <- valid
    ci_meta$ci_type <- "asymptotic"
  } else if (identical(ci_method, "bootstrap")) {
    if (is.null(task_seed)) stop("config$seed is required for bootstrap CI")
    stat_fn <- function(boot_df) {
      tmle::tmle(
        Y      = boot_df$y,
        A      = boot_df$w,
        W      = boot_df[, setdiff(names(boot_df), c("y", "w")), drop = FALSE],
        family = "gaussian",
        ...
      )$estimates$ATE$psi
    }
    salt <- paste("est_tmle_att", dgp_id, sep = "|")
    boot_seed <- cs_derive_seed(task_seed, salt)
    ci_res <- cs_bootstrap_ci(stat_fn, df, n_boot = n_boot, seed = boot_seed, alpha = 0.05)
    ci_lo <- if (length(ci_res$ci_lo) > 0) ci_res$ci_lo[1] else NA_real_
    ci_hi <- if (length(ci_res$ci_hi) > 0) ci_res$ci_hi[1] else NA_real_
    ci_meta <- ci_res$meta
    est <- stat_fn(df)
  } else {
    warning("Unsupported ci_method; falling back to none.")
    ci_meta$ci_method <- "none"
    ci_meta$ci_fail_code <- "unsupported_ci_method"
    est <- tmle::tmle(
      Y      = Y,
      A      = A,
      W      = W,
      family = "gaussian",
      ...
    )$estimates$ATE$psi
  }

  res <- list(
    att = list(
      estimate = as.numeric(est),
      ci_lo    = as.numeric(ci_lo),
      ci_hi    = as.numeric(ci_hi)
    ),
    qst = NULL,
    meta = list(
      estimator_id      = "tmle_att",
      oracle            = FALSE,
      supports_qst      = FALSE,
      estimator_pkgs    = c("tmle", "SuperLearner"),
      estimand_target   = "ATE",
      warnings          = "TMLE package targets ATE; result may be biased for ATT if treatment effect is heterogeneous.",
      num_threads       = threads,
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

  res
}
