#' TMLE estimator (ATE target)
#'
#' Soft-dependency wrapper around `tmle::tmle`. Note: the tmle package targets
#' the ATE; when used in CausalStress (ATT benchmarking), the result may be
#' biased under treatment effect heterogeneity. This wrapper records the
#' estimand target in the metadata and adds a warning.
#'
#' @param df Data frame with outcome `y`, treatment `w`, and covariates.
#' @param config Optional list of configuration options. Common fields include:
#' \itemize{
#'   \item \code{ci_method}: CI intent; one of "none", "default", "bootstrap", "native" (see [cs_ci_methods]).
#'   \item \code{seed}: required when bootstrap CIs are requested.
#'   \item \code{n_boot}: number of bootstrap draws if using bootstrap CIs.
#'   \item \code{num_threads}: sets \code{mc.cores} for downstream learners.
#'   \item Additional parameters forwarded to `tmle::tmle`.
#' }
#' For this estimator, \code{ci_method = "default"} maps to \code{"native"}.
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

  method_in <- config$ci_method %||% "none"
  if (identical(method_in, "default")) {
    ci_method <- "native"
  } else {
    ci_method <- method_in
  }
  n_boot <- if (is.null(config$n_boot)) 200 else config$n_boot
  dgp_id <- if (is.null(config$dgp_id)) "unk" else config$dgp_id
  task_seed <- config$seed
  ci_method_source <- config$ci_method_source %||% {
    if (is.null(config$ci_method)) {
      "implicit_none"
    } else if (identical(config$ci_method, "default")) {
      "default_mapped"
    } else {
      "explicit"
    }
  }

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
    if (is.null(task_seed)) {
      rlang::abort(
        message = "Bootstrap CI requested (or implied by default) but `config$seed` is missing.",
        class = "causalstress_config_error",
        body = c(
          "x" = "Bootstrap relies on random sampling and requires a deterministic seed for reproducibility.",
          "i" = "Provide `seed` in the `config` list or use `cs_run_campaign()` / `cs_run_seeds()` (which handle this automatically).",
          "i2" = "If you only need point estimates, set `ci_method = \"none\"`."
        )
      )
    }
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
      ci_method_in = method_in,
      ci_method_source = ci_method_source,
      seed_used    = task_seed %||% NA_integer_,
      n_boot_ok    = ci_meta$n_boot_ok,
      n_boot_fail  = ci_meta$n_boot_fail
    )
  )

  res
}
