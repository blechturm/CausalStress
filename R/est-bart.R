#' BART ATT estimator (bartCause)
#'
#' Soft-dependency wrapper around `bartCause::bartc` targeting the ATT.
#'
#' @param df Data frame with outcome `y`, treatment `w`, and covariates.
#' @param config Optional list of parameters passed through to `bartCause::bartc`.
#'   Recognised fields: `seed` (numeric scalar).
#' @param tau Unused (included for signature compatibility).
#' @param ... Additional arguments passed to `bartCause::bartc`.
#'
#' @return A list compatible with `cs_check_estimator_output()`.
#' @export
est_bart_att <- function(df, config = list(), tau = cs_tau_oracle, ...) {
  if (!requireNamespace("bartCause", quietly = TRUE)) {
    stop("Package 'bartCause' needed for this estimator.")
  }

  extract_first_numeric <- function(row, keys) {
    for (k in keys) {
      if (k %in% names(row)) {
        v <- row[[k]]
        if (length(v) >= 1) return(as.numeric(v[[1]]))
      }
    }
    NA_real_
  }

  pick_att_row <- function(tab) {
    if (!is.null(tab) && is.data.frame(tab) && nrow(tab) > 0) {
      if ("estimand" %in% names(tab)) {
        idx <- which(as.character(tab$estimand) == "att")
        if (length(idx) > 0) return(tab[idx[1], , drop = FALSE])
      }
      rn <- rownames(tab)
      if (!is.null(rn) && "att" %in% rn) return(tab["att", , drop = FALSE])
      if (!is.null(rn) && "ATT" %in% rn) return(tab["ATT", , drop = FALSE])
      return(tab[1, , drop = FALSE])
    }
    tab
  }

  ci_method <- if (is.null(config$ci_method)) "native" else config$ci_method
  n_boot <- if (is.null(config$n_boot)) 200 else config$n_boot
  dgp_id <- if (is.null(config$dgp_id)) "unk" else config$dgp_id
  task_seed <- config$seed

  threads <- if (is.null(config$num_threads)) 1L else config$num_threads

  y   <- df$y
  trt <- df$w
  w   <- df[, setdiff(names(df), c("y", "w")), drop = FALSE]

  seed_val <- config$seed
  if (is.null(seed_val)) seed_val <- NA

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

  estimate <- NA_real_

  run_bart <- function(data) {
    bartCause::bartc(
      response    = data$y,
      treatment   = data$w,
      confounders = data[, setdiff(names(data), c("y", "w")), drop = FALSE],
      estimand    = "att",
      n.threads   = threads,
      n.chains    = 1L,
      seed        = seed_val,
      ...
    )
  }

  if (identical(ci_method, "native")) {
    fit <- run_bart(df)
    est_tab <- summary(fit)$estimates
    est_row <- pick_att_row(est_tab)
    estimate <- extract_first_numeric(est_row, c("est", "estimate", "psi"))
    ci_lo <- extract_first_numeric(est_row, c("ci.025", "ci.lower", "ci.low", "ci_lo", "lower", "lwr"))
    ci_hi <- extract_first_numeric(est_row, c("ci.975", "ci.upper", "ci.high", "ci_hi", "upper", "upr"))
    valid <- is.finite(ci_lo) && is.finite(ci_hi) && ci_lo <= ci_hi
    if (is.na(valid)) valid <- FALSE
    ci_meta$ci_valid <- valid
    ci_meta$ci_fail_code <- if (valid) NA_character_ else "invalid_bounds"
    ci_meta$collapsed <- valid && abs(ci_hi - ci_lo) < 1e-8
    ci_meta$ci_valid_by_dim <- valid
    ci_meta$ci_type <- "credible"
  } else if (identical(ci_method, "bootstrap")) {
    if (is.null(task_seed)) stop("config$seed is required for bootstrap CI")
    stat_fn <- function(boot_df) {
      fit_b <- run_bart(boot_df)
      est_tab_b <- summary(fit_b)$estimates
      est_row_b <- pick_att_row(est_tab_b)
      extract_first_numeric(est_row_b, c("est", "estimate", "psi"))
    }
    salt <- paste("est_bart_att", dgp_id, sep = "|")
    boot_seed <- cs_derive_seed(task_seed, salt)
    ci_res <- cs_bootstrap_ci(stat_fn, df, n_boot = n_boot, seed = boot_seed, alpha = 0.05)
    ci_lo <- if (length(ci_res$ci_lo) > 0) ci_res$ci_lo[1] else NA_real_
    ci_hi <- if (length(ci_res$ci_hi) > 0) ci_res$ci_hi[1] else NA_real_
    ci_meta <- ci_res$meta
    fit_final <- run_bart(df)
    est_tab_final <- summary(fit_final)$estimates
    est_row <- pick_att_row(est_tab_final)
    estimate <- extract_first_numeric(est_row, c("est", "estimate", "psi"))
  } else {
    warning("Unsupported ci_method; falling back to none.")
    ci_meta$ci_method <- "none"
    ci_meta$ci_fail_code <- "unsupported_ci_method"
    fit <- run_bart(df)
    est_tab <- summary(fit)$estimates
    est_row <- pick_att_row(est_tab)
    estimate <- extract_first_numeric(est_row, c("est", "estimate", "psi"))
  }

  list(
    att = list(
      estimate = as.numeric(estimate),
      ci_lo    = as.numeric(ci_lo),
      ci_hi    = as.numeric(ci_hi)
    ),
    qst = NULL,
    meta = list(
      estimator_id   = "bart_att",
      oracle         = FALSE,
      supports_qst   = FALSE,
      estimator_pkgs = c("bartCause"),
      num_threads    = threads,
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
}
