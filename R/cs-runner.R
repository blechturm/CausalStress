#' Run a single DGP/estimator combination for one seed
#'
#' @export
cs_run_single <- function(
  dgp_id,
  estimator_id,
  n,
  seed,
  tau       = cs_tau_oracle,
  bootstrap = FALSE,
  B         = 200L,
  config    = list(),
  ...
) {
  cs_chk_scalar_numeric(n, "n")
  if (n <= 0) {
    rlang::abort(
      "`n` must be a positive scalar.",
      class = "causalstress_runner_error"
    )
  }

  # Look up DGP + estimator descriptors
  dgp_desc <- cs_get_dgp(dgp_id)
  est_desc <- cs_get_estimator(estimator_id)

  dgp <- dgp_desc$generator(n = n, seed = seed)
  cs_check_dgp_synthetic(dgp)

  df_raw    <- dgp$df
  true_att  <- dgp$true_att

  oracle_allowed <- isTRUE(est_desc$oracle) || isTRUE(config$use_oracle)
  if (!oracle_allowed) {
    drop_cols <- c("y0", "y1", "p", "structural_te")
    keep <- setdiff(names(df_raw), drop_cols)
    df_run <- df_raw[, keep, drop = FALSE]
    attr(df_run, "structural_te") <- NULL
    attr(df_run, "params") <- NULL
  } else {
    df_run <- df_raw
  }

  # Run estimator
  res <- est_desc$generator(
    df     = df_run,
    config = config,
    tau    = tau,
    ...
  )
  cs_check_estimator_output(res, require_qst = est_desc$supports_qst, tau = tau)

  # Extract ATT estimate (works for tibble or list)
  att <- res$att
  if (is.data.frame(att)) {
    est_att <- att[["estimate"]]
  } else {
    est_att <- att$estimate
  }

  ci_lo_att <- NA_real_
  ci_hi_att <- NA_real_
  n_ok <- 0L
  if (bootstrap) {
    n_obs <- nrow(df_run)
    boot_att <- numeric(B)
    for (b in seq_len(B)) {
      idx_b <- sample.int(n_obs, size = n_obs, replace = TRUE)
      df_b  <- df_run[idx_b, , drop = FALSE]
      res_b <- try(est_desc$generator(df = df_b, config = config, tau = tau, ...), silent = TRUE)
      if (inherits(res_b, "try-error")) next
      att_b <- if (is.data.frame(res_b$att)) res_b$att[["estimate"]] else res_b$att$estimate
      if (is.finite(att_b)) {
        n_ok <- n_ok + 1L
        boot_att[n_ok] <- att_b
      }
    }
    if (n_ok > 0L) {
      boot_att <- boot_att[seq_len(n_ok)]
      ci_lo_att <- stats::quantile(boot_att, 0.025, na.rm = TRUE)
      ci_hi_att <- stats::quantile(boot_att, 0.975, na.rm = TRUE)
    }
  }

  att_error      <- est_att - true_att
  att_abs_error  <- abs(att_error)

  cs_ver <- as.character(utils::packageVersion("CausalStress"))
  if (length(est_desc$requires_pkgs) == 0L) {
    pkg_versions <- c(CausalStress = cs_ver)
  } else {
    dep_versions <- vapply(
      est_desc$requires_pkgs,
      function(pkg) as.character(utils::packageVersion(pkg)),
      character(1)
    )
    pkg_versions <- c(CausalStress = cs_ver, dep_versions)
  }

  estimator_pkgs <- paste0(names(pkg_versions), "=", pkg_versions, collapse = ";")

  att_covered <- if (!is.na(ci_lo_att) && !is.na(ci_hi_att)) {
    true_att >= ci_lo_att && true_att <= ci_hi_att
  } else {
    NA
  }

  att_ci_width <- if (!is.na(ci_lo_att) && !is.na(ci_hi_att)) {
    ci_hi_att - ci_lo_att
  } else {
    NA
  }

  tibble::tibble(
    dgp_id         = dgp_id,
    estimator_id   = estimator_id,
    n              = as.integer(n),
    seed           = as.integer(seed),
    oracle         = est_desc$oracle,
    supports_qst   = est_desc$supports_qst,
    true_att       = true_att,
    est_att        = est_att,
    att_error      = att_error,
    att_abs_error  = att_abs_error,
    att_ci_lo      = ci_lo_att,
    att_ci_hi      = ci_hi_att,
    att_covered    = att_covered,
    att_ci_width   = att_ci_width,
    n_boot_ok      = n_ok,
    estimator_pkgs = estimator_pkgs
  )
}


#' Run a DGP × estimator combination over multiple seeds
#'
#' This function repeatedly calls [cs_run_single()] for a given DGP and
#' estimator, using a vector of seeds. It returns a tibble with one row
#' per seed and the same columns as [cs_run_single()].
#'
#' @param dgp_id Character scalar, identifier of the DGP (e.g., "synth_baseline").
#' @param estimator_id Character scalar, identifier of the estimator (e.g., "oracle_att").
#' @param n Integer, number of observations to generate per seed.
#' @param seeds Integer vector of seeds to use. Each seed produces one row
#'   in the output tibble.
#' @param tau Numeric vector of quantile levels. Passed through to the
#'   estimator via [cs_run_single()]. Default is [cs_tau_oracle].
#' @param config List of estimator-specific configuration options. Passed
#'   through to the estimator via [cs_run_single()].
#'
#' @return A tibble with one row per seed and at least the columns returned
#'   by [cs_run_single()], including `dgp_id`, `estimator_id`, `n`, `seed`,
#'   `oracle`, `supports_qst`, `true_att`, `est_att`, `att_error`,
#'   `att_abs_error`.
#'
#' @export
cs_run_seeds <- function(
  dgp_id,
  estimator_id,
  n,
  seeds,
  tau       = cs_tau_oracle,
  bootstrap = FALSE,
  B         = 200L,
  config    = list()
) {
  if (length(seeds) == 0L) {
    rlang::abort(
      message = "`seeds` must have length >= 1.",
      class   = "causalstress_runner_error"
    )
  }
  if (!is.numeric(seeds) && !is.integer(seeds)) {
    rlang::abort(
      message = "`seeds` must be numeric or integer.",
      class   = "causalstress_runner_error"
    )
  }
  if (any(!is.finite(seeds))) {
    rlang::abort(
      message = "`seeds` must be finite.",
      class   = "causalstress_runner_error"
    )
  }
  seeds <- as.integer(seeds)

  rows <- lapply(seeds, function(s) {
    cs_run_single(
      dgp_id       = dgp_id,
      estimator_id = estimator_id,
      n            = n,
      seed         = s,
      tau          = tau,
      bootstrap    = bootstrap,
      B            = B,
      config       = config
    )
  })

  tibble::as_tibble(do.call(rbind, rows))
}
