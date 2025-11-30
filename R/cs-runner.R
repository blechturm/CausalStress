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
  B         = 0L,
  config    = list(),
  board     = NULL,
  ...
) {
  cs_chk_scalar_numeric(n, "n")
  if (n <= 0) {
    rlang::abort(
      "`n` must be a positive scalar.",
      class = "causalstress_runner_error"
    )
  }

  logs <- character(0L)
  warnings_vec <- character(0L)
  errors_vec <- character(0L)
  boot_draws <- NULL
  ci_lo_att <- NA_real_
  ci_hi_att <- NA_real_
  att_covered <- NA
  n_boot_ok <- 0L
  success <- TRUE
  collect_msg <- function(e) {
    logs <<- c(logs, paste0("[message] ", conditionMessage(e)))
    invokeRestart("muffleMessage")
  }
  collect_warn <- function(e) {
    logs <<- c(logs, paste0("[warning] ", conditionMessage(e)))
    warnings_vec <<- c(warnings_vec, conditionMessage(e))
    invokeRestart("muffleWarning")
  }

  # Look up DGP + estimator descriptors
  dgp_desc <- cs_get_dgp(dgp_id)
  est_desc <- cs_get_estimator(estimator_id)

  dgp <- dgp_desc$generator(n = n, seed = seed)
  cs_check_dgp_synthetic(dgp)

  df_raw    <- dgp$df
  true_att  <- dgp$true_att

  oracle_allowed <- isTRUE(est_desc$oracle) || isTRUE(config$use_oracle)
  df_run <- cs_airlock(df_raw, oracle_allowed = oracle_allowed)

  # Run estimator
  res <- tryCatch(
    withCallingHandlers(
      est_desc$generator(
        df     = df_run,
        config = config,
        tau    = tau,
        ...
      ),
      message = collect_msg,
      warning = collect_warn
    ),
    error = function(e) {
      logs <<- c(logs, paste0("[error] ", conditionMessage(e)))
      errors_vec <<- c(errors_vec, conditionMessage(e))
      success <<- FALSE
      stop(e)
    }
  )
  if (success) {
    cs_check_estimator_output(res, require_qst = est_desc$supports_qst, tau = tau)
  }

  # Extract ATT estimate (works for tibble or list)
  att <- res$att
  if (is.data.frame(att)) {
    est_att <- att[["estimate"]]
  } else {
    est_att <- att$estimate
  }

  att_error      <- est_att - true_att
  att_abs_error  <- abs(att_error)

  if (bootstrap) {
    n_obs <- nrow(df_run)
    boot_att <- numeric(B)
    for (b in seq_len(B)) {
      idx_b <- sample.int(n_obs, size = n_obs, replace = TRUE)
      df_b  <- df_run[idx_b, , drop = FALSE]
      res_b <- try(
        withCallingHandlers(
          est_desc$generator(df = df_b, config = config, tau = tau, ...),
          message = collect_msg,
          warning = collect_warn
        ),
        silent = TRUE
      )
      if (inherits(res_b, "try-error")) {
        logs <<- c(logs, paste0("[error] bootstrap[", b, "]: ", conditionMessage(attr(res_b, "condition"))))
        next
      }
      att_b <- if (is.data.frame(res_b$att)) res_b$att[["estimate"]] else res_b$att$estimate
      if (is.finite(att_b)) {
        n_boot_ok <- n_boot_ok + 1L
        boot_att[n_boot_ok] <- att_b
      }
    }
  if (n_boot_ok > 0L) {
    boot_att <- boot_att[seq_len(n_boot_ok)]
    ci_lo_att <- stats::quantile(boot_att, 0.025, na.rm = TRUE)
    ci_hi_att <- stats::quantile(boot_att, 0.975, na.rm = TRUE)
    boot_draws <- tibble::tibble(
        b       = seq_along(boot_att),
        est_att = boot_att
      )
      att_covered <- !is.na(true_att) &&
        !is.na(ci_lo_att) &&
        !is.na(ci_hi_att) &&
        true_att >= ci_lo_att &&
        true_att <= ci_hi_att
    }
  }

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

  log_str <- if (length(logs) == 0L) NA_character_ else paste(logs, collapse = "\n")

  log_str <- if (length(logs) == 0L) NA_character_ else paste(logs, collapse = "\n")

  result <- list(
    att = list(
      estimate     = est_att,
      true         = true_att,
      error        = att_error,
      abs_error    = att_abs_error,
      ci_lo        = ci_lo_att,
      ci_hi        = ci_hi_att,
      boot_covered = att_covered,
      ci_width     = if (!is.na(ci_lo_att) && !is.na(ci_hi_att)) ci_hi_att - ci_lo_att else NA_real_
    ),
    qst        = res$qst %||% NULL,
    boot_draws = boot_draws,
    meta = list(
      dgp_id         = dgp_id,
      estimator_id   = estimator_id,
      n              = as.integer(n),
      seed           = as.integer(seed),
      oracle         = est_desc$oracle,
      supports_qst   = est_desc$supports_qst,
      estimator_pkgs = estimator_pkgs,
      n_boot_ok      = n_boot_ok,
      log            = log_str
    )
  )

  if (!is.null(board)) {
    cs_pin_write(board = board, result = result)
  }

  result
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
  config    = list(),
  board     = NULL,
  skip_existing = FALSE,
  show_progress = TRUE
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

  run_block <- function() {
    p <- progressr::progressor(steps = length(seeds))

    rows <- lapply(seeds, function(s) {
      msg <- glue::glue("{dgp_id} | {estimator_id} | seed {s}")
      on.exit(p(message = msg), add = TRUE)
      if (!is.null(board) && isTRUE(skip_existing)) {
        if (cs_pin_exists(board, dgp_id, estimator_id, n, s)) {
          name <- glue::glue(
            "results__dgp={dgp_id}__est={estimator_id}__n={n}__seed={s}"
          )
          cached <- pins::pin_read(board, name)
          has_ci <- function(run_row) {
            n_ok <- tryCatch(run_row$n_boot_ok, error = function(...) NA)
            lo <- tryCatch(run_row$att_ci_lo, error = function(...) NA)
            hi <- tryCatch(run_row$att_ci_hi, error = function(...) NA)
            if (is.null(lo) || is.null(hi)) return(FALSE)
            if (is.na(n_ok) || n_ok == 0L) return(FALSE)
            if (all(is.na(lo)) || all(is.na(hi))) return(FALSE)
            TRUE
          }
          if (isTRUE(bootstrap) && B > 0 && !has_ci(cached)) {
            stop(
              "Existing run found for this (dgp_id, estimator_id, n, seed) ",
              "but it was computed without bootstrap CIs, while you requested ",
              "bootstrap = TRUE, B = ", B, ". Use a fresh board or set ",
              "skip_existing = FALSE to recompute.",
              call. = FALSE
            )
          }
          return(cs_result_to_row(cached))
        }
      }

      res <- cs_run_single(
        dgp_id       = dgp_id,
        estimator_id = estimator_id,
        n            = n,
        seed         = s,
        tau          = tau,
        bootstrap    = bootstrap,
        B            = B,
        config       = config,
        board        = board
      )
      cs_result_to_row(res)
    })

    tibble::as_tibble(do.call(rbind, rows))
  }

  if (isTRUE(show_progress)) {
    progressr::with_progress(run_block())
  } else {
    run_block()
  }
}

#' Run a campaign of seeds for a single DGP–estimator pair
#'
#' `cs_run_campaign()` is an alias for [cs_run_seeds()] kept for compatibility
#' with design documents and early drafts of the API.
#'
#' @inheritParams cs_run_seeds
#' @return A tibble of runs, identical to [cs_run_seeds()].
#' @export
cs_run_campaign <- function(...) {
  cs_run_seeds(...)
}
