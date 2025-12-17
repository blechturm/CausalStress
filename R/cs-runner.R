#' Run a single DGP/estimator combination for one seed
#'
#' @param dgp_id Character scalar, identifier of the DGP (e.g., "synth_baseline").
#' @param estimator_id Character scalar, identifier of the estimator (e.g., "lm_att").
#' @param n Integer sample size for the run.
#' @param seed Integer seed for reproducibility.
#' @param version Optional character version string for the DGP. Defaults to latest stable via registry.
#' @param status Optional status filter for DGP lookup ("stable", "experimental", "deprecated", "invalidated").
#' @param quiet Logical; if `TRUE`, suppress warnings from DGP lookup (use with
#'   care—defaults to `FALSE` to honor loud-warning protocol).
#' @export
cs_run_single <- function(
  dgp_id,
  estimator_id,
  n,
  seed,
  version   = NULL,
  status    = "stable",
  quiet     = FALSE,
  tau       = cs_tau_oracle,
  bootstrap = FALSE,
  B         = 0L,
  config    = list(),
  board     = NULL,
  max_runtime = Inf,
  ...
) {
  t_total_start <- Sys.time()

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
  dgp_desc <- cs_get_dgp(
    dgp_id  = dgp_id,
    version = version,
    status  = status,
    quiet   = quiet
  )
  est_desc <- cs_get_estimator(estimator_id)

  dgp_generator <- dgp_desc$generator[[1]]
  t_dgp_start <- Sys.time()
  dgp <- dgp_generator(n = n, seed = seed)
  run_time_dgp <- as.numeric(difftime(Sys.time(), t_dgp_start, units = "secs"))
  cs_check_dgp_synthetic(dgp)

  df_raw    <- dgp$df
  true_att  <- dgp$true_att

  oracle_allowed <- isTRUE(est_desc$oracle) || isTRUE(config$use_oracle)
  df_run <- cs_airlock(df_raw, oracle_allowed = oracle_allowed)

  config_local <- config
  if (is.null(config_local$seed)) {
    config_local$seed <- seed
  }
  if (is.null(config_local$ci_method)) {
    config_local$ci_method <- if (isTRUE(bootstrap) && B > 0L) "bootstrap" else "none"
  }
  if (isTRUE(bootstrap) && B > 0L && is.null(config_local$n_boot)) {
    config_local$n_boot <- B
  }

  config_fingerprint <- cs_build_config_fingerprint(
    dgp_id            = dgp_id,
    estimator_id      = estimator_id,
    n                 = n,
    seed              = seed,
    bootstrap         = bootstrap,
    B                 = B,
    oracle            = oracle_allowed,
    estimator_version = est_desc$version,
    config            = config_local,
    tau               = tau
  )

  # Run estimator
  t_est_start <- Sys.time()
  res <- NULL
  config <- config_local
  res <- tryCatch(
    withCallingHandlers(
      {
        setTimeLimit(cpu = max_runtime, elapsed = max_runtime, transient = TRUE)
        on.exit(setTimeLimit(cpu = Inf, elapsed = Inf, transient = TRUE), add = TRUE)
        est_desc$generator(
          df     = df_run,
          config = config,
          tau    = tau,
          ...
        )
      },
      message = collect_msg,
      warning = collect_warn
    ),
    error = function(e) {
      logs <<- c(logs, paste0("[error] ", conditionMessage(e)))
      errors_vec <<- c(errors_vec, conditionMessage(e))
      success <<- FALSE
      list(att = list(estimate = NA_real_), qst = NULL)
    }
  )
  if (isTRUE(success)) {
    cs_check_estimator_output(res, require_qst = est_desc$supports_qst, tau = tau)
  }
  run_time_est <- as.numeric(difftime(Sys.time(), t_est_start, units = "secs"))

  extracted <- cs_extract_estimator_result(res)
  est_att <- extracted$att

  att_ci <- res$att %||% list()
  ci_lo_att <- att_ci$ci_lo %||% NA_real_
  ci_hi_att <- att_ci$ci_hi %||% NA_real_
  n_boot_ok <- res$meta$n_boot_ok %||% 0L
  boot_draws <- res$boot_draws %||% NULL

  att_error      <- est_att - true_att
  att_abs_error  <- abs(att_error)

  # QST point estimates and truth join
  qst_df <- NULL
  if (!is.null(extracted$qst)) {
    qst_df <- extracted$qst
    if (!"estimate" %in% names(qst_df)) {
      rlang::abort("qst output must contain `estimate` or `value` column.", class = "causalstress_runner_error")
    }

    if (!is.null(dgp$true_qst)) {
      truth_tbl <- dgp$true_qst
      if ("value" %in% names(truth_tbl)) {
        truth_tbl <- dplyr::rename(truth_tbl, true = value)
      }
      qst_df <- qst_df %>%
        dplyr::left_join(truth_tbl, by = "tau") %>%
        dplyr::mutate(
          error = estimate - true,
          abs_error = abs(error)
        )
    } else {
      qst_df <- qst_df %>%
        dplyr::mutate(
          true = NA_real_,
          error = NA_real_,
          abs_error = NA_real_
        )
    }
    if (!"ci_lo" %in% names(qst_df)) qst_df$ci_lo <- NA_real_
    if (!"ci_hi" %in% names(qst_df)) qst_df$ci_hi <- NA_real_
    if (!"covered" %in% names(qst_df)) qst_df$covered <- NA
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

  # Fresh run timestamp (POSIXct with a tiny jitter to avoid deduplication)
  ts_now <- Sys.time() + runif(1, 0, 1e-6)

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
    qst        = qst_df %||% NULL,
    boot_draws = boot_draws,
    meta = list(
      success        = isTRUE(success),
    error          = if (!isTRUE(success)) {
      if (any(grepl("reached elapsed time limit|reached CPU time limit|reached time limit", errors_vec))) {
        "Timeout reached"
      } else if (length(errors_vec) == 0L) {
        NA_character_
      } else {
        paste(errors_vec, collapse = "; ")
      }
    } else {
      NA_character_
    },
      dgp_id         = dgp_id,
      estimator_id   = estimator_id,
      n              = as.integer(n),
      seed           = as.integer(seed),
      oracle         = est_desc$oracle,
      supports_qst   = est_desc$supports_qst,
      estimator_pkgs = estimator_pkgs,
      n_boot_ok      = n_boot_ok,
      log            = log_str,
      warnings       = warnings_vec,
      dgp_params         = list(n = n, seed = seed),
      estimator_params   = config,
      config_fingerprint = config_fingerprint,
      timestamp         = ts_now,
      run_timestamp      = ts_now,
      run_time_dgp       = run_time_dgp,
      run_time_est       = run_time_est,
      run_time_total     = as.numeric(difftime(Sys.time(), t_total_start, units = "secs"))
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
#' @param version Optional DGP version string; forwarded to [cs_get_dgp()].
#' @param status Optional DGP status filter; forwarded to [cs_get_dgp()].
#' @param tau Numeric vector of quantile levels. Passed through to the
#'   estimator via [cs_run_single()]. Default is [cs_tau_oracle].
#' @param config List of estimator-specific configuration options. Passed
#'   through to the estimator via [cs_run_single()].
#' @param force Logical; if `TRUE`, recompute even when pins exist (alias for
#'   setting `skip_existing = FALSE`).
#' @param quiet Logical; if `TRUE`, suppress DGP governance warnings inside
#'   per-seed runs (use with care; pre-flight still warns once).
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
  version   = NULL,
  status    = "stable",
  tau       = cs_tau_oracle,
  bootstrap = FALSE,
  B         = 200L,
  config    = list(),
  board     = NULL,
  force     = FALSE,
  skip_existing = FALSE,
  show_progress = TRUE,
  quiet         = FALSE,
  max_runtime   = Inf,
  parallel      = FALSE,
  staging_dir   = NULL
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

  if (!is.null(staging_dir) && !is.null(board)) {
    dir.create(staging_dir, recursive = TRUE, showWarnings = FALSE)
    # Recovery: gather any staged files from prior crash before running new seeds
    cs_gather_results(board, staging_dir)
  }

  est_desc <- cs_get_estimator(estimator_id)

  run_one_seed <- function(s, p = NULL) {
    tick <- function() {
      if (!is.null(p)) p(message = glue::glue("seed {s} done"))
    }
    on.exit(tick(), add = TRUE)

    should_try_cache <- isTRUE(skip_existing) && !isTRUE(force)
    if (!is.null(board) && isTRUE(should_try_cache)) {
      if (cs_pin_exists(board, dgp_id, estimator_id, n, s)) {
        name <- glue::glue(
          "results__dgp={dgp_id}__est={estimator_id}__n={n}__seed={s}"
        )
        cached <- pins::pin_read(board, name)
        stored_fp <- tryCatch(cached$meta$config_fingerprint, error = function(...) NULL)
        config_cache <- config
        if (is.null(config_cache$seed)) {
          config_cache$seed <- s
        }
        if (is.null(config_cache$ci_method)) {
          config_cache$ci_method <- if (isTRUE(bootstrap) && B > 0L) "bootstrap" else "none"
        }
        if (isTRUE(bootstrap) && B > 0L && is.null(config_cache$n_boot)) {
          config_cache$n_boot <- B
        }
        expected_fp <- cs_build_config_fingerprint(
          dgp_id            = dgp_id,
          estimator_id      = estimator_id,
          n                 = n,
          seed              = s,
          bootstrap         = bootstrap,
          B                 = B,
          oracle            = isTRUE(est_desc$oracle),
          estimator_version = est_desc$version,
          config            = config_cache,
          tau               = tau
        )
        if (!is.null(stored_fp) && identical(stored_fp, expected_fp)) {
          tidy_row <- cs_result_to_row(cached)
          has_ci <- function(run_row) {
            n_ok <- tryCatch(run_row$n_boot_ok, error = function(...) NA)
            lo <- tryCatch(run_row$att_ci_lo, error = function(...) NA)
            hi <- tryCatch(run_row$att_ci_hi, error = function(...) NA)
            if (is.null(lo) || is.null(hi)) return(FALSE)
            if (is.na(n_ok) || n_ok == 0L) return(FALSE)
            if (all(is.na(lo)) || all(is.na(hi))) return(FALSE)
            TRUE
          }
          if (isTRUE(bootstrap) && B > 0 && !has_ci(tidy_row)) {
            stop(
              "Existing run found for this (dgp_id, estimator_id, n, seed) ",
              "but it was computed without bootstrap CIs, while you requested ",
              "bootstrap = TRUE, B = ", B, ". Use a fresh board or set ",
              "skip_existing = FALSE to recompute.",
              call. = FALSE
            )
          }
          return(tidy_row)
        } else {
          old_txt <- if (is.null(stored_fp)) "missing" else stored_fp
          stop(
            "Configuration fingerprint mismatch for ",
            dgp_id, " x ", estimator_id, " seed ", s, ". ",
            "(Stored: ", old_txt, ", Current: ", expected_fp, "). ",
            "To overwrite this run with new settings, set force = TRUE or skip_existing = FALSE.",
            call. = FALSE
          )
        }
      }
    }

    # If we are forcing recompute and a pin exists, delete it to avoid stale metadata
    if (!is.null(board) && !isTRUE(should_try_cache) &&
        cs_pin_exists(board, dgp_id, estimator_id, n, s)) {
      pins::pin_delete(
        board,
        glue::glue("results__dgp={dgp_id}__est={estimator_id}__n={n}__seed={s}")
      )
    }

    worker_board <- if (isTRUE(parallel) || !is.null(staging_dir)) NULL else board

    res <- CausalStress::cs_run_single(
      dgp_id       = dgp_id,
      estimator_id = estimator_id,
      n            = n,
      seed         = s,
      version      = version,
      status       = status,
      quiet        = TRUE,
      tau          = tau,
      bootstrap    = bootstrap,
      B            = B,
      config       = config,
      board        = worker_board,
      max_runtime  = max_runtime
    )
    if (!is.null(staging_dir)) {
      cs_stage_result(res, staging_dir)
    } else if (!is.null(board)) {
      cs_pin_write(board = board, result = res)
    }
    cs_result_to_row(res)
  }

  # pre-flight governance warning (once per call)
  cs_get_dgp(
    dgp_id  = dgp_id,
    version = version,
    status  = status,
    quiet   = FALSE
  )

  run_with_progress <- function() {
    p <- if (isTRUE(show_progress)) progressr::progressor(steps = length(seeds) + 1L) else NULL

    if (isTRUE(parallel)) {
      rows <- furrr::future_map(
        sample(seeds),
        run_one_seed,
        p = p,
        .options  = furrr::furrr_options(seed = TRUE, packages = "CausalStress"),
        .progress = FALSE
      )
      out <- tibble::as_tibble(dplyr::bind_rows(rows))
    } else {
      rows <- lapply(seeds, function(s) run_one_seed(s, p = p))
      out <- tibble::as_tibble(do.call(rbind, rows))
    }

    if (!is.null(staging_dir) && !is.null(board)) {
      gathered <- cs_gather_results(board, staging_dir)
      if (!is.null(p)) p(message = glue::glue("Gathered {gathered} staged results"))
    } else if (!is.null(p)) {
      p(message = "Gathering results...")
    }

    dplyr::arrange(out, seed)
  }

  if (isTRUE(show_progress)) {
    progressr::with_progress(run_with_progress())
  } else {
    run_with_progress()
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
