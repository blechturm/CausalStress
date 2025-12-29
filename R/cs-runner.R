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
  # RNG lock (Constitution Article II): one lock at the highest runner boundary per task.
  cs_set_rng(seed)

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

  exec_meta <- cs_dgp_executable_meta(dgp_id = dgp_id, version = dgp_desc$version[[1L]])

  dgp_generator <- dgp_desc$generator[[1]]
  t_dgp_start <- Sys.time()
  dgp <- dgp_generator(n = n, seed = seed)
  run_time_dgp <- as.numeric(difftime(Sys.time(), t_dgp_start, units = "secs"))
  cs_check_dgp_synthetic(dgp)

  df_raw    <- dgp$df
  true_att  <- dgp$true_att

  oracle_allowed <- isTRUE(est_desc$oracle)
  df_run <- cs_airlock(df_raw, oracle_allowed = oracle_allowed)

  config_local <- config
  if (is.null(config_local$seed)) {
    config_local$seed <- seed
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
    tau               = tau,
    max_runtime       = max_runtime
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
  res_meta <- res$meta %||% list()
  n_boot_ok <- res_meta$n_boot_ok %||% 0L
  n_boot_fail <- res_meta$n_boot_fail %||% 0L
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

    if (!"tau_id" %in% names(qst_df)) {
      qst_df$tau_id <- cs_tau_id(qst_df$tau)
    }

    if (!is.null(dgp$true_qst)) {
      truth_tbl <- dgp$true_qst
      if ("value" %in% names(truth_tbl)) {
        truth_tbl <- dplyr::rename(truth_tbl, true = value)
      }
      if (!"tau_id" %in% names(truth_tbl)) {
        truth_tbl$tau_id <- cs_tau_id(truth_tbl$tau)
      }
      truth_tbl <- truth_tbl %>%
        dplyr::select(.data$tau_id, .data$true)

      qst_df <- qst_df %>%
        dplyr::left_join(truth_tbl, by = "tau_id") %>%
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
    if (!"ci_width" %in% names(qst_df)) qst_df$ci_width <- NA_real_
    if (!"covered" %in% names(qst_df)) qst_df$covered <- NA

    # If estimator exposes QST CIs as qst_ci_* columns, map them into the
    # standardized ci_* fields used by downstream summaries/governance.
    if ("qst_ci_lo" %in% names(qst_df) && all(is.na(qst_df$ci_lo))) {
      qst_df$ci_lo <- qst_df$qst_ci_lo
    }
    if ("qst_ci_hi" %in% names(qst_df) && all(is.na(qst_df$ci_hi))) {
      qst_df$ci_hi <- qst_df$qst_ci_hi
    }
    if (all(is.na(qst_df$ci_width))) {
      qst_df$ci_width <- ifelse(
        !is.na(qst_df$ci_lo) & !is.na(qst_df$ci_hi),
        qst_df$ci_hi - qst_df$ci_lo,
        NA_real_
      )
    }
    if (all(is.na(qst_df$covered))) {
      qst_df$covered <- ifelse(
        !is.na(qst_df$true) & !is.na(qst_df$ci_lo) & !is.na(qst_df$ci_hi),
        qst_df$true >= qst_df$ci_lo & qst_df$true <= qst_df$ci_hi,
        NA
      )
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

  # Non-deterministic provenance is stored separately from the science payload.
  ts_now <- Sys.time()

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
      dgp_version    = dgp_desc$version[[1L]] %||% NA_character_,
      dgp_status     = dgp_desc$status[[1L]] %||% NA_character_,
      dgp_design_spec = dgp_desc$design_spec[[1L]] %||% NA_character_,
      estimator_version = est_desc$version %||% NA_character_,
      config_fingerprint_schema = 2L,
      estimator_pkgs = estimator_pkgs,
      n_boot_ok      = n_boot_ok,
      n_boot_fail    = n_boot_fail,
      ci_method      = res_meta$ci_method %||% NA_character_,
      ci_type        = res_meta$ci_type %||% NA_character_,
      ci_level       = res_meta$ci_level %||% NA_real_,
      ci_valid       = res_meta$ci_valid %||% NA,
      ci_fail_code   = res_meta$ci_fail_code %||% NA_character_,
      ci_valid_by_dim = res_meta$ci_valid_by_dim %||% logical(0),
      collapsed      = res_meta$collapsed %||% logical(0),
      qst_ci_method       = res_meta$qst_ci_method %||% NA_character_,
      qst_ci_type         = res_meta$qst_ci_type %||% NA_character_,
      qst_ci_level        = res_meta$qst_ci_level %||% NA_real_,
      qst_ci_valid        = res_meta$qst_ci_valid %||% NA,
      qst_ci_fail_code    = res_meta$qst_ci_fail_code %||% NA_character_,
      qst_ci_valid_by_dim = res_meta$qst_ci_valid_by_dim %||% logical(0),
      qst_ci_collapsed    = res_meta$qst_ci_collapsed %||% logical(0),
      log            = log_str,
      warnings       = warnings_vec,
      dgp_params         = list(n = n, seed = seed),
      estimator_params   = config,
      config_fingerprint = config_fingerprint
    ),
    provenance = list(
      timestamp         = ts_now,
      run_timestamp      = ts_now,
      max_runtime        = max_runtime,
      dgp_noise_family   = exec_meta$noise_family %||% NA_character_,
      dgp_effect_type    = exec_meta$effect_type %||% NA_character_,
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
  experimental_parallel = FALSE,
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

  cs_require_experimental_parallel(parallel = parallel, experimental_parallel = experimental_parallel)
  cs_require_staging_for_parallel_persistence(parallel = parallel, board = board, staging_dir = staging_dir)

  parallel_warning_emitted <- FALSE
  parallel_backend <- NA_character_
  if (isTRUE(parallel) && isTRUE(experimental_parallel)) {
    rlang::warn(
      "Experimental parallel execution enabled for this seeds runner call.",
      class = "causalstress_experimental_parallel"
    )
    parallel_warning_emitted <- TRUE
    parallel_backend <- cs_parallel_backend_string()
  }

  if (!is.null(staging_dir) && !is.null(board)) {
    dir.create(staging_dir, recursive = TRUE, showWarnings = FALSE)
    # Recovery: gather any staged files from prior crash before running new seeds
    cs_gather_results(board, staging_dir)
  }

  est_desc <- cs_get_estimator(estimator_id)

  pin_name_for_seed <- function(s) {
    glue::glue(
      "results__dgp={dgp_id}__est={estimator_id}__n={n}__seed={s}"
    )
  }

  apply_runner_defaults <- function(cfg, seed_i) {
    if (is.null(cfg$seed)) {
      cfg$seed <- seed_i
    }
    if (isTRUE(bootstrap) && B > 0L && is.null(cfg$n_boot)) {
      cfg$n_boot <- B
    }
    cfg
  }

  build_expected_fp_schema2 <- function(seed_i) {
    expected_cfg <- apply_runner_defaults(config, seed_i)
    cs_build_config_fingerprint(
      dgp_id            = dgp_id,
      estimator_id      = estimator_id,
      n                 = n,
      seed              = seed_i,
      bootstrap         = bootstrap,
      B                 = B,
      oracle            = isTRUE(est_desc$oracle),
      estimator_version = est_desc$version,
      config            = expected_cfg,
      tau               = tau,
      max_runtime       = max_runtime
    )
  }

  build_expected_fp_legacy <- function(seed_i) {
    if (is.finite(max_runtime)) {
      rlang::abort(
        message = "Cannot resume legacy (v0.1.7) pins with non-infinite `max_runtime`; legacy fingerprints do not encode runtime guards.",
        class   = "causalstress_fingerprint_error"
      )
    }
    expected_cfg <- apply_runner_defaults(config, seed_i)
    cs_build_config_fingerprint_legacy(
      dgp_id            = dgp_id,
      estimator_id      = estimator_id,
      n                 = n,
      seed              = seed_i,
      bootstrap         = bootstrap,
      B                 = B,
      oracle            = isTRUE(est_desc$oracle),
      estimator_version = est_desc$version,
      config            = expected_cfg,
      tau               = tau
    )
  }

  should_try_cache <- isTRUE(skip_existing) && !isTRUE(force)
  cached_seeds <- integer(0)
  seeds_to_run <- seeds

  if (!is.null(board) && isTRUE(should_try_cache)) {
    cached <- logical(length(seeds))
    for (i in seq_along(seeds)) {
      s <- seeds[[i]]
      if (!cs_pin_exists(board, dgp_id, estimator_id, n, s)) next

      name <- pin_name_for_seed(s)
      meta_obj <- pins::pin_meta(board, name)
      md <- cs_pin_meta_user_or_metadata(meta_obj)
      stored_fp <- md$config_fingerprint %||% NULL
      stored_schema <- suppressWarnings(as.integer(md$config_fingerprint_schema %||% NA_integer_))
      expected_fp <- if (is.na(stored_schema) || stored_schema == 1L) {
        build_expected_fp_legacy(s)
      } else if (stored_schema == 2L) {
        build_expected_fp_schema2(s)
      } else {
        rlang::abort(
          message = glue::glue("Unsupported config fingerprint schema: {stored_schema}."),
          class   = "causalstress_fingerprint_error"
        )
      }

      if (is.null(stored_fp) || !identical(stored_fp, expected_fp)) {
        old_txt <- if (is.null(stored_fp)) "missing" else stored_fp
        stop(
          "Configuration fingerprint mismatch for ",
          dgp_id, " x ", estimator_id, " seed ", s, ". ",
          "(Stored: ", old_txt, ", Current: ", expected_fp, "). ",
          "To overwrite this run with new settings, set force = TRUE or skip_existing = FALSE.",
          call. = FALSE
        )
      }

      if (isTRUE(bootstrap) && B > 0L && !cs_has_boot_ci_meta(md)) {
        stop(
          "Existing run found for this (dgp_id, estimator_id, n, seed) ",
          "but it was computed without bootstrap CIs, while you requested ",
          "bootstrap = TRUE, B = ", B, ". Use a fresh board or set ",
          "skip_existing = FALSE to recompute.",
          call. = FALSE
        )
      }

      cached[[i]] <- TRUE
    }

    cached_seeds <- seeds[cached]
    seeds_to_run <- seeds[!cached]
  }

  # If we are forcing recompute and a pin exists, delete it to avoid stale metadata
  if (!is.null(board) && !isTRUE(should_try_cache)) {
    for (s in seeds_to_run) {
      if (!cs_pin_exists(board, dgp_id, estimator_id, n, s)) next
      pins::pin_delete(board, pin_name_for_seed(s))
    }
  }

  cached_rows <- list()
  if (!is.null(board) && length(cached_seeds) > 0L) {
    cached_rows <- lapply(cached_seeds, function(s) {
      cached <- pins::pin_read(board, pin_name_for_seed(s))
      cs_result_to_row(cached)
    })
  }

  run_one_seed <- function(s, p = NULL) {
    tick <- function() {
      if (!is.null(p)) p(message = glue::glue("seed {s} done"))
    }
    on.exit(tick(), add = TRUE)

    worker_board <- if (isTRUE(parallel) || !is.null(staging_dir)) NULL else board

    config_eff <- config
    requested_threads <- suppressWarnings(as.integer(config_eff$num_threads %||% NA_integer_))
    effective_threads <- requested_threads

    wrap_call <- function(expr) expr
    if (isTRUE(parallel) && isTRUE(experimental_parallel)) {
      config_eff$num_threads <- 1L
      effective_threads <- 1L
      wrap_call <- function(expr) cs_with_envvar(cs_thread_caps_env(), expr)
    }

    res <- wrap_call(
      CausalStress::cs_run_single(
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
        config       = config_eff,
        board        = worker_board,
        max_runtime  = max_runtime
      )
    )

    if (!is.null(res$provenance) && isTRUE(parallel) && isTRUE(experimental_parallel)) {
      res$provenance$experimental_parallel <- TRUE
      res$provenance$parallel_warning_emitted <- isTRUE(parallel_warning_emitted)
      res$provenance$parallel_backend <- parallel_backend
      res$provenance$thread_caps_applied <- TRUE
      res$provenance$thread_caps_env <- cs_thread_caps_env()
      res$provenance$requested_num_threads <- requested_threads
      res$provenance$effective_num_threads <- effective_threads
      res$provenance$staging_dir_used <- !is.null(staging_dir)
    }
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
    p <- if (isTRUE(show_progress)) progressr::progressor(steps = length(seeds_to_run) + 1L) else NULL

    if (isTRUE(parallel)) {
      rows <- furrr::future_map(
        seeds_to_run,
        run_one_seed,
        p = p,
        .options  = furrr::furrr_options(seed = TRUE, packages = "CausalStress"),
        .progress = FALSE
      )
      out <- tibble::as_tibble(dplyr::bind_rows(rows))
    } else {
      rows <- lapply(seeds_to_run, function(s) run_one_seed(s, p = p))
      out <- tibble::as_tibble(do.call(rbind, rows))
    }

    if (!is.null(staging_dir) && !is.null(board)) {
      gathered <- cs_gather_results(board, staging_dir)
      if (!is.null(p)) p(message = glue::glue("Gathered {gathered} staged results"))
    } else if (!is.null(p)) {
      p(message = "Gathering results...")
    }

    out_all <- dplyr::bind_rows(cached_rows, out)
    dplyr::arrange(out_all, seed)
  }

  if (isTRUE(show_progress)) {
    progressr::with_progress(run_with_progress())
  } else {
    run_with_progress()
  }
}

