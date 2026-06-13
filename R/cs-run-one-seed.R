cs_run_one_seed_internal <- function(dgp_id,
                                     estimator_id,
                                     n,
                                     seed,
                                     version = NULL,
                                     status = "stable",
                                     tau = cs_tau_oracle,
                                     bootstrap = FALSE,
                                     B = 0L,
                                     config = list(),
                                     board = NULL,
                                     skip_existing = FALSE,
                                     force = FALSE,
                                     quiet = TRUE,
                                     max_runtime = Inf,
                                     parallel = FALSE,
                                     experimental_parallel = FALSE,
                                     parallel_backend = NA_character_,
                                     parallel_warning_emitted = FALSE,
                                     staging_dir = NULL,
                                     p = NULL,
                                     ...) {
  cs_require_experimental_parallel(parallel = parallel, experimental_parallel = experimental_parallel)
  cs_require_staging_for_parallel_persistence(parallel = parallel, board = board, staging_dir = staging_dir)

  dgp_desc <- cs_get_dgp(dgp_id = dgp_id, version = version, status = status, quiet = TRUE)
  dgp_version <- dgp_desc$version[[1L]]
  est_desc <- cs_get_estimator(estimator_id)

  should_try_cache <- isTRUE(skip_existing) && !isTRUE(force)
  if (!is.null(board) && isTRUE(should_try_cache)) {
      name <- cs_find_result_pin(
        board = board,
        dgp_id = dgp_id,
        dgp_version = dgp_version,
        estimator_id = estimator_id,
        n = n,
        seed = seed
      )
      if (!is.na(name)) {
        meta_obj <- pins::pin_meta(board, name)
        md <- cs_pin_meta_user_or_metadata(meta_obj)
      stored_fp <- md$config_fingerprint %||% NULL
      stored_schema <- suppressWarnings(as.integer(md$config_fingerprint_schema %||% NA_integer_))
      expected_fp <- if (is.na(stored_schema) || stored_schema == 1L) {
        if (is.finite(max_runtime)) {
          rlang::abort(
            message = "Cannot resume legacy (v0.1.7) pins with non-infinite `max_runtime`; legacy fingerprints do not encode runtime guards.",
            class   = "causalstress_fingerprint_error"
          )
        }
        cs_build_config_fingerprint_legacy(
          dgp_id            = dgp_id,
          estimator_id      = estimator_id,
          n                 = n,
          seed              = seed,
          bootstrap         = bootstrap,
          B                 = B,
          oracle            = isTRUE(est_desc$oracle),
          estimator_version = est_desc$version,
          config            = config,
          tau               = tau
        )
      } else if (stored_schema == 2L) {
        stored_dgp_version <- as.character(md$dgp_version %||% NA_character_)
        if (is.na(stored_dgp_version) || !identical(stored_dgp_version, as.character(dgp_version))) {
          rlang::abort(
            message = "Cannot resume schema-2 pin because its DGP version metadata does not match the resolved DGP version.",
            class   = "causalstress_fingerprint_error"
          )
        }
        cs_build_config_fingerprint_schema2(
          dgp_id            = dgp_id,
          estimator_id      = estimator_id,
          n                 = n,
          seed              = seed,
          bootstrap         = bootstrap,
          B                 = B,
          oracle            = isTRUE(est_desc$oracle),
          estimator_version = est_desc$version,
          config            = config,
          tau               = tau,
          max_runtime       = max_runtime
        )
      } else if (stored_schema == 3L) {
        cs_build_config_fingerprint(
          dgp_id            = dgp_id,
          estimator_id      = estimator_id,
          n                 = n,
          seed              = seed,
          bootstrap         = bootstrap,
          B                 = B,
          oracle            = isTRUE(est_desc$oracle),
          estimator_version = est_desc$version,
          config            = config,
          tau               = tau,
          max_runtime       = max_runtime,
          dgp_version       = dgp_version
        )
      } else {
        rlang::abort(
          message = glue::glue("Unsupported config fingerprint schema: {stored_schema}."),
          class   = "causalstress_fingerprint_error"
        )
      }
        if (!is.null(stored_fp) && identical(stored_fp, expected_fp)) {
          cached <- pins::pin_read(board, name)
          tidy_row <- cs_result_to_row(cached)
          if (!is.null(p)) p(message = glue::glue("seed {seed} done (cached)"))
          return(tidy_row)
        } else {
          old_txt <- if (is.null(stored_fp)) "missing" else stored_fp
          stop(
            "Configuration fingerprint mismatch for ",
            dgp_id, " x ", estimator_id, " seed ", seed, ". ",
            "(Stored: ", old_txt, ", Current: ", expected_fp, "). ",
            "To overwrite this run with new settings, set force = TRUE or skip_existing = FALSE.",
            call. = FALSE
          )
        }
      }
    }

  if (!is.null(board) && !isTRUE(should_try_cache) &&
      cs_pin_exists(board, dgp_id, estimator_id, n, seed, dgp_version = dgp_version, include_legacy = FALSE)) {
    pins::pin_delete(board, cs_find_result_pin(
      board = board,
      dgp_id = dgp_id,
      dgp_version = dgp_version,
      estimator_id = estimator_id,
      n = n,
      seed = seed,
      include_legacy = FALSE
    ))
    }

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
      seed         = seed,
      version      = version,
      status       = status,
      quiet        = quiet,
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
  if (!is.null(p)) p(message = glue::glue("seed {seed} done"))
  cs_result_to_row(res)
}
