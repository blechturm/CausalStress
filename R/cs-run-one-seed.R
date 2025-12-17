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
                                     staging_dir = NULL,
                                     p = NULL,
                                     ...) {
  if (isTRUE(parallel) && !is.null(board) && is.null(staging_dir)) {
    cli::cli_abort(
      c(
        "Parallel execution with persistence requires a staging directory.",
        "i" = "Set `staging_dir` when using `parallel = TRUE` with a non-NULL `board`."
      )
    )
  }

  # Phase 1.1: RNG Locking (Constitution Article II) -- enforce deterministic RNG per seed
  CausalStress::cs_set_rng(seed)

  est_desc <- cs_get_estimator(estimator_id)

  should_try_cache <- isTRUE(skip_existing) && !isTRUE(force)
  if (!is.null(board) && isTRUE(should_try_cache)) {
      if (cs_pin_exists(board, dgp_id, estimator_id, n, seed)) {
        name <- glue::glue(
          "results__dgp={dgp_id}__est={estimator_id}__n={n}__seed={seed}"
        )
        meta_obj <- pins::pin_meta(board, name)
        md <- meta_obj$metadata %||% meta_obj$user %||% list()
      stored_fp <- md$config_fingerprint %||% NULL
      expected_fp <- cs_build_config_fingerprint(
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
      cs_pin_exists(board, dgp_id, estimator_id, n, seed)) {
    pins::pin_delete(
      board,
      glue::glue("results__dgp={dgp_id}__est={estimator_id}__n={n}__seed={seed}")
    )
    }

  worker_board <- if (isTRUE(parallel) || !is.null(staging_dir)) NULL else board

  res <- CausalStress::cs_run_single(
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
    config       = config,
    board        = worker_board,
    max_runtime  = max_runtime
  )
  if (!is.null(staging_dir)) {
    cs_stage_result(res, staging_dir)
  } else if (!is.null(board)) {
    cs_pin_write(board = board, result = res)
  }
  if (!is.null(p)) p(message = glue::glue("seed {seed} done"))
  cs_result_to_row(res)
}
