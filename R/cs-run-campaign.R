#' Run a full campaign across DGPs, estimators, and seeds
#'
#' This runner parallelizes over the full Cartesian product of
#' (dgp_id, estimator_id, seed) with dynamic load balancing, making it
#' the recommended entry point for large heterogeneous campaigns.
#'
#' @param plan Optional plan tibble from `cs_plan_campaign()`. If supplied,
#'   `cs_run_campaign()` will execute the planned batches and ignore the
#'   grid-based arguments.
#' @param dgp_ids Character vector of DGP IDs.
#' @param estimator_ids Character vector of estimator IDs.
#' @param dgp_id Deprecated alias for `dgp_ids` (scalar character).
#' @param estimator_id Deprecated alias for `estimator_ids` (scalar character).
#' @param seeds Integer vector of seeds.
#' @param n Integer sample size per run.
#' @param defaults Optional default estimator config list forwarded to
#'   `cs_run_single()` (e.g., `list(n_boot = 200, num_threads = 1L)`). Common
#'   fields include `ci_method` (see [cs_ci_methods]).
#' @param overrides Optional named list of estimator-specific config overrides
#'   (e.g., `list(tmle_att = list(ci_method = "native"))`). Each override is
#'   merged over `defaults` using `utils::modifyList()`.
#' @param campaign_seed Optional scalar integer used to deterministically
#'   shuffle task execution order for better load balancing without relying on
#'   ambient RNG state. If `NULL`, tasks are executed in deterministic grid
#'   order.
#' @param skip_existing Logical; if TRUE, skip tasks already pinned on `board`.
#' @param board Optional pins board for persistence.
#' @param staging_dir Optional staging directory for crash recovery.
#' @param parallel Logical; if TRUE, uses furrr/future for parallel execution.
#' @param workers Number of parallel workers for plan-based execution.
#' @param show_progress Logical; show progressr-based progress.
#' @param bootstrap Logical; runner-level convenience flag forwarded to
#'   [cs_run_single()]. When `TRUE` and `ci_method` is missing in the resolved
#'   config, bootstrap CIs are requested and the per-task seed is injected (see
#'   [cs_ci_methods]).
#' @param B Integer; convenience alias forwarded to [cs_run_single()] for setting
#'   `config$n_boot` when `bootstrap=TRUE`.
#' @param ... Additional arguments forwarded to cs_run_single() (tau, etc.).
#'
#' @return Tibble with one row per run (grid mode) or invisibly returns the
#'   batch ids executed (plan mode).
#' @export
#'
#' @examples
#' \dontrun{
#' plan <- cs_plan_campaign(
#'   dgp_list = "synth_baseline",
#'   estimator_list = "lm_att",
#'   n_seeds = 1:4,
#'   batch_size = 2,
#'   campaign_seed = 123,
#'   strategy_map = list(defaults = list(n = 200))
#' )
#' cs_run_campaign(plan = plan, staging_dir = "staging_batches", workers = 2)
#' cs_consolidate(staging_dir = "staging_batches", board = pins::board_temp())
#' }
cs_run_campaign <- function(
  plan = NULL,
  dgp_ids = NULL,
  estimator_ids = NULL,
  dgp_id = NULL,
  estimator_id = NULL,
  seeds = NULL,
  n = NULL,
  defaults = list(),
  overrides = list(),
  campaign_seed = NULL,
  version = NULL,
  status = "stable",
  tau = cs_tau_oracle,
  bootstrap = FALSE,
  B = 0L,
  skip_existing = FALSE,
  board = NULL,
  staging_dir = NULL,
  parallel = FALSE,
  experimental_parallel = FALSE,
  workers = parallel::detectCores() - 1L,
  show_progress = TRUE,
  force = FALSE,
  quiet = TRUE,
  max_runtime = Inf,
  ...
) {
  if (!is.null(plan)) {
    if (is.null(staging_dir) || !nzchar(staging_dir)) {
      rlang::abort("`staging_dir` must be provided when plan is supplied.")
    }
    return(cs_run_campaign_plan(
      plan = plan,
      staging_dir = staging_dir,
      board = board,
      workers = workers,
      show_progress = show_progress
    ))
  }
  dots <- list(...)
  # Backward compatibility: `config` / `config_by_estimator` were the previous
  # names for `defaults` / `overrides`. Prefer the new names for clarity.
  if ("config" %in% names(dots)) {
    if (length(defaults) > 0L) {
      rlang::abort("Provide only one of `defaults` or legacy `config`.")
    }
    defaults <- dots$config
    dots$config <- NULL
  }
  if ("config_by_estimator" %in% names(dots)) {
    if (length(overrides) > 0L) {
      rlang::abort("Provide only one of `overrides` or legacy `config_by_estimator`.")
    }
    overrides <- dots$config_by_estimator
    dots$config_by_estimator <- NULL
  }

  # Backward compatibility with legacy argument names
  if (!is.null(dgp_id)) {
    if (!is.null(dgp_ids)) rlang::abort("Provide only one of `dgp_ids` or legacy `dgp_id`.")
    dgp_ids <- dgp_id
  }
  if (!is.null(estimator_id)) {
    if (!is.null(estimator_ids)) rlang::abort("Provide only one of `estimator_ids` or legacy `estimator_id`.")
    estimator_ids <- estimator_id
  }
  if (is.null(dgp_ids) || length(dgp_ids) == 0L) {
    rlang::abort("`dgp_ids` must be a non-empty character vector.", class = "causalstress_contract_error")
  }
  if (is.null(estimator_ids) || length(estimator_ids) == 0L) {
    rlang::abort("`estimator_ids` must be a non-empty character vector.", class = "causalstress_contract_error")
  }
  if (is.null(seeds) || length(seeds) == 0L) {
    rlang::abort("`seeds` must be a non-empty integer vector.", class = "causalstress_contract_error")
  }
  if (is.null(n) || length(n) != 1L || !is.finite(n)) {
    rlang::abort("`n` must be a finite numeric scalar.", class = "causalstress_contract_error")
  }

  tasks <- tidyr::expand_grid(
    dgp_id       = dgp_ids,
    estimator_id = estimator_ids,
    seed         = seeds
  ) %>%
    dplyr::mutate(n = n)

  if (!is.null(campaign_seed) && (!is.numeric(campaign_seed) || length(campaign_seed) != 1L || !is.finite(campaign_seed))) {
    rlang::abort("`campaign_seed` must be a finite numeric scalar or NULL.", class = "causalstress_contract_error")
  }

  cs_require_experimental_parallel(parallel = parallel, experimental_parallel = experimental_parallel)
  cs_require_staging_for_parallel_persistence(parallel = parallel, board = board, staging_dir = staging_dir)

  parallel_warning_emitted <- FALSE
  parallel_backend <- NA_character_
  if (isTRUE(parallel) && isTRUE(experimental_parallel)) {
    rlang::warn(
      "Experimental parallel execution enabled for this campaign call.",
      class = "causalstress_experimental_parallel"
    )
    parallel_warning_emitted <- TRUE
    parallel_backend <- cs_parallel_backend_string()
  }

  if (!is.null(staging_dir) && !is.null(board)) {
    dir.create(staging_dir, recursive = TRUE, showWarnings = FALSE)
    cs_gather_results(board, staging_dir)
  }

  resolve_config <- function(est_id) {
    cfg <- defaults
    if (!is.null(est_id) && est_id %in% names(overrides)) {
      cfg <- utils::modifyList(cfg, overrides[[est_id]])
    }
    cfg
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

  # Skip existing pins if requested (with fingerprint/CI checks)
  should_try_cache <- isTRUE(skip_existing) && !isTRUE(force)
  if (isTRUE(should_try_cache) && !is.null(board)) {
    skip_vec <- logical(nrow(tasks))

    for (i in seq_len(nrow(tasks))) {
      dgp_id_i  <- tasks$dgp_id[i]
      est_id_i  <- tasks$estimator_id[i]
      n_i       <- tasks$n[i]
      seed_i    <- tasks$seed[i]
      dgp_desc_i <- cs_get_dgp(dgp_id_i, version = version, status = status, quiet = TRUE)
      dgp_version_i <- dgp_desc_i$version[[1L]]

      name <- cs_find_result_pin(
        board = board,
        dgp_id = dgp_id_i,
        dgp_version = dgp_version_i,
        estimator_id = est_id_i,
        n = n_i,
        seed = seed_i
      )
      if (!is.na(name)) {
        meta_obj <- pins::pin_meta(board, name)
        md <- cs_pin_meta_user_or_metadata(meta_obj)
        stored_fp <- md$config_fingerprint %||% NULL
        est_desc <- cs_get_estimator(est_id_i)
        caller_config <- resolve_config(est_id_i)
        task_config <- apply_runner_defaults(caller_config, seed_i)
        stored_schema <- suppressWarnings(as.integer(md$config_fingerprint_schema %||% NA_integer_))
        expected_fp <- if (is.na(stored_schema) || stored_schema == 1L) {
          if (is.finite(max_runtime)) {
            rlang::abort(
              message = "Cannot resume legacy (v0.1.7) pins with non-infinite `max_runtime`; legacy fingerprints do not encode runtime guards.",
              class   = "causalstress_fingerprint_error"
            )
          }
          cs_build_config_fingerprint_legacy(
            dgp_id            = dgp_id_i,
            estimator_id      = est_id_i,
            n                 = n_i,
            seed              = seed_i,
            bootstrap         = bootstrap,
            B                 = B,
            oracle            = isTRUE(est_desc$oracle),
            estimator_version = est_desc$version,
            config            = task_config,
            tau               = tau
          )
        } else if (stored_schema == 2L) {
          stored_dgp_version <- as.character(md$dgp_version %||% NA_character_)
          if (is.na(stored_dgp_version) || !identical(stored_dgp_version, as.character(dgp_version_i))) {
            rlang::abort(
              message = "Cannot resume schema-2 pin because its DGP version metadata does not match the resolved DGP version.",
              class   = "causalstress_fingerprint_error"
            )
          }
          cs_build_config_fingerprint_schema2(
            dgp_id            = dgp_id_i,
            estimator_id      = est_id_i,
            n                 = n_i,
            seed              = seed_i,
            bootstrap         = bootstrap,
            B                 = B,
            oracle            = isTRUE(est_desc$oracle),
            estimator_version = est_desc$version,
            config            = task_config,
            tau               = tau,
            max_runtime       = max_runtime
          )
        } else if (stored_schema == 3L) {
          cs_build_config_fingerprint(
            dgp_id            = dgp_id_i,
            estimator_id      = est_id_i,
            n                 = n_i,
            seed              = seed_i,
            bootstrap         = bootstrap,
            B                 = B,
            oracle            = isTRUE(est_desc$oracle),
            estimator_version = est_desc$version,
            config            = caller_config,
            tau               = tau,
            max_runtime       = max_runtime,
            dgp_version       = dgp_version_i
          )
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
            dgp_id_i, " x ", est_id_i, " seed ", seed_i, ". ",
            "(Stored: ", old_txt, ", Current: ", expected_fp, "). ",
            "To overwrite this run with new settings, set skip_existing = FALSE or use a fresh board.",
            call. = FALSE
          )
        }
        if (isTRUE(bootstrap) && B > 0 && !cs_has_boot_ci_meta(md)) {
          stop(
            "Existing run found for this (dgp_id, estimator_id, n, seed) ",
            "but it was computed without bootstrap CIs, while you requested ",
            "bootstrap = TRUE, B = ", B, ". Use a fresh board or set ",
            "skip_existing = FALSE to recompute.",
            call. = FALSE
          )
        }
        skip_vec[i] <- TRUE
      }
    }

    n_skip <- sum(skip_vec)
    if (n_skip > 0L) {
      tasks <- tasks[!skip_vec, , drop = FALSE]
    }
    message(glue::glue("Skipping {n_skip} existing tasks... Running {nrow(tasks)} tasks."))
  }

  if (nrow(tasks) == 0L) {
    return(tibble::tibble())
  }

  # Shuffle tasks (deterministically when campaign_seed is provided)
  if (!is.null(campaign_seed)) {
    idx <- cs_with_mandated_rng(as.integer(campaign_seed), sample.int(nrow(tasks)))
    tasks <- tasks[idx, , drop = FALSE]
  }

  run_task <- function(dgp_id, estimator_id, seed, n, p = NULL) {
    task_config <- resolve_config(estimator_id)
    do.call(
      cs_run_one_seed_internal,
      c(
        list(
          dgp_id        = dgp_id,
          estimator_id  = estimator_id,
          n             = n,
          seed          = seed,
          version       = version,
          status        = status,
          tau           = tau,
          bootstrap     = bootstrap,
          B             = B,
          config        = task_config,
          board         = if (isTRUE(parallel) || !is.null(staging_dir)) NULL else board,
          skip_existing = FALSE,
          force         = force,
          quiet         = quiet,
          max_runtime   = max_runtime,
          parallel      = parallel,
          experimental_parallel = experimental_parallel,
          parallel_backend = parallel_backend,
          parallel_warning_emitted = parallel_warning_emitted,
          staging_dir   = staging_dir,
          p             = p
        ),
        dots
      )
    )
  }

  run_campaign <- function() {
    p <- if (isTRUE(show_progress)) progressr::progressor(steps = nrow(tasks) + 1L) else NULL
    if (isTRUE(parallel)) {
      rows <- furrr::future_pmap(
        tasks,
        run_task,
        p = p,
        .options  = furrr::furrr_options(seed = TRUE, scheduling = TRUE, packages = "CausalStress"),
        .progress = FALSE
      )
    } else {
      rows <- purrr::pmap(tasks, run_task, p = p)
    }

    out <- dplyr::bind_rows(rows)

    if (!is.null(staging_dir) && !is.null(board)) {
      gathered <- cs_gather_results(board, staging_dir)
      if (!is.null(p)) p(message = glue::glue("Gathered {gathered} staged results"))
    } else if (!is.null(p)) {
      p(message = "Gathering results...")
    }

    dplyr::arrange(out, dgp_id, estimator_id, seed)
  }

  if (isTRUE(show_progress)) {
    current_handlers <- progressr::handlers(default = NA)
    if (length(current_handlers) == 0L) {
      if (requireNamespace("cli", quietly = TRUE)) {
        progressr::handlers(
          progressr::handler_cli(
            intrusiveness = getOption("progressr.intrusiveness.gui", 1)
          )
        )
      } else {
        progressr::handlers(progressr::handler_txtprogressbar(style = 3))
      }
    }
    progressr::with_progress(run_campaign())
  } else {
    run_campaign()
  }
}
