#' Run a full campaign across DGPs, estimators, and seeds
#'
#' This runner parallelizes over the full Cartesian product of
#' (dgp_id, estimator_id, seed) with dynamic load balancing, making it
#' the recommended entry point for large heterogeneous campaigns.
#'
#' @param dgp_ids Character vector of DGP IDs.
#' @param estimator_ids Character vector of estimator IDs.
#' @param dgp_id Deprecated alias for `dgp_ids` (scalar character).
#' @param estimator_id Deprecated alias for `estimator_ids` (scalar character).
#' @param seeds Integer vector of seeds.
#' @param n Integer sample size per run.
#' @param defaults Optional default estimator config list forwarded to
#'   `cs_run_single()` (e.g., `list(n_boot = 200, num_threads = 1L)`).
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
#' @param show_progress Logical; show progressr-based progress.
#' @param ... Additional arguments forwarded to cs_run_single() (tau, etc.).
#'
#' @return Tibble with one row per run.
#' @export
cs_run_campaign <- function(
  dgp_ids = NULL,
  estimator_ids = NULL,
  dgp_id = NULL,
  estimator_id = NULL,
  seeds,
  n,
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
  show_progress = TRUE,
  force = FALSE,
  quiet = TRUE,
  max_runtime = Inf,
  ...
) {
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

  tasks <- tidyr::expand_grid(
    dgp_id       = dgp_ids,
    estimator_id = estimator_ids,
    seed         = seeds
  ) %>%
    dplyr::mutate(n = n)

  if (!is.null(campaign_seed) && (!is.numeric(campaign_seed) || length(campaign_seed) != 1L || !is.finite(campaign_seed))) {
    rlang::abort("`campaign_seed` must be a finite numeric scalar or NULL.", class = "causalstress_contract_error")
  }

  cs_require_staging_for_parallel_persistence(parallel = parallel, board = board, staging_dir = staging_dir)

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

      if (cs_pin_exists(board, dgp_id_i, est_id_i, n_i, seed_i)) {
        name <- glue::glue(
          "results__dgp={dgp_id_i}__est={est_id_i}__n={n_i}__seed={seed_i}"
        )
        meta_obj <- pins::pin_meta(board, name)
        md <- cs_pin_meta_user_or_metadata(meta_obj)
        stored_fp <- md$config_fingerprint %||% NULL
        est_desc <- cs_get_estimator(est_id_i)
        task_config <- apply_runner_defaults(resolve_config(est_id_i), seed_i)
        expected_fp <- cs_build_config_fingerprint(
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
    idx <- withr::with_seed(as.integer(campaign_seed), sample.int(nrow(tasks)))
    tasks <- tasks[idx, , drop = FALSE]
  }

  run_task <- function(dgp_id, estimator_id, seed, n, p = NULL) {
    task_config <- apply_runner_defaults(resolve_config(estimator_id), seed)
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
    progressr::with_progress(run_campaign())
  } else {
    run_campaign()
  }
}
