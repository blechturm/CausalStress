#' Run a full campaign across DGPs, estimators, and seeds
#'
#' This runner parallelizes over the full Cartesian product of
#' (dgp_id, estimator_id, seed) with dynamic load balancing, making it
#' the recommended entry point for large heterogeneous campaigns.
#'
#' @param dgp_ids Character vector of DGP IDs.
#' @param estimator_ids Character vector of estimator IDs.
#' @param seeds Integer vector of seeds.
#' @param n Integer sample size per run.
#' @param config Optional estimator config list forwarded to cs_run_single().
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
  dgp_ids,
  estimator_ids,
  seeds,
  n,
  config = list(),
  version = NULL,
  status = "stable",
  tau = cs_tau_oracle,
  bootstrap = FALSE,
  B = 0L,
  skip_existing = FALSE,
  board = NULL,
  staging_dir = NULL,
  parallel = TRUE,
  show_progress = TRUE,
  force = FALSE,
  quiet = TRUE,
  max_runtime = Inf,
  ...
) {
  # Backward compatibility with legacy argument names
  if (missing(dgp_ids) && !missing(dgp_id)) {
    dgp_ids <- dgp_id
  }
  if (missing(estimator_ids) && !missing(estimator_id)) {
    estimator_ids <- estimator_id
  }

  tasks <- tidyr::expand_grid(
    dgp_id       = dgp_ids,
    estimator_id = estimator_ids,
    seed         = seeds
  ) %>%
    dplyr::mutate(n = n)

  if (!is.null(staging_dir) && !is.null(board)) {
    dir.create(staging_dir, recursive = TRUE, showWarnings = FALSE)
    cs_gather_results(board, staging_dir)
  }

  # Skip existing pins if requested (with fingerprint/CI checks)
  if (isTRUE(skip_existing) && !is.null(board)) {
    skip_vec <- logical(nrow(tasks))

    has_ci <- function(run_row) {
      n_ok <- tryCatch(run_row$n_boot_ok, error = function(...) NA)
      lo <- tryCatch(run_row$att_ci_lo, error = function(...) NA)
      hi <- tryCatch(run_row$att_ci_hi, error = function(...) NA)
      if (is.null(lo) || is.null(hi)) return(FALSE)
      if (is.na(n_ok) || n_ok == 0L) return(FALSE)
      if (all(is.na(lo)) || all(is.na(hi))) return(FALSE)
      TRUE
    }

    for (i in seq_len(nrow(tasks))) {
      dgp_id_i  <- tasks$dgp_id[i]
      est_id_i  <- tasks$estimator_id[i]
      n_i       <- tasks$n[i]
      seed_i    <- tasks$seed[i]

      if (cs_pin_exists(board, dgp_id_i, est_id_i, n_i, seed_i)) {
        name <- glue::glue(
          "results__dgp={dgp_id_i}__est={est_id_i}__n={n_i}__seed={seed_i}"
        )
        cached <- pins::pin_read(board, name)
        stored_fp <- tryCatch(cached$meta$config_fingerprint, error = function(...) NULL)
        est_desc <- cs_get_estimator(est_id_i)
        expected_fp <- cs_build_config_fingerprint(
          dgp_id            = dgp_id_i,
          estimator_id      = est_id_i,
          n                 = n_i,
          seed              = seed_i,
          bootstrap         = bootstrap,
          B                 = B,
          oracle            = isTRUE(est_desc$oracle),
          estimator_version = est_desc$version,
          config            = config,
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
        tidy_row <- cs_result_to_row(cached)
        if (isTRUE(bootstrap) && B > 0 && !has_ci(tidy_row)) {
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

  # Shuffle tasks to mix DGPs/seeds for better load distribution
  tasks <- tasks[sample(nrow(tasks)), , drop = FALSE]

  run_task <- function(dgp_id, estimator_id, seed, n, p = NULL) {
    cs_run_one_seed_internal(
      dgp_id       = dgp_id,
      estimator_id = estimator_id,
      n            = n,
      seed         = seed,
      version      = version,
      status       = status,
      tau          = tau,
      bootstrap    = bootstrap,
      B            = B,
      config       = config,
      board        = board,
      skip_existing = FALSE,
      force         = force,
      quiet         = quiet,
      max_runtime   = max_runtime,
      parallel      = parallel,
      staging_dir   = staging_dir,
      p             = p,
      ...
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

    out
  }

  if (isTRUE(show_progress)) {
    progressr::with_progress(run_campaign())
  } else {
    run_campaign()
  }
}
