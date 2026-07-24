#' Extract the science payload from a run result
#'
#' Returns a canonicalized object suitable for bitwise identity checks. This
#' excludes non-deterministic and non-scientific fields (timestamps, runtimes,
#' logs, warnings).
#'
#' @param x A result list as returned by [cs_run_single()] or read from pins.
#' @return A list with elements `att`, `qst`, and `meta`.
#' @export
cs_science_payload <- function(x) {
  if (!is.list(x)) {
    rlang::abort("`x` must be a run result list.", class = "causalstress_contract_error")
  }
  att <- x$att %||% list()
  meta <- x$meta %||% list()
  scores <- x$scores %||% NULL

  qst <- x$qst %||% NULL
  if (!is.null(qst)) {
    if (!tibble::is_tibble(qst)) {
      rlang::abort("`x$qst` must be a tibble or NULL.", class = "causalstress_contract_error")
    }
    if (!"tau_id" %in% names(qst) && "tau" %in% names(qst)) {
      qst <- dplyr::mutate(qst, tau_id = cs_tau_id(.data$tau))
    }
    if ("tau_id" %in% names(qst)) {
      qst <- dplyr::arrange(qst, .data$tau_id)
    }
  }
  if (!is.null(scores)) {
    scores <- tibble::as_tibble(scores)
    order_cols <- intersect(
      c("estimand_target_id", "metric_id", "tau_id", "tau_index"),
      names(scores)
    )
    if (length(order_cols) > 0L) {
      scores <- dplyr::arrange(scores, dplyr::across(dplyr::all_of(order_cols)))
    }
  }

  list(
    att = list(
      estimate     = att$estimate %||% NA_real_,
      true         = att$true %||% NA_real_,
      error        = att$error %||% NA_real_,
      abs_error    = att$abs_error %||% NA_real_,
      ci_lo        = att$ci_lo %||% NA_real_,
      ci_hi        = att$ci_hi %||% NA_real_,
      boot_covered = att$boot_covered %||% att$att_covered %||% NA,
      ci_width     = att$ci_width %||% NA_real_
    ),
    qst = qst,
    scores = scores,
    meta = list(
      dgp_id            = meta$dgp_id %||% NA_character_,
      dgp_version       = meta$dgp_version %||% NA_character_,
      estimator_id      = meta$estimator_id %||% NA_character_,
      estimator_version = meta$estimator_version %||% NA_character_,
      n                 = meta$n %||% NA_integer_,
      seed              = meta$seed %||% NA_integer_,
      oracle            = meta$oracle %||% NA,
      oracle_columns_granted = meta$oracle_columns_granted %||% character(0),
      supports_qst       = meta$supports_qst %||% NA,
      config_fingerprint = meta$config_fingerprint %||% NA_character_,
      config_fingerprint_schema = meta$config_fingerprint_schema %||% NA_integer_,
      fit_fingerprint = meta$fit_fingerprint %||% NA_character_,
      truth_version = meta$truth_version %||% NA_character_,
      score_fingerprints = meta$score_fingerprints %||% character(0),
      score_row_fingerprints = meta$score_row_fingerprints %||% character(0)
    )
  )
}

#' Extract provenance from a run result
#'
#' Returns non-scientific metadata (timestamps, runtimes, logs, warnings, thread
#' caps, parallel flags, timeouts).
#'
#' @param x A result list as returned by [cs_run_single()] or read from pins.
#' @return A list of provenance fields.
#' @export
cs_provenance <- function(x) {
  if (!is.list(x)) {
    rlang::abort("`x` must be a run result list.", class = "causalstress_contract_error")
  }
  prov <- x$provenance %||% list()
  meta <- x$meta %||% list()
  c(
    prov,
    list(
      success = meta$success %||% NA,
      error = meta$error %||% NA_character_,
      log = meta$log %||% NA_character_,
      warnings = meta$warnings %||% character(0)
    )
  )
}

#' Flatten minimal identifiers for analysis
#'
#' @param x A result list as returned by [cs_run_single()] or read from pins.
#' @return A one-row tibble of identifiers and reproducibility controls.
#' @export
cs_meta_flatten <- function(x) {
  if (!is.list(x)) {
    rlang::abort("`x` must be a run result list.", class = "causalstress_contract_error")
  }
  meta <- x$meta %||% list()
  prov <- x$provenance %||% list()

  tibble::tibble(
    dgp_id = meta$dgp_id %||% NA_character_,
    dgp_version = meta$dgp_version %||% NA_character_,
    dgp_status = meta$dgp_status %||% NA_character_,
    dgp_design_spec = meta$dgp_design_spec %||% NA_character_,
    estimator_id = meta$estimator_id %||% NA_character_,
    estimator_version = meta$estimator_version %||% NA_character_,
    n = meta$n %||% NA_integer_,
    seed = meta$seed %||% NA_integer_,
    tau_id = meta$tau_id %||% NA_character_,
    oracle = meta$oracle %||% NA,
    oracle_columns_granted = list(meta$oracle_columns_granted %||% character(0)),
    supports_qst = meta$supports_qst %||% NA,
    config_fingerprint = meta$config_fingerprint %||% NA_character_,
    config_fingerprint_schema = meta$config_fingerprint_schema %||% NA_integer_,
    fit_fingerprint = meta$fit_fingerprint %||% NA_character_,
    truth_version = meta$truth_version %||% NA_character_,
    score_fingerprints = list(meta$score_fingerprints %||% character(0)),
    score_row_fingerprints = list(
      meta$score_row_fingerprints %||% character(0)
    ),
    task_fingerprint = meta$task_fingerprint %||% NA_character_,
    max_runtime = prov$max_runtime %||% NA_real_,
    experimental_parallel = prov$experimental_parallel %||% FALSE,
    parallel_warning_emitted = prov$parallel_warning_emitted %||% FALSE,
    parallel_backend = prov$parallel_backend %||% NA_character_,
    thread_caps_applied = prov$thread_caps_applied %||% FALSE,
    requested_num_threads = prov$requested_num_threads %||% NA_integer_,
    effective_num_threads = prov$effective_num_threads %||% NA_integer_,
    staging_dir_used = prov$staging_dir_used %||% FALSE,
    dgp_noise_family = prov$dgp_noise_family %||% NA_character_,
    dgp_effect_type = prov$dgp_effect_type %||% NA_character_
  )
}

#' Read a batch pin from a board
#'
#' Reads a batch artifact pinned by `cs_consolidate()` and returns the list of
#' run result objects stored under `results`.
#'
#' @param board A pins board containing batch pins.
#' @param batch_id Integer batch id or a full pin name (`batch_{id}`).
#' @return A list of run result objects.
#' @export
cs_read_batch <- function(board, batch_id) {
  if (is.null(board)) {
    rlang::abort("`board` must be provided.")
  }
  if (is.null(batch_id) || length(batch_id) != 1L) {
    rlang::abort("`batch_id` must be a single value.")
  }
  pin_name <- if (is.character(batch_id) && grepl("^batch_", batch_id)) {
    batch_id
  } else {
    paste0("batch_", batch_id)
  }

  obj <- pins::pin_read(board, pin_name)
  if (!is.list(obj) || is.null(obj$results)) {
    rlang::abort("Batch pin does not contain results.")
  }
  obj$results
}

#' Tidy a batch of run results
#'
#' Applies `cs_tidy()` to each result object in a batch and returns a single
#' combined tibble.
#'
#' @param batch_results A list of run result objects (from `cs_read_batch()`).
#' @return A tibble of tidy results.
#' @export
cs_tidy_batch <- function(batch_results) {
  if (!is.list(batch_results)) {
    rlang::abort("`batch_results` must be a list of run results.")
  }
  if (length(batch_results) == 0L) {
    return(tibble::tibble())
  }
  dplyr::bind_rows(lapply(batch_results, cs_tidy))
}
