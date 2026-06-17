#' @noRd
cs_chk_scalar_numeric <- function(x, arg) {
  if (!is.numeric(x) || length(x) != 1L || !is.finite(x)) {
    rlang::abort(
      message = glue::glue("`{arg}` must be a finite numeric scalar."),
      class   = "causalstress_contract_error"
    )
  }
}

#' @noRd
cs_estimand_target_ids <- function() {
  c("att", "ate", "qst", "cate")
}

#' @noRd
cs_estimand_targets <- function() {
  list(
    att = list(
      estimand_target_id = "att",
      truth_tier = "structural",
      target_level = "population-scalar",
      target_population = "treated",
      evaluation_policy = "observed-run-sample",
      metric_ids = "point_error",
      scoring_population_id = "treated"
    ),
    ate = list(
      estimand_target_id = "ate",
      truth_tier = "structural",
      target_level = "population-scalar",
      target_population = "all",
      evaluation_policy = "observed-run-sample",
      metric_ids = "point_error",
      scoring_population_id = "full_generated_run_sample"
    ),
    qst = list(
      estimand_target_id = "qst",
      truth_tier = "distributional",
      target_level = "distributional-curve",
      target_population = "treated",
      evaluation_policy = "runner-tau-grid",
      metric_ids = "point_error",
      scoring_population_id = "treated"
    ),
    cate = list(
      estimand_target_id = "cate",
      truth_tier = "structural",
      target_level = "unit-level",
      target_population = "held-out-eval",
      evaluation_policy = "held-out-eval",
      metric_ids = "point_error",
      scoring_population_id = "held_out_eval"
    )
  )
}

#' @noRd
cs_compact_estimand_target_id <- function(target) {
  if (is.list(target)) {
    target <- target$estimand_target_id %||% target$target_id %||% NULL
  }
  if (!is.character(target) || length(target) != 1L || is.na(target) || !nzchar(target)) {
    rlang::abort(
      "`target` must identify one estimand target.",
      class = "causalstress_estimand_target_error"
    )
  }
  target <- tolower(target)
  if (!target %in% cs_estimand_target_ids()) {
    rlang::abort(
      glue::glue("Unknown estimand target id: {target}."),
      class = "causalstress_estimand_target_error"
    )
  }
  target
}

#' @noRd
cs_estimand_target <- function(target) {
  target_id <- cs_compact_estimand_target_id(target)
  cs_estimand_targets()[[target_id]]
}

#' @noRd
cs_normalize_estimand_target_ids <- function(targets) {
  if (is.null(targets)) {
    return(character(0L))
  }
  if (is.list(targets) && !is.character(targets)) {
    ids <- vapply(targets, cs_compact_estimand_target_id, character(1))
  } else {
    ids <- vapply(as.character(targets), cs_compact_estimand_target_id, character(1))
  }
  unique(unname(ids))
}

#' @noRd
cs_non_comparable_reasons <- function() {
  c(
    "estimator_not_produced",
    "truth_unavailable",
    "metric_invalid_for_regime",
    "ci_unavailable",
    "gate_unimplemented",
    "not_requested",
    "target_not_implemented"
  )
}

#' @noRd
cs_check_non_comparable_reason <- function(reason) {
  if (!is.character(reason) || length(reason) != 1L || is.na(reason) || !nzchar(reason)) {
    rlang::abort(
      "`reason` must be a non-empty character scalar.",
      class = "causalstress_non_comparable_reason_error"
    )
  }
  if (!reason %in% cs_non_comparable_reasons()) {
    rlang::abort(
      glue::glue("Unknown non-comparable reason: {reason}."),
      class = "causalstress_non_comparable_reason_error"
    )
  }
  invisible(reason)
}

#' @noRd
cs_requested_estimand_targets <- function(config = list(), estimator_desc = list()) {
  config <- config %||% list()
  requested <- config$estimand_targets %||%
    config$estimand_target_ids %||%
    config$targets %||%
    NULL

  if (is.null(requested)) {
    requested <- "att"
    if (isTRUE(estimator_desc$supports_qst)) {
      requested <- c(requested, "qst")
    }
  }

  cs_normalize_estimand_target_ids(requested)
}

#' @noRd
cs_abort_target_not_implemented <- function(target_id) {
  rlang::abort(
    glue::glue(
      "{toupper(target_id)} is registered but not implemented in Wave 1 ",
      "(`target_not_implemented`); {toupper(target_id)}-only tasks cannot be executed."
    ),
    class = c(
      "causalstress_target_not_implemented_error",
      "target_not_implemented",
      "causalstress_runner_error"
    )
  )
}

#' @noRd
cs_assert_wave1_targets_executable <- function(config = list(), estimator_desc = list()) {
  requested <- cs_requested_estimand_targets(
    config = config,
    estimator_desc = estimator_desc
  )
  if (identical(requested, "cate")) {
    cs_abort_target_not_implemented("cate")
  }
  requested
}

#' @noRd
cs_normalize_scalar_estimand_output <- function(x, target_id) {
  if (is.null(x)) {
    return(NULL)
  }
  if (is.data.frame(x)) {
    if (!"estimate" %in% names(x)) {
      rlang::abort(
        glue::glue("`outputs${target_id}` must contain an `estimate` field."),
        class = c("causalstress_contract_error", "causalstress_estimator_error")
      )
    }
    estimate <- x[["estimate"]][[1L]]
    out <- as.list(x[1L, , drop = FALSE])
  } else if (is.list(x)) {
    if (is.null(x$estimate)) {
      rlang::abort(
        glue::glue("`outputs${target_id}` must contain an `estimate` field."),
        class = c("causalstress_contract_error", "causalstress_estimator_error")
      )
    }
    estimate <- x$estimate
    out <- x
  } else {
    rlang::abort(
      glue::glue("`outputs${target_id}` must be a list or data frame."),
      class = c("causalstress_contract_error", "causalstress_estimator_error")
    )
  }
  if (!is.numeric(estimate) || length(estimate) != 1L) {
    rlang::abort(
      glue::glue("`outputs${target_id}$estimate` must be a numeric scalar."),
      class = c("causalstress_contract_error", "causalstress_estimator_error")
    )
  }
  out$estimate <- estimate
  out$estimand_target_id <- target_id
  out
}

#' @noRd
cs_normalize_qst_estimand_output <- function(x, tau = NULL) {
  if (is.null(x)) {
    return(NULL)
  }
  if (is.list(x) && !is.data.frame(x) && !is.null(x$values)) {
    x <- x$values
  }
  if (!tibble::is_tibble(x)) {
    x <- tibble::as_tibble(x)
  }
  if (!"tau" %in% names(x)) {
    rlang::abort(
      "`outputs$qst` must contain a `tau` column.",
      class = c("causalstress_contract_error", "causalstress_estimator_error")
    )
  }
  if (!"estimate" %in% names(x) && "value" %in% names(x)) {
    x <- dplyr::rename(x, estimate = value)
  }
  if (!"estimate" %in% names(x)) {
    rlang::abort(
      "`outputs$qst` must contain an `estimate` or legacy `value` column.",
      class = c("causalstress_contract_error", "causalstress_estimator_error")
    )
  }
  if (!is.numeric(x$tau) || !is.numeric(x$estimate)) {
    rlang::abort(
      "`outputs$qst$tau` and `outputs$qst$estimate` must be numeric.",
      class = c("causalstress_contract_error", "causalstress_estimator_error")
    )
  }
  if (anyNA(x$tau) || anyNA(x$estimate)) {
    rlang::abort(
      "`outputs$qst$tau` and `outputs$qst$estimate` must not contain NA.",
      class = c("causalstress_contract_error", "causalstress_estimator_error")
    )
  }
  if (!is.null(tau) && !identical(cs_tau_id(x$tau), cs_tau_id(tau))) {
    rlang::abort(
      "`outputs$qst$tau` must match the requested tau grid exactly.",
      class = c("causalstress_contract_error", "causalstress_estimator_error")
    )
  }
  if (!"tau_id" %in% names(x)) {
    x$tau_id <- cs_tau_id(x$tau)
  }
  x
}

#' @noRd
cs_normalize_estimator_outputs <- function(res, tau = NULL) {
  if (!is.list(res)) {
    rlang::abort(
      "`res` must be a list returned by an estimator.",
      class = "causalstress_estimator_error"
    )
  }

  raw_outputs <- res$outputs %||% NULL
  if (is.null(raw_outputs)) {
    raw_outputs <- list(
      att = res$att %||% NULL,
      qst = res$qst %||% NULL
    )
  }
  if (!is.list(raw_outputs)) {
    rlang::abort(
      "`outputs` must be a named list.",
      class = "causalstress_estimator_error"
    )
  }

  outputs <- list()
  raw_names <- names(raw_outputs) %||% character(length(raw_outputs))
  for (i in seq_along(raw_outputs)) {
    raw <- raw_outputs[[i]]
    if (is.null(raw)) {
      next
    }
    target_id <- raw_names[[i]]
    if (is.na(target_id) || !nzchar(target_id)) {
      target_id <- raw$estimand_target_id %||% raw$target_id %||% NA_character_
    }
    target_id <- cs_compact_estimand_target_id(target_id)
    outputs[[target_id]] <- switch(
      target_id,
      att = cs_normalize_scalar_estimand_output(raw, "att"),
      ate = cs_normalize_scalar_estimand_output(raw, "ate"),
      qst = cs_normalize_qst_estimand_output(raw, tau = tau),
      cate = raw
    )
  }

  outputs
}

#' @noRd
cs_truth_available_targets <- function(dgp) {
  out <- character(0L)
  if (!is.null(dgp$true_att) && is.numeric(dgp$true_att) && length(dgp$true_att) == 1L && is.finite(dgp$true_att)) {
    out <- c(out, "att")
  }
  if (!is.null(dgp$true_qst) && tibble::is_tibble(dgp$true_qst)) {
    out <- c(out, "qst")
  }
  meta <- dgp$meta %||% list()
  if (identical(meta$type, "synthetic") &&
      is.numeric(meta$structural_te) &&
      length(meta$structural_te) == nrow(dgp$df %||% data.frame()) &&
      !anyNA(meta$structural_te)) {
    out <- c(out, "ate")
  }
  unique(out)
}

#' @noRd
cs_make_score_row <- function(target_id, estimate = NA_real_, truth = NA_real_,
                              error = NA_real_, abs_error = NA_real_,
                              ci_lo = NA_real_, ci_hi = NA_real_,
                              tau = NA_real_, tau_id = NA_character_,
                              tau_index = NA_integer_,
                              status = "scored", reason = NA_character_) {
  desc <- cs_estimand_target(target_id)
  if (!is.na(reason)) {
    cs_check_non_comparable_reason(reason)
  }
  tibble::tibble(
    estimand_target_id = target_id,
    truth_tier = desc$truth_tier,
    target_level = desc$target_level,
    target_population = desc$target_population,
    evaluation_policy = desc$evaluation_policy,
    scoring_population_id = desc$scoring_population_id,
    metric_id = desc$metric_ids[[1L]] %||% "point_error",
    tau = tau,
    tau_id = tau_id,
    tau_index = tau_index,
    estimate = estimate,
    truth = truth,
    error = error,
    abs_error = abs_error,
    ci_lo = ci_lo,
    ci_hi = ci_hi,
    score_status = status,
    non_comparable_reason = reason,
    fit_fingerprint = NA_character_,
    score_fingerprint = NA_character_,
    truth_version = NA_character_,
    seed_eval = NA_integer_,
    n_eval = NA_integer_,
    eval_derivation = NA_character_,
    unit_id_digest = NA_character_,
    prediction_digest = NA_character_,
    transductive = NA
  )
}

#' @noRd
cs_build_score_surface <- function(requested_targets, outputs, dgp, att = NULL,
                                   qst = NULL, failure_status = NULL) {
  requested_targets <- cs_normalize_estimand_target_ids(requested_targets)
  if (!is.null(failure_status)) {
    if (!failure_status %in% c("estimator_error", "runner_error")) {
      rlang::abort(
        "`failure_status` must be `estimator_error`, `runner_error`, or NULL.",
        class = "causalstress_score_status_error"
      )
    }
    rows <- lapply(
      requested_targets,
      function(target_id) {
        cs_make_score_row(
          target_id,
          status = failure_status,
          reason = NA_character_
        )
      }
    )
    if (length(rows) == 0L) {
      return(tibble::tibble())
    }
    return(dplyr::bind_rows(rows))
  }

  produced_targets <- names(outputs)
  truth_targets <- cs_truth_available_targets(dgp)
  rows <- list()

  for (target_id in requested_targets) {
    if (identical(target_id, "cate")) {
      rows[[length(rows) + 1L]] <- cs_make_score_row(
        target_id,
        status = "non_comparable",
        reason = "target_not_implemented"
      )
      next
    }

    if (!target_id %in% produced_targets) {
      rows[[length(rows) + 1L]] <- cs_make_score_row(
        target_id,
        status = "non_comparable",
        reason = "estimator_not_produced"
      )
      next
    }

    if (!target_id %in% truth_targets) {
      rows[[length(rows) + 1L]] <- cs_make_score_row(
        target_id,
        status = "non_comparable",
        reason = "truth_unavailable"
      )
      next
    }

    if (identical(target_id, "att")) {
      att <- att %||% list()
      rows[[length(rows) + 1L]] <- cs_make_score_row(
        "att",
        estimate = att$estimate %||% NA_real_,
        truth = att$true %||% NA_real_,
        error = att$error %||% NA_real_,
        abs_error = att$abs_error %||% NA_real_,
        ci_lo = att$ci_lo %||% NA_real_,
        ci_hi = att$ci_hi %||% NA_real_
      )
    } else if (identical(target_id, "ate")) {
      ate_out <- outputs$ate
      ate_truth <- cs_true_ate(dgp$meta$structural_te)
      ate_est <- ate_out$estimate
      rows[[length(rows) + 1L]] <- cs_make_score_row(
        "ate",
        estimate = ate_est,
        truth = ate_truth,
        error = ate_est - ate_truth,
        abs_error = abs(ate_est - ate_truth),
        ci_lo = ate_out$ci_lo %||% NA_real_,
        ci_hi = ate_out$ci_hi %||% NA_real_
      )
    } else if (identical(target_id, "qst")) {
      qst <- qst %||% outputs$qst
      if (is.null(qst) || nrow(qst) == 0L) {
        rows[[length(rows) + 1L]] <- cs_make_score_row(
          "qst",
          status = "non_comparable",
          reason = "truth_unavailable"
        )
      } else {
        for (i in seq_len(nrow(qst))) {
          truth_i <- qst$true[[i]] %||% NA_real_
          status_i <- if (is.na(truth_i)) "non_comparable" else "scored"
          reason_i <- if (identical(status_i, "non_comparable")) "truth_unavailable" else NA_character_
          rows[[length(rows) + 1L]] <- cs_make_score_row(
            "qst",
            estimate = qst$estimate[[i]],
            truth = truth_i,
            error = qst$error[[i]] %||% NA_real_,
            abs_error = qst$abs_error[[i]] %||% NA_real_,
            ci_lo = qst$ci_lo[[i]] %||% NA_real_,
            ci_hi = qst$ci_hi[[i]] %||% NA_real_,
          tau = qst$tau[[i]],
          tau_id = qst$tau_id[[i]] %||% cs_tau_id(qst$tau[[i]]),
          tau_index = i,
          status = status_i,
          reason = reason_i
          )
        }
      }
    }
  }

  if (length(rows) == 0L) {
    return(tibble::tibble())
  }
  dplyr::bind_rows(rows)
}

#' @noRd
cs_attach_score_identity <- function(scores, fit_fingerprint, truth_version) {
  if (!is.data.frame(scores) || nrow(scores) == 0L) {
    return(scores)
  }
  scores$fit_fingerprint <- fit_fingerprint
  scores$truth_version <- truth_version
  scores$schema_version <- 4L
  scores$score_fingerprint <- vapply(
    seq_len(nrow(scores)),
    function(i) {
      cs_build_score_fingerprint(
        fit_fingerprint = fit_fingerprint,
        estimand_target_id = scores$estimand_target_id[[i]],
        metric_id = scores$metric_id[[i]],
        truth_version = truth_version,
        scoring_population_id = scores$scoring_population_id[[i]],
        tau_id = scores$tau_id[[i]] %||% NA_character_
      )
    },
    character(1)
  )
  scores
}

#' @noRd
cs_check_dgp_synthetic <- function(dgp) {
  # top-level structure
  if (!is.list(dgp)) {
    rlang::abort(
      message = "`dgp` must be a list returned by a DGP generator.",
      class   = "causalstress_dgp_error"
    )
  }

  required <- c("df", "true_att", "true_qst", "meta")
  missing <- setdiff(required, names(dgp))
  if (length(missing) > 0) {
    rlang::abort(
      message = glue::glue(
        "DGP output is missing required components: {toString(missing)}"
      ),
      class   = "causalstress_dgp_error"
    )
  }

  # df checks
  df <- dgp$df
  if (!is.data.frame(df)) {
    rlang::abort(
      message = "`df` must be a data.frame (or tibble).",
      class   = "causalstress_dgp_error"
    )
  }

  n <- nrow(df)

  # required core columns per registry: y, w, y0, y1, p
  required_cols <- c("y", "w", "y0", "y1", "p")
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    rlang::abort(
      message = glue::glue(
        "DGP data frame is missing required columns: {toString(missing_cols)}"
      ),
      class   = "causalstress_dgp_error"
    )
  }

  if (anyNA(df[required_cols])) {
    rlang::abort(
      message = "Core DGP columns `y`, `w`, `y0`, `y1`, `p` must not contain NA.",
      class   = "causalstress_dgp_error"
    )
  }

  # structural_te column is optional but, if present, must be non-NA
  if ("structural_te" %in% names(df)) {
    if (anyNA(df[["structural_te"]])) {
      rlang::abort(
        message = "Column `structural_te` must not contain NA when present.",
        class   = "causalstress_dgp_error"
      )
    }
  }

  # treatment indicator well-formed
  w_vals <- df$w
  if (!(is.numeric(w_vals) || is.integer(w_vals)) || !all(w_vals %in% c(0, 1))) {
    rlang::abort(
      message = "`w` must be numeric/integer and contain only 0/1.",
      class   = "causalstress_dgp_error"
    )
  }

  # true_att
  cs_chk_scalar_numeric(dgp$true_att, "true_att")

  # true_qst
  tq <- dgp$true_qst
  if (!tibble::is_tibble(tq)) {
    rlang::abort(
      message = "`true_qst` must be a tibble with columns `tau` and `value`.",
      class   = "causalstress_dgp_error"
    )
  }
  if (!all(c("tau", "value") %in% names(tq))) {
    rlang::abort(
      message = "`true_qst` must have columns `tau` and `value`.",
      class   = "causalstress_dgp_error"
    )
  }
  if (!is.numeric(tq$tau) ||
      length(tq$tau) != length(tq$value)) {
    rlang::abort(
      message = "`true_qst$tau` must be numeric and match length of `true_qst$value`.",
      class   = "causalstress_dgp_error"
    )
  }
  if (!identical(cs_tau_id(tq$tau), cs_tau_id(cs_tau_oracle))) {
    rlang::abort(
      message = "`true_qst$tau` must equal the canonical cs_tau_oracle() grid.",
      class   = "causalstress_dgp_error"
    )
  }

  # meta
  meta <- dgp$meta
  if (!is.list(meta)) {
    rlang::abort(
      message = "`meta` must be a list.",
      class   = "causalstress_dgp_error"
    )
  }
  if (!is.character(meta$dgp_id) || length(meta$dgp_id) != 1L) {
    rlang::abort(
      message = "`meta$dgp_id` must be a character scalar.",
      class   = "causalstress_dgp_error"
    )
  }
  if (!identical(meta$type, "synthetic")) {
    rlang::abort(
      message = "`meta$type` must be \"synthetic\" for synthetic DGPs.",
      class   = "causalstress_dgp_error"
    )
  }

  # meta$structural_te is required for synthetic DGPs
  if (is.null(meta$structural_te)) {
    rlang::abort(
      message = "`meta$structural_te` must be provided for synthetic DGPs.",
      class   = "causalstress_dgp_error"
    )
  }
  if (!is.numeric(meta$structural_te)) {
    rlang::abort(
      message = "`meta$structural_te` must be a numeric vector.",
      class   = "causalstress_dgp_error"
    )
  }
  if (length(meta$structural_te) != n) {
    rlang::abort(
      message = "`meta$structural_te` must have length equal to nrow(df).",
      class   = "causalstress_dgp_error"
    )
  }
  if (anyNA(meta$structural_te)) {
    rlang::abort(
      message = "`meta$structural_te` must not contain NA.",
      class   = "causalstress_dgp_error"
    )
  }

  # if df also has structural_te, enforce consistency
  if ("structural_te" %in% names(df)) {
    if (!isTRUE(all.equal(meta$structural_te, df$structural_te))) {
      rlang::abort(
        message = "`meta$structural_te` must match `df$structural_te` when both are present.",
        class   = "causalstress_dgp_error"
      )
    }
  }

  invisible(TRUE)
}


#' @noRd
cs_check_estimator_output <- function(res, require_qst = FALSE, tau = NULL) {
  if (!is.list(res)) {
    rlang::abort(
      message = "`res` must be a list returned by an estimator.",
      class   = "causalstress_estimator_error"
    )
  }

  required <- if (!is.null(res$outputs)) c("outputs", "meta") else c("att", "qst", "meta")
  missing <- setdiff(required, names(res))
  if (length(missing) > 0) {
    rlang::abort(
      message = glue::glue(
        "Estimator output is missing required components: {toString(missing)}"
      ),
      class   = "causalstress_estimator_error"
    )
  }

  # meta
  if (!is.list(res$meta)) {
    rlang::abort(
      message = "`meta` must be a list.",
      class   = "causalstress_estimator_error"
    )
  }
  if (!is.character(res$meta$estimator_id) || length(res$meta$estimator_id) != 1L) {
    rlang::abort(
      message = "`meta$estimator_id` must be a character scalar.",
      class   = "causalstress_estimator_error"
    )
  }

  outputs <- cs_normalize_estimator_outputs(res, tau = tau)
  if (length(outputs) == 0L) {
    rlang::abort(
      message = "Estimator output did not produce any typed estimand outputs.",
      class   = "causalstress_estimator_error"
    )
  }

  for (target_id in intersect(names(outputs), c("att", "ate"))) {
    cs_chk_scalar_numeric(outputs[[target_id]]$estimate, glue::glue("{target_id}$estimate"))
  }

  if (require_qst && is.null(outputs$qst)) {
    rlang::abort(
      message = "Estimator must return `qst` when `require_qst = TRUE`.",
      class   = "causalstress_estimator_error"
    )
  }

  invisible(TRUE)
}
