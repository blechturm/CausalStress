#' Build a configuration fingerprint for a run
#'
#' Internal helper to generate a compact hash of key run settings for
#' resume safety.
#'
#' @keywords internal
#' @noRd
#' @importFrom digest digest
cs_ci_intent <- function(config = list(), bootstrap = FALSE, B = 0L) {
  if (is.list(config) && "ci_method" %in% names(config) && !is.null(config$ci_method)) {
    return(as.character(config$ci_method[[1L]]))
  }
  if (isTRUE(bootstrap) && as.integer(B) > 0L) {
    return("bootstrap")
  }
  "none"
}

cs_normalize_for_fingerprint <- function(x, path = "config") {
  if (is.null(x)) return(NULL)

  if (is.environment(x) || is.function(x) || is.symbol(x) || is.expression(x) || is.call(x)) {
    rlang::abort(
      message = glue::glue("Non-serializable value in `{path}` (type {typeof(x)})."),
      class = "causalstress_fingerprint_error"
    )
  }
  if (methods::is(x, "externalptr") || methods::is(x, "refClass")) {
    rlang::abort(
      message = glue::glue("Non-serializable value in `{path}` (class {class(x)[1]})."),
      class = "causalstress_fingerprint_error"
    )
  }
  if (methods::is(x, "S4")) {
    rlang::abort(
      message = glue::glue("S4 object not allowed in `{path}` for fingerprinting."),
      class = "causalstress_fingerprint_error"
    )
  }

  if (is.list(x)) {
    nm <- names(x)
    if (!is.null(nm)) {
      ord <- order(nm)
      x <- x[ord]
      nm <- nm[ord]
    }
    out <- vector("list", length(x))
    for (i in seq_along(x)) {
      key <- if (!is.null(nm) && nzchar(nm[[i]])) nm[[i]] else as.character(i)
      out[[i]] <- cs_normalize_for_fingerprint(x[[i]], path = paste0(path, "$", key))
    }
    if (!is.null(nm)) names(out) <- nm
    return(out)
  }

  cls <- class(x)
  if (!is.null(cls) && !identical(cls, typeof(x))) {
    if (!identical(cls, "numeric") && !identical(cls, "integer") && !identical(cls, "logical") && !identical(cls, "character")) {
      rlang::abort(
        message = glue::glue("Classed value not allowed in `{path}` for fingerprinting (class {cls[1]})."),
        class = "causalstress_fingerprint_error"
      )
    }
  }

  if (is.numeric(x)) {
    return(cs_tau_id(x))
  }
  if (is.integer(x) || is.logical(x) || is.character(x)) {
    return(unname(x))
  }

  rlang::abort(
    message = glue::glue("Unsupported type in `{path}` for fingerprinting (type {typeof(x)})."),
    class = "causalstress_fingerprint_error"
  )
}

cs_fingerprint_config_payload <- function(config = list()) {
  config_clean <- config
  if (!is.list(config_clean)) {
    config_clean <- list(value = config_clean)
  }
  runner_keys <- c("seed", "ci_method", "ci_method_source", "estimator_id", "num_threads")
  for (key in intersect(runner_keys, names(config_clean))) {
    config_clean[[key]] <- NULL
  }
  if (length(config_clean) == 0L) {
    config_clean <- list()
  }
  cs_normalize_for_fingerprint(config_clean, path = "config")
}

cs_build_config_fingerprint <- function(dgp_id, estimator_id, n, seed,
                                         bootstrap, B, oracle, estimator_version,
                                         config = list(), tau = cs_tau_oracle,
                                         max_runtime = Inf,
                                         dgp_version = NA_character_) {
  fingerprint_schema <- 4L
  ci_intent <- cs_ci_intent(config = config, bootstrap = bootstrap, B = B)
  config_norm <- cs_fingerprint_config_payload(config)

  digest::digest(
    list(
      config_fingerprint_schema = fingerprint_schema,
      dgp_id = dgp_id,
      dgp_version = as.character(dgp_version %||% NA_character_),
      estimator_id = estimator_id,
      n = as.integer(n),
      seed = as.integer(seed),
      bootstrap = as.logical(bootstrap),
      B = as.integer(B),
      oracle = as.logical(oracle),
      estimator_version = as.character(estimator_version),
      max_runtime = as.numeric(max_runtime),
      ci_intent = ci_intent,
      config = config_norm,
      tau_id = cs_tau_id(tau)
    ),
    algo = "sha256"
  )
}

cs_assert_schema4_resume <- function(stored_schema) {
  stored_schema <- suppressWarnings(as.integer(stored_schema %||% NA_integer_))
  if (is.na(stored_schema) || stored_schema < 4L) {
    rlang::abort(
      message = paste0(
        "Schema 1-3 artifacts are read-only historical inputs in v0.2.0 and ",
        "cannot be resumed into schema-4 runs. Use skip_existing = FALSE, ",
        "force = TRUE, or a fresh board."
      ),
      class = "causalstress_schema_migration_error"
    )
  }
  if (!identical(stored_schema, 4L)) {
    rlang::abort(
      message = glue::glue("Unsupported config fingerprint schema for resume: {stored_schema}."),
      class = "causalstress_schema_migration_error"
    )
  }
  invisible(TRUE)
}

cs_build_fit_fingerprint <- function(dgp_id, dgp_version, estimator_id,
                                     estimator_version, n, seed,
                                     config_fingerprint,
                                     config = list()) {
  digest::digest(
    list(
      fingerprint_version = 4L,
      artifact_type = "fit",
      dgp_id = as.character(dgp_id),
      dgp_version = as.character(dgp_version %||% NA_character_),
      estimator_id = as.character(estimator_id),
      estimator_version = as.character(estimator_version %||% NA_character_),
      n = as.integer(n),
      seed = as.integer(seed),
      config_fingerprint = as.character(config_fingerprint %||% NA_character_),
      transductive = as.logical(config$transductive %||% FALSE)
    ),
    algo = "sha256"
  )
}

cs_truth_version_id <- function(dgp_id, dgp_version, truth_payload = NULL) {
  digest::digest(
    list(
      fingerprint_version = 4L,
      artifact_type = "truth",
      dgp_id = as.character(dgp_id),
      dgp_version = as.character(dgp_version %||% NA_character_),
      truth_payload = cs_normalize_for_fingerprint(truth_payload %||% list(), path = "truth_payload")
    ),
    algo = "sha256"
  )
}

cs_build_score_fingerprint <- function(fit_fingerprint, estimand_target_id,
                                       metric_id, truth_version,
                                       scoring_population_id) {
  digest::digest(
    list(
      fingerprint_version = 4L,
      artifact_type = "score",
      fit_fingerprint = as.character(fit_fingerprint),
      estimand_target_id = as.character(estimand_target_id),
      metric_id = as.character(metric_id),
      truth_version = as.character(truth_version),
      scoring_population_id = as.character(scoring_population_id)
    ),
    algo = "sha256"
  )
}

cs_score_row_coordinate <- function(estimand_target_id,
                                    tau_id = NA_character_,
                                    score_status = "scored") {
  if (identical(estimand_target_id, "qst") &&
      !is.na(tau_id) &&
      nzchar(tau_id)) {
    return(tau_id)
  }

  if (estimand_target_id %in% c("att", "ate") &&
      identical(score_status, "scored")) {
    return("scalar")
  }

  "record_status"
}

cs_build_score_row_fingerprint <- function(score_fingerprint, row_coordinate) {
  digest::digest(
    list(
      fingerprint_version = 4L,
      artifact_type = "score_row",
      score_fingerprint = as.character(score_fingerprint),
      row_coordinate = as.character(row_coordinate)
    ),
    algo = "sha256"
  )
}

cs_build_config_fingerprint_schema2 <- function(dgp_id, estimator_id, n, seed,
                                                bootstrap, B, oracle, estimator_version,
                                                config = list(), tau = cs_tau_oracle,
                                                max_runtime = Inf) {
  config_clean <- config
  if (is.list(config_clean) && "seed" %in% names(config_clean)) {
    config_clean$seed <- NULL
  }
  ci_intent <- "default"
  if (isTRUE(bootstrap) && as.integer(B) > 0L) {
    ci_intent <- "bootstrap"
  } else if (is.list(config) && "ci_method" %in% names(config) && !is.null(config$ci_method)) {
    ci_intent <- as.character(config$ci_method[[1L]])
  }
  if (is.list(config_clean) && "ci_method" %in% names(config_clean)) {
    config_clean$ci_method <- NULL
  }
  config_norm <- cs_normalize_for_fingerprint(config_clean, path = "config")

  digest::digest(
    list(
      config_fingerprint_schema = 2L,
      dgp_id = dgp_id,
      estimator_id = estimator_id,
      n = as.integer(n),
      seed = as.integer(seed),
      bootstrap = as.logical(bootstrap),
      B = as.integer(B),
      oracle = as.logical(oracle),
      estimator_version = as.character(estimator_version),
      max_runtime = as.numeric(max_runtime),
      ci_intent = ci_intent,
      config = config_norm,
      tau_id = cs_tau_id(tau)
    ),
    algo = "sha256"
  )
}

cs_build_task_fingerprint <- function(dgp_id, dgp_version, estimator_id,
                                      estimator_version, n, seed, config = list(),
                                      tau = cs_tau_oracle, bootstrap = FALSE, B = 0L) {
  digest::digest(
    list(
      fingerprint_version = 4L,
      dgp_id = dgp_id,
      dgp_version = as.character(dgp_version %||% NA_character_),
      estimator_id = estimator_id,
      estimator_version = as.character(estimator_version %||% NA_character_),
      n = as.integer(n),
      seed = as.integer(seed),
      tau_id = cs_tau_id(tau),
      ci_intent = cs_ci_intent(config = config, bootstrap = bootstrap, B = B),
      config = cs_fingerprint_config_payload(config)
    ),
    algo = "sha256"
  )
}

# Legacy fingerprint for v0.1.7 pins (schema missing).
# This must remain stable to keep v0.1.7 artifacts resumable in v0.1.8.
cs_build_config_fingerprint_legacy <- function(dgp_id, estimator_id, n, seed,
                                               bootstrap, B, oracle, estimator_version,
                                               config = list(), tau = cs_tau_oracle) {
  config_clean <- config
  if (is.list(config_clean) && "seed" %in% names(config_clean)) {
    config_clean$seed <- NULL
  }

  ci_intent <- "default"
  if (isTRUE(bootstrap) && as.integer(B) > 0L) {
    ci_intent <- "bootstrap"
  } else if (is.list(config) && "ci_method" %in% names(config) && !is.null(config$ci_method)) {
    ci_intent <- as.character(config$ci_method[[1L]])
  }
  if (is.list(config_clean) && "ci_method" %in% names(config_clean)) {
    config_clean$ci_method <- NULL
  }

  config_norm <- cs_normalize_for_fingerprint(config_clean, path = "config")

  digest::digest(
    list(
      dgp_id = dgp_id,
      estimator_id = estimator_id,
      n = as.integer(n),
      seed = as.integer(seed),
      bootstrap = as.logical(bootstrap),
      B = as.integer(B),
      oracle = as.logical(oracle),
      estimator_version = as.character(estimator_version),
      ci_intent = ci_intent,
      config = config_norm,
      tau_id = cs_tau_id(tau)
    ),
    algo = "sha256"
  )
}
