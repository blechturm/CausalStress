#' Build a configuration fingerprint for a run
#'
#' Internal helper to generate a compact hash of key run settings for
#' resume safety.
#'
#' @keywords internal
#' @noRd
#' @importFrom digest digest
cs_build_config_fingerprint <- function(dgp_id, estimator_id, n, seed,
                                         bootstrap, B, oracle, estimator_version,
                                         config = list(), tau = cs_tau_oracle) {
  # `seed` is a per-task identifier (already part of the pin name and the
  # fingerprint payload). Keeping it inside `config` would create unnecessary
  # mismatches when comparing runs across versions that did/didn't inject
  # `config$seed`.
  config_clean <- config
  if (is.list(config_clean) && "seed" %in% names(config_clean)) {
    config_clean$seed <- NULL
  }
  # `ci_method` is a per-run CI intent; it should not affect the estimator's
  # defaults unless the user explicitly passes it. We capture it separately
  # (including the "default" case) and drop it from the config payload to avoid
  # accidental changes in runner injection behavior changing fingerprints.
  ci_intent <- "default"
  if (isTRUE(bootstrap) && as.integer(B) > 0L) {
    ci_intent <- "bootstrap"
  } else if (is.list(config) && "ci_method" %in% names(config) && !is.null(config$ci_method)) {
    ci_intent <- as.character(config$ci_method[[1L]])
  }
  if (is.list(config_clean) && "ci_method" %in% names(config_clean)) {
    config_clean$ci_method <- NULL
  }

  normalize_for_fingerprint <- function(x, path = "config") {
    # Allow only deterministic, serializable primitives (and lists thereof).
    if (is.null(x)) return(NULL)

    # Reject environments, functions, external pointers, S4, etc.
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
        out[[i]] <- normalize_for_fingerprint(x[[i]], path = paste0(path, "$", key))
      }
      if (!is.null(nm)) names(out) <- nm
      return(out)
    }

    # Atomic vectors: allow base logical/integer/double/character without class.
    cls <- class(x)
    if (!is.null(cls) && !identical(cls, typeof(x))) {
      # factors, POSIXct, Date, etc. are not allowed
      if (!identical(cls, "numeric") && !identical(cls, "integer") && !identical(cls, "logical") && !identical(cls, "character")) {
        rlang::abort(
          message = glue::glue("Classed value not allowed in `{path}` for fingerprinting (class {cls[1]})."),
          class = "causalstress_fingerprint_error"
        )
      }
    }

    if (is.numeric(x)) {
      # Canonicalize numeric to a stable character representation to avoid
      # platform-level float drift.
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

  config_norm <- normalize_for_fingerprint(config_clean, path = "config")

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
