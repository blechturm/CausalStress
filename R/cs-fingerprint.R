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

  normalize_list <- function(x) {
    if (!is.list(x)) return(x)
    nm <- names(x)
    if (is.null(nm)) {
      return(lapply(x, normalize_list))
    }
    ord <- order(nm)
    x <- x[ord]
    lapply(x, normalize_list)
  }

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
      config = normalize_list(config_clean),
      tau_id = cs_tau_id(tau)
    ),
    algo = "sha256"
  )
}
