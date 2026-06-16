# Internal environment to hold dynamically registered estimators
.causalstress_estimator_registry_extra <- new.env(parent = emptyenv())
.causalstress_estimator_registry_extra$tbl <- tibble::tibble(
  estimator_id  = character(),
  type          = character(),
  generator     = list(),
  oracle        = logical(),
  oracle_columns = list(),
  oracle_default_columns = list(),
  supports_qst  = logical(),
  version       = character(),
  description   = character(),
  source        = character(),
  requires_pkgs = list()
)

#' Base estimator registry (core estimators shipped with the package)
#'
#' @noRd
cs_estimator_registry_base <- function() {
  pkg_ver <- as.character(utils::packageVersion("CausalStress"))

  tibble::tibble(
    estimator_id = c("oracle_att", "lm_att", "ipw_att", "gengc", "gengc_dr", "grf_dr_att", "bart_att", "tmle_att"),
    type = c("oracle", "gcomp", "ipw", "gcomp", "gcomp", "dr", "gcomp", "dr"),
    generator = list(
      est_oracle_att,
      est_lm_att,
      est_ipw_att,
      est_gengc,
      est_gengc_dr,
      est_grf_dr_att,
      est_bart_att,
      est_tmle_att
    ),
    oracle = c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE),
    oracle_columns = list(
      "structural_te",
      character(0),
      character(0),
      character(0),
      character(0),
      character(0),
      character(0),
      character(0)
    ),
    oracle_default_columns = list(
      "structural_te",
      character(0),
      character(0),
      character(0),
      character(0),
      character(0),
      character(0),
      character(0)
    ),
    supports_qst = c(FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE),
    version = rep(pkg_ver, 8L),
    description = c(
      "Oracle ATT/ATE using structural treatment effects.",
      "Linear outcome regression g-computation ATT estimator.",
      "Inverse-probability weighted ATT estimator based on logistic propensity.",
      "GenGC distributional estimator (ATT + QST); supports dual-engine (qrf/qr) and optional screening.",
      "GenGC doubly-robust distributional estimator (ATT + QST); supports dual-engine (qrf/qr) and optional screening.",
      "GRF causal forest doubly-robust ATT estimator.",
      "BART ATT estimator via bartCause::bartc.",
      "TMLE ATT estimator via tmle::tmle."
    ),
    source = c("core", "core", "core", "optional", "optional", "optional", "optional", "optional"),
    requires_pkgs = list(
      character(0),
      character(0),
      character(0),
      "GenGC",
      "GenGC",
      "grf",
      c("bartCause"),
      c("tmle", "SuperLearner")
    )
  )
}

#' Estimator registry (core + dynamically registered estimators)
#'
#' @noRd
cs_estimator_registry <- function() {
  base <- cs_estimator_registry_base()
  extra <- .causalstress_estimator_registry_extra$tbl

  if (nrow(extra) == 0L) {
    base
  } else {
    rbind(base, extra)
  }
}

#' Register an additional estimator at runtime
#'
#' This function allows external packages or user code to register new estimators
#' into the CausalStress estimator registry. Registered estimators must obey the
#' estimator contract: the `generator` function must accept `(df, config, tau, ...)`
#' and return a list compatible with `cs_check_estimator_output()`.
#'
#' @param estimator_id Character scalar, unique identifier for the estimator.
#' @param type Character scalar describing the estimator family
#'   (e.g. "gcomp", "ipw", "dr", "rf", "external").
#' @param generator Function that implements the estimator, taking at least
#'   arguments `(df, config = list(), tau = cs_tau_oracle, ...)`.
#' @param oracle Logical, whether this is an oracle estimator.
#' @param oracle_columns Character vector of truth columns this estimator is
#'   eligible to receive through the runner airlock. Allowed values in v0.1.10
#'   are `"p"` and `"structural_te"`.
#' @param oracle_default_columns Character vector of eligible truth columns the
#'   runner grants without a user config flag. This should be empty except for
#'   internal benchmark estimators that cannot function otherwise.
#' @param supports_qst Logical, whether the estimator returns QST values.
#' @param version Character scalar, version string for the estimator implementation.
#'   Defaults to `"0.0.0-local"`.
#' @param description Character scalar, short human-readable description.
#' @param source Character scalar indicating where the estimator comes from,
#'   e.g. "external", "optional". Defaults to `"external"`.
#' @param requires_pkgs Character vector of package names that must be installed
#'   for the estimator to work. Defaults to `character(0)`.
#'
#' @return Invisibly, the updated registry tibble (core + extra).
#' @export
cs_register_estimator <- function(
  estimator_id,
  type,
  generator,
  oracle        = FALSE,
  oracle_columns = character(0),
  oracle_default_columns = character(0),
  supports_qst  = FALSE,
  version       = "0.0.0-local",
  description   = "",
  source        = "external",
  requires_pkgs = character(0)
) {
  if (!is.character(estimator_id) || length(estimator_id) != 1L) {
    rlang::abort(
      "`estimator_id` must be a character scalar.",
      class = "causalstress_registry_error"
    )
  }
  if (!is.character(type) || length(type) != 1L) {
    rlang::abort(
      "`type` must be a character scalar.",
      class = "causalstress_registry_error"
    )
  }
  if (!is.function(generator)) {
    rlang::abort(
      "`generator` must be a function.",
      class = "causalstress_registry_error"
    )
  }
  if (!is.character(version) || length(version) != 1L) {
    rlang::abort(
      "`version` must be a character scalar.",
      class = "causalstress_registry_error"
    )
  }
  if (!is.character(description) || length(description) != 1L) {
    rlang::abort(
      "`description` must be a character scalar.",
      class = "causalstress_registry_error"
    )
  }
  if (!is.character(source) || length(source) != 1L) {
    rlang::abort(
      "`source` must be a character scalar.",
      class = "causalstress_registry_error"
    )
  }
  if (!is.character(requires_pkgs)) {
    rlang::abort(
      "`requires_pkgs` must be a character vector.",
      class = "causalstress_registry_error"
    )
  }
  supported_oracle_columns <- c("p", "structural_te")
  if (!is.character(oracle_columns)) {
    rlang::abort(
      "`oracle_columns` must be a character vector.",
      class = "causalstress_registry_error"
    )
  }
  if (!is.character(oracle_default_columns)) {
    rlang::abort(
      "`oracle_default_columns` must be a character vector.",
      class = "causalstress_registry_error"
    )
  }
  bad_cols <- setdiff(unique(c(oracle_columns, oracle_default_columns)), supported_oracle_columns)
  if (length(bad_cols) > 0L) {
    rlang::abort(
      glue::glue("Unsupported oracle column(s): {paste(bad_cols, collapse = ', ')}."),
      class = "causalstress_registry_error"
    )
  }
  if (length(setdiff(oracle_default_columns, oracle_columns)) > 0L) {
    rlang::abort(
      "`oracle_default_columns` must be a subset of `oracle_columns`.",
      class = "causalstress_registry_error"
    )
  }
  if (!isTRUE(oracle) && length(oracle_columns) > 0L) {
    rlang::abort(
      "`oracle_columns` requires `oracle = TRUE`.",
      class = "causalstress_registry_error"
    )
  }

  reg <- cs_estimator_registry()
  if (estimator_id %in% reg$estimator_id) {
    rlang::abort(
      glue::glue("Estimator id '{estimator_id}' is already registered."),
      class = "causalstress_registry_error"
    )
  }

  extra <- .causalstress_estimator_registry_extra$tbl
  new_row <- tibble::tibble(
    estimator_id  = estimator_id,
    type          = type,
    generator     = list(generator),
    oracle        = as.logical(oracle),
    oracle_columns = list(unique(oracle_columns)),
    oracle_default_columns = list(unique(oracle_default_columns)),
    supports_qst  = as.logical(supports_qst),
    version       = version,
    description   = description,
    source        = source,
    requires_pkgs = list(requires_pkgs)
  )

  .causalstress_estimator_registry_extra$tbl <- rbind(extra, new_row)

  invisible(cs_estimator_registry())
}

#' Get an estimator descriptor by ID
#'
#' Look up an estimator in the internal registry and return a descriptor
#' containing metadata and the estimator function.
#'
#' @param estimator_id Character scalar, the estimator identifier
#'   (e.g., "oracle_att").
#'
#' @return A list with elements:
#'   - estimator_id: character scalar
#'   - type: character scalar
#'   - fn: function(df, tau, config) returning an estimator result
#'   - oracle: logical, TRUE if this is an oracle estimator
#'   - oracle_columns: character vector of eligible oracle columns
#'   - oracle_default_columns: character vector of default-granted oracle columns
#'   - supports_qst: logical, TRUE if estimator returns QST
#'   - version: character scalar
#'   - description: character scalar
#'   - source: character scalar indicating origin (core/optional/external)
#'   - requires_pkgs: character vector of required packages
#'
#' @export
cs_get_estimator <- function(estimator_id) {
  reg <- cs_estimator_registry()
  row <- reg[reg$estimator_id == estimator_id, , drop = FALSE]

  if (nrow(row) == 0L) {
    rlang::abort(
      message = glue::glue("Unknown estimator id: {estimator_id}"),
      class   = "causalstress_registry_error"
    )
  }

  list(
    estimator_id  = row$estimator_id[[1L]],
    type          = row$type[[1L]],
    generator     = row$generator[[1L]],
    oracle        = row$oracle[[1L]],
    oracle_columns = row$oracle_columns[[1L]],
    oracle_default_columns = row$oracle_default_columns[[1L]],
    supports_qst  = row$supports_qst[[1L]],
    version       = row$version[[1L]],
    description   = row$description[[1L]],
    source        = row$source[[1L]],
    requires_pkgs = row$requires_pkgs[[1L]]
  )
}
