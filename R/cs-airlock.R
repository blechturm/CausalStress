#' Internal helper: compute granted oracle columns
#'
#' @noRd
cs_oracle_columns_granted <- function(config = list(), estimator_desc = list()) {
  config <- config %||% list()
  eligible <- estimator_desc$oracle_columns %||% character(0)
  defaults <- estimator_desc$oracle_default_columns %||% character(0)

  supported <- c("p", "structural_te")
  bad_eligible <- setdiff(eligible, supported)
  bad_defaults <- setdiff(defaults, supported)
  if (length(bad_eligible) > 0L || length(bad_defaults) > 0L) {
    rlang::abort(
      message = "Estimator descriptor contains unsupported oracle column grants.",
      class = "causalstress_airlock_error"
    )
  }
  if (length(setdiff(defaults, eligible)) > 0L) {
    rlang::abort(
      message = "`oracle_default_columns` must be a subset of `oracle_columns`.",
      class = "causalstress_airlock_error"
    )
  }

  requested <- character(0)
  if (isTRUE(config$use_true_propensity)) {
    requested <- c(requested, "p")
  }
  if (isTRUE(config$use_structural_te)) {
    requested <- c(requested, "structural_te")
  }

  denied <- setdiff(requested, eligible)
  if (length(denied) > 0L) {
    rlang::abort(
      message = glue::glue(
        "Estimator '{estimator_desc$estimator_id %||% '<unknown>'}' requested ineligible oracle column(s): {paste(denied, collapse = ', ')}."
      ),
      class = "causalstress_airlock_error"
    )
  }

  unique(c(defaults, requested))
}

#' Internal helper: enforce Airlock using column-scoped oracle grants
#'
#' @noRd
cs_airlock <- function(df, config = list(), estimator_desc = list()) {
  drop <- c("y0", "y1", "p", "structural_te")
  granted <- cs_oracle_columns_granted(config = config, estimator_desc = estimator_desc)
  keep <- c(setdiff(names(df), drop), intersect(granted, names(df)))

  df2 <- df[, keep, drop = FALSE]

  # Strip all attributes except the data.frame essentials to prevent
  # attribute-based truth leakage (side-channel), including oracle grants.
  attrs <- attributes(df2)
  keep_attrs <- c("names", "row.names", "class")
  attributes(df2) <- attrs[intersect(names(attrs), keep_attrs)]

  df2
}
