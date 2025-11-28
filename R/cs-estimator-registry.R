#' @noRd
cs_estimator_registry <- function() {
  tibble::tibble(
    estimator_id = c("oracle_att", "lm_att"),
    oracle = c(TRUE, FALSE),
    supports_qst = c(FALSE, FALSE),
    fn = list(
      est_oracle_att,
      est_lm_att
    ),
    version = c("0.0.1", "0.0.1"),
    description = c(
      "Oracle ATT estimator using structural treatment effects tau(X); for internal testing only.",
      "Linear outcome regression ATT estimator (g-comp) based on control units."
    )
  )
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
#'   - fn: function(df, tau, config) returning an estimator result
#'   - oracle: logical, TRUE if this is an oracle estimator
#'   - supports_qst: logical, TRUE if estimator returns QST
#'   - version: character scalar
#'   - description: character scalar
#'
#' @export
cs_get_estimator <- function(estimator_id) {
  if (!is.character(estimator_id) || length(estimator_id) != 1L) {
    rlang::abort(
      message = "`estimator_id` must be a character scalar.",
      class   = "causalstress_registry_error"
    )
  }

  reg <- cs_estimator_registry()
  rows <- reg[reg$estimator_id == estimator_id, , drop = FALSE]

  if (nrow(rows) == 0) {
    rlang::abort(
      message = glue::glue("Unknown estimator_id: {estimator_id}."),
      class   = "causalstress_registry_error"
    )
  }
  if (nrow(rows) > 1) {
    rlang::abort(
      message = glue::glue("Duplicate entries for estimator_id: {estimator_id}."),
      class   = "causalstress_registry_error"
    )
  }

  list(
    estimator_id = rows$estimator_id,
    fn           = rows$fn[[1L]],
    oracle       = rows$oracle,
    supports_qst = rows$supports_qst,
    version      = rows$version,
    description  = rows$description
  )
}
