#' Validate that a truth tibble uses the canonical tau grid
#'
#' Ensures a QST oracle truth object carries the canonical tau grid
#' `cs_tau_oracle()` in the correct order with no missing or extra values.
#'
#' @param truth A tibble/data frame containing at least a `tau` column.
#'
#' @return Invisible `TRUE` if validation passes; otherwise aborts.
#' @export
cs_validate_tau_grid <- function(truth) {
  if (is.null(truth) || !is.data.frame(truth)) {
    cli::cli_abort("`truth` must be a data.frame/tibble.")
  }
  if (!"tau" %in% names(truth)) {
    cli::cli_abort("`truth` must contain a `tau` column.")
  }
  canonical <- cs_tau_oracle
  tau_vals <- truth$tau
  if (!is.numeric(tau_vals)) {
    cli::cli_abort("`tau` column must be numeric.")
  }
  if (!identical(as.numeric(tau_vals), canonical)) {
    cli::cli_abort("`tau` grid does not match the canonical cs_tau_oracle().")
  }
  invisible(TRUE)
}
