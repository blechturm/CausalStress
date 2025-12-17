#' Oracle quantile grid
#'
#' Constant sequence of tau values used for oracle quantile targets.
#'
#' @return Numeric vector of quantile levels from 0.01 to 0.99.
#' @export
cs_tau_oracle <- seq(0.01, 0.99, by = 0.01)

# Canonical identifier for tau grid points.
# Used to avoid floating-point drift when joining/grouping/fingerprinting.
#
# Keep numeric `tau` for estimator inputs; promote to `tau_id` downstream.
#
# @keywords internal
# @noRd
cs_tau_id <- function(tau) {
  if (is.null(tau)) return(character(0))
  tau <- as.numeric(tau)
  if (!length(tau)) return(character(0))

  # Round aggressively to collapse platform-level float noise, then format in a
  # stable, human-readable way (no scientific notation).
  tau_round <- round(tau, 12)
  out <- formatC(tau_round, format = "f", digits = 12)
  out <- sub("0+$", "", out)
  out <- sub("\\.$", "", out)
  out
}
