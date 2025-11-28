#' Oracle quantile grid
#'
#' Constant sequence of tau values used for oracle quantile targets.
#'
#' @return Numeric vector of quantile levels from 0.01 to 0.99.
#' @export
cs_tau_oracle <- seq(0.01, 0.99, by = 0.01)
