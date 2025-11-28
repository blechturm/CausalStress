#' True ATT calculator
#'
#' Computes the average treatment effect on the treated from structural treatment effects.
#'
#' @param structural_te Numeric vector of structural treatment effects.
#' @param w Binary treatment indicator vector where treated units equal 1.
#' @return Numeric scalar giving the mean treatment effect for treated units.
#' @export
cs_true_att <- function(structural_te, w) {
  mean(structural_te[w == 1])
}

#' QST truth from potential outcomes
#'
#' Computes quantile-specific treatment effects for treated units.
#'
#' @param y0 Numeric vector of potential outcomes under control.
#' @param y1 Numeric vector of potential outcomes under treatment.
#' @param w Binary treatment indicator vector where treated units equal 1.
#' @param tau Numeric vector of quantile levels; defaults to `cs_tau_oracle`.
#' @return Tibble with columns `tau` and `value` containing QST values.
#' @export
cs_true_qst <- function(y0, y1, w, tau = cs_tau_oracle) {
  treated <- w == 1
  q1 <- stats::quantile(y1[treated], probs = tau, names = FALSE)
  q0 <- stats::quantile(y0[treated], probs = tau, names = FALSE)
  value <- q1 - q0
  tibble::tibble(tau = tau, value = value)
}
