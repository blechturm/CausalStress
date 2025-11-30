#' Nonlinear heteroskedastic synthetic DGP for CausalStress
#'
#' Implements the `synth_nonlinear_heteroskedastic` design from the
#' DGP registry. This DGP has nonlinear baseline outcome, constant
#' treatment effect, and heteroskedastic Gaussian noise.
#'
#' @param n Integer, number of observations.
#' @param seed Optional seed for reproducibility (passed to `cs_set_rng()`).
#'
#' @return A list with `df`, `true_att`, `true_qst`, and `meta` satisfying
#'   the synthetic DGP contract.
#' @export
dgp_synth_nonlinear_heteroskedastic <- function(n, seed = NULL) {
  if (!is.null(seed)) {
    cs_set_rng(seed)
  }

  X1 <- stats::rnorm(n, mean = 0, sd = 1)
  X2 <- stats::rnorm(n, mean = 0, sd = 1)
  X3 <- stats::runif(n, min = -2, max = 2)
  X4 <- stats::rbinom(n, size = 1L, prob = 0.4)

  mu0 <- 1 + 0.8 * sin(X1) + 0.5 * X2^2 - 0.3 * X4
  tau <- rep(1.0, n)

  sigma <- 0.3 + 0.2 * abs(X3)
  eps0  <- stats::rnorm(n, mean = 0, sd = sigma)
  eps1  <- stats::rnorm(n, mean = 0, sd = sigma)

  p <- stats::plogis(0.5 * X1 - 0.5 * X2)

  w <- stats::rbinom(n, size = 1L, prob = p)

  y0 <- mu0 + eps0
  y1 <- mu0 + tau + eps1
  y  <- ifelse(w == 1L, y1, y0)

  true_att <- cs_true_att(structural_te = tau, w = w)
  true_qst <- cs_get_oracle_qst("synth_nonlinear_heteroskedastic")

  out <- list(
    df = tibble::tibble(
      y  = y,
      w  = w,
      y0 = y0,
      y1 = y1,
      p  = p,
      structural_te = tau,
      X1 = X1,
      X2 = X2,
      X3 = X3,
      X4 = X4
    ),
    true_att = true_att,
    true_qst = true_qst,
    meta = list(
      dgp_id        = "synth_nonlinear_heteroskedastic",
      type          = "synthetic",
      structural_te = tau
    )
  )

  cs_check_dgp_synthetic(out)
  out
}
