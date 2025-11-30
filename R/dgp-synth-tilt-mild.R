#' Mildly tilted propensity synthetic DGP for CausalStress
#'
#' Implements the `synth_tilt_mild` design from the DGP registry. Uses the same
#' outcome and noise as `synth_baseline` but a mildly tilted propensity to
#' induce covariate shift.
#'
#' @param n Integer, number of observations.
#' @param seed Optional seed for reproducibility (passed to `cs_set_rng()`).
#'
#' @return A list with df, true_att, true_qst, and meta following the synthetic
#'   DGP contract.
#' @export
dgp_synth_tilt_mild <- function(n, seed = NULL) {
  if (!is.null(seed)) {
    cs_set_rng(seed)
  }

  X1 <- stats::rnorm(n, mean = 0, sd = 1)
  X2 <- stats::rnorm(n, mean = 0, sd = 1)
  X3 <- stats::rnorm(n, mean = 0, sd = 1)
  X4 <- stats::rnorm(n, mean = 0, sd = 1)
  X5 <- stats::rnorm(n, mean = 0, sd = 1)

  mu0 <- 1 + X1 + 0.5 * X2
  tau <- 1 + 0.5 * X1
  p   <- stats::plogis(0.45 * X1 - 0.3 * X2 - 0.25 * X4)

  w <- stats::rbinom(n, size = 1L, prob = p)

  eps0 <- stats::rnorm(n, mean = 0, sd = 0.5)
  eps1 <- stats::rnorm(n, mean = 0, sd = 0.5)

  y0 <- mu0 + eps0
  y1 <- mu0 + tau + eps1
  y  <- ifelse(w == 1L, y1, y0)

  true_att <- cs_true_att(structural_te = tau, w = w)
  true_qst <- cs_get_oracle_qst("synth_tilt_mild")

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
      X4 = X4,
      X5 = X5
    ),
    true_att = true_att,
    true_qst = true_qst,
    meta = list(
      dgp_id        = "synth_tilt_mild",
      type          = "synthetic",
      structural_te = tau
    )
  )

  cs_check_dgp_synthetic(out)
  out
}
