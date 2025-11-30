#' Kang-Schafer placebo synthetic DGP (sharp null)
#'
#' Implements the `synth_placebo_kangschafer` design: latent Gaussian covariates
#' transformed nonlinearly to observed X, linear outcome/propensity in latent Z,
#' and sharp-null treatment effect (Y1 == Y0 pathwise).
#'
#' @param n Integer, number of observations.
#' @param seed Optional seed for reproducibility (passed to `cs_set_rng()`).
#'
#' @return A synthetic DGP list with df, true_att, true_qst, meta.
#' @export
dgp_synth_placebo_kangschafer <- function(n, seed = NULL) {
  if (!is.null(seed)) {
    cs_set_rng(seed)
  }

  Z1 <- stats::rnorm(n, mean = 0, sd = 1)
  Z2 <- stats::rnorm(n, mean = 0, sd = 1)
  Z3 <- stats::rnorm(n, mean = 0, sd = 1)
  Z4 <- stats::rnorm(n, mean = 0, sd = 1)

  X1 <- exp(Z1 / 2)
  X2 <- Z2 / (1 + exp(Z1)) + 10
  X3 <- (Z1 * Z3 / 25 + 0.6)^3
  X4 <- (Z2 + Z4 + 20)^2

  lin_ps <- -Z1 + 0.5 * Z2 - 0.25 * Z3 - 0.1 * Z4
  p      <- stats::plogis(lin_ps)
  w      <- stats::rbinom(n, size = 1L, prob = p)

  mu0 <- 210 + 27.4 * Z1 + 13.7 * Z2 + 13.7 * Z3 + 13.7 * Z4
  eps <- stats::rnorm(n, mean = 0, sd = 1)

  y0 <- mu0 + eps
  y1 <- y0
  y  <- ifelse(w == 1L, y1, y0)

  tau <- rep(0, n)

  true_att <- cs_true_att(structural_te = tau, w = w)
  true_qst <- cs_get_oracle_qst("synth_placebo_kangschafer")

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
      dgp_id        = "synth_placebo_kangschafer",
      type          = "synthetic",
      structural_te = tau
    )
  )

  cs_check_dgp_synthetic(out)
  out
}
