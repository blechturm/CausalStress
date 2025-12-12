#' Placebo nonlinear synthetic DGP (sharp null, v1.3.0)
#'
#' Implements the `synth_placebo_nonlinear` design: nonlinear baseline outcome
#' with sharp-null treatment effect (Y1 == Y0 pathwise).
#'
#' @details
#' \strong{Placebo contract (sharp null):} \eqn{\tau(X) \equiv 0} and \eqn{Y_1 \equiv Y_0}; `structural_te` zeros, `true_att = 0`, `true_qst` zero on `cs_tau_oracle()`.
#'
#' \strong{Registry nuance (this variant):} Baseline outcome \eqn{\mu_0 = \sin(X_1) + \cos(X_2)}; propensity and noise match \code{synth_baseline} (\eqn{p(X) = \mathrm{plogis}(0.5 X_1 - 0.5 X_2)}, \eqn{\varepsilon \sim \mathcal{N}(0, 0.5)}).
#'
#' @param n Integer, number of observations.
#' @param seed Optional seed for reproducibility (passed to `cs_set_rng()`).
#'
#' @return A synthetic DGP list with df, true_att, true_qst, meta.
#' @export
dgp_synth_placebo_nonlinear_v130 <- function(n, seed = NULL) {
  if (!is.null(seed)) {
    cs_set_rng(seed)
  }

  X1 <- stats::rnorm(n, mean = 0, sd = 1)
  X2 <- stats::rnorm(n, mean = 0, sd = 1)
  X3 <- stats::rnorm(n, mean = 0, sd = 1)
  X4 <- stats::rnorm(n, mean = 0, sd = 1)
  X5 <- stats::rnorm(n, mean = 0, sd = 1)

  mu0 <- sin(X1) + cos(X2)
  tau <- rep(0, n)

  p <- stats::plogis(0.5 * X1 - 0.5 * X2)
  w <- stats::rbinom(n, size = 1L, prob = p)

  eps <- stats::rnorm(n, mean = 0, sd = 0.5)

  y0 <- mu0 + eps
  y1 <- y0
  y  <- ifelse(w == 1L, y1, y0)

  true_att <- cs_true_att(structural_te = tau, w = w)
  true_qst <- cs_get_oracle_qst("synth_placebo_nonlinear")

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
      dgp_id        = "synth_placebo_nonlinear",
      type          = "synthetic",
      structural_te = tau
    )
  )

  cs_check_dgp_synthetic(out)
  out
}

#' @export
dgp_synth_placebo_nonlinear <- function(n, seed = NULL) {
  dgp_synth_placebo_nonlinear_v130(n = n, seed = seed)
}
