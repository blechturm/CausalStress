#' Kang-Schafer placebo synthetic DGP (sharp null, v1.4.0)
#'
#' Implements the `synth_placebo_kangschafer` design: latent Gaussian covariates
#' transformed nonlinearly to observed X, linear outcome/propensity in latent Z,
#' and sharp-null treatment effect (Y1 == Y0 pathwise).
#'
#' @details
#' \strong{Goal:} Canonical misspecification trap (Kang–Schafer) adapted as a sharp-null placebo; any nonzero effect indicates functional-form failure.
#'
#' \strong{Description:} Data are generated from latent Gaussian covariates \eqn{Z}, but the analyst only observes nonlinear transforms \eqn{X = f(Z)}. True outcome regression and propensity are linear in \eqn{Z} yet become severely misspecified when fit linearly in the observed \eqn{X}. Treatment effect is zero pathwise.
#'
#' \strong{Generative structure:}
#' \itemize{
#'   \item \strong{Latent covariates (unobserved):} \eqn{Z = (Z_1, Z_2, Z_3, Z_4)^\top \sim \mathcal{N}_4(0, I_4)}.
#'   \item \strong{Observed covariates (returned):}
#'     \deqn{X_1 = \exp(Z_1/2),\quad X_2 = \frac{Z_2}{1 + \exp(Z_1)} + 10,}
#'     \deqn{X_3 = (Z_1 Z_3 / 25 + 0.6)^3,\quad X_4 = (Z_2 + Z_4 + 20)^2.}
#'   \item \strong{Propensity (in \eqn{Z}):} \eqn{p(Z) = \mathrm{expit}(-Z_1 + 0.5 Z_2 - 0.25 Z_3 - 0.1 Z_4)}, with \eqn{W \sim \mathrm{Bernoulli}(p(Z))}.
#'   \item \strong{Baseline outcome (in \eqn{Z}):} \deqn{\mu_0(Z) = 210 + 27.4 Z_1 + 13.7 Z_2 + 13.7 Z_3 + 13.7 Z_4,}
#'     \eqn{Y_0 = \mu_0(Z) + \varepsilon}, \eqn{\varepsilon \sim \mathcal{N}(0,1)}.
#'   \item \strong{Treatment effect (sharp null):} \eqn{\tau(Z) \equiv 0}, so \eqn{Y_1 = Y_0} and \eqn{Y = Y_0}.
#' }
#'
#' \strong{Returned fields:} `df` contains `y`, `w`, `y0`, `y1`, `p`, `structural_te`, `X1`–`X4`; `structural_te` is all zeros; `true_att = 0`; `true_qst` is identically zero on `cs_tau_oracle()`.
#'
#' \strong{Challenge:} Severe outcome and propensity misspecification in observed \eqn{X}-space; belongs in the placebo suite.
#'
#' @param n Integer, number of observations.
#' @param seed Optional seed for reproducibility (passed to `cs_set_rng()`).
#'
#' @return A synthetic DGP list with df, true_att, true_qst, meta.
#' @export
dgp_synth_placebo_kangschafer_v140 <- function(n, seed = NULL, include_truth = TRUE, oracle_only = FALSE) {
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
  if (isTRUE(oracle_only)) {
    return(list(df = tibble::tibble(w = w, y0 = y0, y1 = y1)))
  }
  y  <- ifelse(w == 1L, y1, y0)

  tau <- rep(0, n)

  true_att <- cs_true_att(structural_te = tau, w = w)
  true_qst <- if (isTRUE(include_truth)) cs_get_oracle_qst("synth_placebo_kangschafer", version = "1.4.0") else NULL

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

#' @export
dgp_synth_placebo_kangschafer <- function(n, seed = NULL) {
  dgp_synth_placebo_kangschafer_v140(n = n, seed = seed)
}
