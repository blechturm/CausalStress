#' Heavy-tailed synthetic DGP for CausalStress (v1.3.0)
#'
#' Implements the `synth_heavytail` design from the DGP registry. This DGP
#' shares the same covariates, baseline outcome, treatment effect, and
#' propensity as `synth_baseline`, but replaces the Gaussian noise with a
#' heavy-tailed mixture:
#'   epsilon ~ 0.8 * N(0, 0.5) + 0.2 * Cauchy(0, 1).
#'
#' @param n Integer, number of observations.
#' @param seed Optional seed for reproducibility (passed to `cs_set_rng()`).
#'
#' @return A list with:
#'   - df: tibble with columns `y`, `w`, `y0`, `y1`, `p`, `structural_te`,
#'     `X1`, `X2`, `X3`, `X4`, `X5`
#'   - true_att: numeric scalar (structural ATT for treated units)
#'   - true_qst: tibble with columns `tau` and `value` (QST truth on
#'     `cs_tau_oracle` grid)
#'   - meta: list with fields `dgp_id = "synth_heavytail"`, `type = "synthetic"`,
#'     `structural_te = tau`
#'
#' @export
dgp_synth_heavytail_v130 <- function(n, seed = NULL) {
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
  p <- stats::plogis(0.5 * X1 - 0.5 * X2)

  w <- stats::rbinom(n, size = 1, prob = p)

  mix_ind0 <- stats::rbinom(n, size = 1, prob = 0.8)
  mix_ind1 <- stats::rbinom(n, size = 1, prob = 0.8)

  eps0 <- ifelse(
    mix_ind0 == 1L,
    stats::rnorm(n, mean = 0, sd = 0.5),
    stats::rcauchy(n, location = 0, scale = 1)
  )

  eps1 <- ifelse(
    mix_ind1 == 1L,
    stats::rnorm(n, mean = 0, sd = 0.5),
    stats::rcauchy(n, location = 0, scale = 1)
  )

  y0 <- mu0 + eps0
  y1 <- mu0 + tau + eps1
  y <- ifelse(w == 1L, y1, y0)

  true_att <- cs_true_att(structural_te = tau, w = w)
  true_qst <- cs_get_oracle_qst("synth_heavytail")

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
      dgp_id        = "synth_heavytail",
      type          = "synthetic",
      structural_te = tau
    )
  )

  cs_check_dgp_synthetic(out)

  out
}

#' @export
dgp_synth_heavytail <- function(n, seed = NULL) {
  dgp_synth_heavytail_v130(n = n, seed = seed)
}
