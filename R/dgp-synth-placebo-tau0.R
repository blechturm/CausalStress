#' Sharp-null placebo synthetic DGP (tau = 0, v1.3.0)
#'
#' Generates a placebo DGP where the structural treatment effect is identically
#' zero and the potential outcomes are pathwise identical (Y1 == Y0). This
#' design is intended for gatekeeper/placebo checks.
#'
#' @details
#' \strong{Placebo contract (sharp null):} \eqn{\tau(X) \equiv 0} and \eqn{Y_1 \equiv Y_0} pathwise; `structural_te` is all zeros, `true_att = 0`, `true_qst` is zero on the `cs_tau_oracle()` grid.
#'
#' \strong{Registry nuance (this variant):} Outcome, noise, and propensity match \code{synth_baseline}: \eqn{\mu_0(X) = 1 + X_1 + 0.5 X_2}, \eqn{p(X) = \mathrm{plogis}(0.5 X_1 - 0.5 X_2)}, \eqn{\varepsilon \sim \mathcal{N}(0, 0.5)}.
#'
#' @param n Integer, number of observations.
#' @param seed Optional seed for reproducibility (passed to `cs_set_rng()`).
#'
#' @return A list with components:
#'   - df: tibble with columns `y`, `w`, `y0`, `y1`, `p`, `structural_te`,
#'     `X1`-`X5`
#'   - true_att: numeric scalar (ATT truth; zero)
#'   - true_qst: tibble with columns `tau` and `value` (all zeros)
#'   - meta: list with `dgp_id`, `type`, and `structural_te`
#' @export
dgp_synth_placebo_tau0_v130 <- function(n, seed = NULL, include_truth = TRUE, oracle_only = FALSE) {
  if (!is.null(seed)) {
    cs_set_rng(seed)
  }

  X1 <- stats::rnorm(n, mean = 0, sd = 1)
  X2 <- stats::rnorm(n, mean = 0, sd = 1)
  X3 <- stats::rnorm(n, mean = 0, sd = 1)
  X4 <- stats::rnorm(n, mean = 0, sd = 1)
  X5 <- stats::rnorm(n, mean = 0, sd = 1)

  mu0 <- 1 + X1 + 0.5 * X2
  tau <- rep(0, n)
  p   <- stats::plogis(0.5 * X1 - 0.5 * X2)

  w <- stats::rbinom(n, size = 1, prob = p)

  eps <- stats::rnorm(n, mean = 0, sd = 0.5)

  y0 <- mu0 + eps
  y1 <- y0
  if (isTRUE(oracle_only)) {
    return(list(df = tibble::tibble(w = w, y0 = y0, y1 = y1)))
  }
  y  <- ifelse(w == 1L, y1, y0)

  true_att <- cs_true_att(structural_te = tau, w = w)
  true_qst <- if (isTRUE(include_truth)) cs_get_oracle_qst("synth_placebo_tau0", version = "1.3.0") else NULL

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
      dgp_id        = "synth_placebo_tau0",
      version       = "1.3.0",
      type          = "synthetic",
      params        = list(n = n, seed = seed),
      structural_te = tau
    )
  )

  cs_check_dgp_synthetic(out)
  out
}

#' @export
dgp_synth_placebo_tau0 <- function(n, seed = NULL) {
  dgp_synth_placebo_tau0_v130(n = n, seed = seed)
}
