#' Nonlinear heteroskedastic synthetic DGP for CausalStress (v1.3.0)
#'
#' Implements the `synth_nonlinear_heteroskedastic` design from the
#' DGP registry. This DGP has nonlinear baseline outcome, constant
#' treatment effect, and heteroskedastic Gaussian noise.
#'
#' @details
#' \strong{Goal:} Test curve fitting and variance adaptation.
#'
#' \strong{Covariates:} \eqn{X \in \mathbb{R}^4}.
#' \itemize{
#'   \item \eqn{X_1, X_2 \sim \mathcal{N}(0,1)}.
#'   \item \eqn{X_3 \sim \mathcal{U}[-2, 2]}.
#'   \item \eqn{X_4 \sim \mathrm{Bernoulli}(0.4)}.
#' }
#'
#' \strong{Outcome:} \eqn{Y_0 = 1 + 0.8 \sin(X_1) + 0.5 X_2^2 - 0.3 X_4}.
#'
#' \strong{Treatment effect:} \eqn{\tau(X) = 1.0} (constant).
#'
#' \strong{Noise:} Gaussian with heteroskedastic scale \eqn{\varepsilon \sim \mathcal{N}(0, \sigma(X)^2)} where \eqn{\sigma(X) = 0.3 + 0.2 |X_3|}.
#'
#' \strong{Propensity:} \eqn{p(X) = \mathrm{plogis}(0.5 X_1 - 0.5 X_2)}.
#'
#' @param n Integer, number of observations.
#' @param seed Optional seed for reproducibility (passed to `cs_set_rng()`).
#'
#' @return A list with `df`, `true_att`, `true_qst`, and `meta` satisfying
#'   the synthetic DGP contract.
#' @export
dgp_synth_nonlinear_heteroskedastic_v130 <- function(n, seed = NULL, include_truth = TRUE, oracle_only = FALSE) {
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

  if (isTRUE(oracle_only)) {
    return(list(df = tibble::tibble(w = w, y0 = y0, y1 = y1)))
  }
  y  <- ifelse(w == 1L, y1, y0)

  true_att <- cs_true_att(structural_te = tau, w = w)
  true_qst <- if (isTRUE(include_truth)) cs_get_oracle_qst("synth_nonlinear_heteroskedastic", version = "1.3.0") else NULL

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
dgp_synth_nonlinear_heteroskedastic <- function(n, seed = NULL) {
  dgp_synth_nonlinear_heteroskedastic_v130(n = n, seed = seed)
}
