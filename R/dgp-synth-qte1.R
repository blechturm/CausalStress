#' Sign-flip QTE synthetic DGP (v1.3.0)
#'
#' Generates a synthetic DGP with treatment effect heterogeneity driven by a
#' sign flip on X1: tau(X) = +1 if X1 > 0, else -1. Noise is Student-t with
#' df = 4 scaled to sigma = 0.5 to induce heavier tails and heterogeneity in
#' the QST curve.
#'
#' @details
#' \strong{Goal:} Verify detection of distributional heterogeneity.
#'
#' \strong{Structure:} Same \eqn{X}, \eqn{\mu_0}, and \eqn{p} as \code{synth_baseline}.
#'
#' \strong{Mechanism:} \eqn{\tau(X)} flips sign based on \eqn{X_1}:
#' \itemize{
#'   \item \eqn{\tau(X) = +1.0} if \eqn{X_1 > 0}.
#'   \item \eqn{\tau(X) = -1.0} if \eqn{X_1 \le 0}.
#' }
#'
#' \strong{Noise:} Student-t with \eqn{df = 4}, scaled to \eqn{\sigma = 0.5}.
#'
#' @param n Integer, number of observations.
#' @param seed Optional seed for reproducibility (passed to `cs_set_rng()`).
#'
#' @return A list with components `df`, `true_att`, `true_qst`, and `meta`
#'   following the synthetic DGP contract.
#' @export
dgp_synth_qte1_v130 <- function(n, seed = NULL, include_truth = TRUE, oracle_only = FALSE) {
  if (!is.null(seed)) {
    cs_set_rng(seed)
  }

  X1 <- stats::rnorm(n, mean = 0, sd = 1)
  X2 <- stats::rnorm(n, mean = 0, sd = 1)
  if (!isTRUE(oracle_only)) {
    X3 <- stats::rnorm(n, mean = 0, sd = 1)
    X4 <- stats::rnorm(n, mean = 0, sd = 1)
    X5 <- stats::rnorm(n, mean = 0, sd = 1)
  }

  mu0 <- 1 + X1 + 0.5 * X2
  tau <- ifelse(X1 > 0, 1, -1)
  p   <- stats::plogis(0.5 * X1 - 0.5 * X2)

  w <- stats::rbinom(n, size = 1L, prob = p)

  eps0 <- 0.5 * stats::rt(n, df = 4)
  eps1 <- 0.5 * stats::rt(n, df = 4)

  y0 <- mu0 + eps0
  y1 <- mu0 + tau + eps1

  if (isTRUE(oracle_only)) {
    return(list(df = tibble::tibble(w = w, y0 = y0, y1 = y1)))
  }
  y  <- ifelse(w == 1L, y1, y0)

  true_att <- cs_true_att(structural_te = tau, w = w)
  true_qst <- if (isTRUE(include_truth)) cs_get_oracle_qst("synth_qte1", version = "1.3.0") else NULL

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
      dgp_id        = "synth_qte1",
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
dgp_synth_qte1 <- function(n, seed = NULL) {
  dgp_synth_qte1_v130(n = n, seed = seed)
}
