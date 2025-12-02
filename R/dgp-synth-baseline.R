#' Baseline synthetic DGP for CausalStress (v1.3.0)
#'
#' Generates the baseline synthetic DGP as defined in the DGP Registry.
#'
#' @param n Integer, number of observations.
#' @param seed Optional seed for reproducibility (passed to `cs_set_rng()`).
#'
#' @return A list with:
#'   - df: tibble with columns `y`, `w`, `y0`, `y1`, `p`, `structural_te`, `X1`, `X2`, `X3`, `X4`, `X5`
#'   - true_att: numeric scalar
#'   - true_qst: tibble with columns `tau` and `value`
#'   - meta: list with fields `dgp_id = "synth_baseline"`, `type = "synthetic"`, `structural_te = tau`
#' @export
dgp_synth_baseline_v130 <- function(n, seed = NULL) {
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

  eps0 <- stats::rnorm(n, mean = 0, sd = 0.5)
  eps1 <- stats::rnorm(n, mean = 0, sd = 0.5)

  y0 <- mu0 + eps0
  y1 <- mu0 + tau + eps1
  y <- ifelse(w == 1, y1, y0)

  true_att <- cs_true_att(structural_te = tau, w = w)
  true_qst <- cs_get_oracle_qst("synth_baseline")

  list(
    df = tibble::tibble(
      y = y,
      w = w,
      y0 = y0,
      y1 = y1,
      p = p,
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
      dgp_id = "synth_baseline",
      type = "synthetic",
      structural_te = tau
    )
  )
}

#' @export
dgp_synth_baseline <- function(n, seed = NULL) {
  dgp_synth_baseline_v130(n = n, seed = seed)
}
