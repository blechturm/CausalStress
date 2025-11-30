#' High-dimensional sparse partially linear DGP
#'
#' Implements the `synth_hd_sparse_plm` design from the DGP registry 1.4.0:
#' correlated Gaussian covariates (p = 50) with sparse linear outcome and
#' propensity, constant ATT tau = 1.
#'
#' @param n Integer, number of observations.
#' @param seed Optional seed for reproducibility (passed to `cs_set_rng()`).
#'
#' @return A synthetic DGP list with df, true_att, true_qst, and meta.
#' @export
dgp_synth_hd_sparse_plm <- function(n, seed = NULL) {
  if (!is.null(seed)) {
    cs_set_rng(seed)
  }

  p_hd <- 50L
  idx <- seq_len(p_hd)
  Sigma <- outer(idx, idx, function(i, j) 0.5 ^ abs(i - j))
  L <- chol(Sigma)
  Z <- matrix(stats::rnorm(n * p_hd), nrow = n)
  X <- Z %*% L

  colnames(X) <- paste0("X", seq_len(p_hd))

  beta_y <- c(rep(1, 5), rep(0, p_hd - 5))
  mu0    <- as.numeric(X %*% beta_y)
  eps    <- stats::rnorm(n, mean = 0, sd = 1)
  y0     <- mu0 + eps

  tau <- rep(1, n)
  y1  <- y0 + tau

  gamma <- c(0.5, -0.5, 0.25, -0.25, 0.1, rep(0, p_hd - 5))
  lin_ps <- as.numeric(X %*% gamma)
  p      <- stats::plogis(lin_ps)
  w      <- stats::rbinom(n, size = 1L, prob = p)

  y <- ifelse(w == 1L, y1, y0)

  true_att <- cs_true_att(structural_te = tau, w = w)
  true_qst <- cs_get_oracle_qst("synth_hd_sparse_plm")

  df <- tibble::tibble(
    y  = y,
    w  = w,
    y0 = y0,
    y1 = y1,
    p  = p,
    structural_te = tau
  )
  df <- dplyr::bind_cols(df, tibble::as_tibble(X))

  out <- list(
    df = df,
    true_att = true_att,
    true_qst = true_qst,
    meta = list(
      dgp_id        = "synth_hd_sparse_plm",
      type          = "synthetic",
      structural_te = tau
    )
  )

  cs_check_dgp_synthetic(out)
  out
}
