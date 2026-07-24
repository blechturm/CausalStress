# Tests for cs_check_dgp_synthetic() contract helper.
#
# These tests enforce that synthetic DGPs satisfy the basic registry contract:
# - DGPs must return a list(df, true_att, true_qst, meta).
# - df must contain {y, w, y0, y1, p} without NA and canonical X1...Xk
#   covariates.
# - w must be binary {0, 1}.
# - true_att must be a finite numeric scalar.
# - true_qst must be a tibble(tau, value) on (a subset of) cs_tau_oracle.
# - meta$type must be "synthetic".
# - meta$structural_te must be a numeric vector of length nrow(df),
#   and must match df$structural_te when that column is present.

test_that("cs_check_dgp_synthetic accepts a valid synthetic DGP", {
  n <- 5L

  df <- tibble::tibble(
    y             = c(1, 2, 3, 4, 5),
    w             = c(0, 1, 0, 1, 0),
    y0            = c(1, 2, 3, 4, 5),
    y1            = c(2, 3, 4, 5, 6),
    p             = c(0.2, 0.5, 0.3, 0.7, 0.4),
    structural_te = c(1, 1, 1, 1, 1),
    X1            = c(-1, 0, 1, 2, 3)
  )

  true_qst <- tibble::tibble(
    tau   = cs_tau_oracle,
    value = rep(0.5, length(cs_tau_oracle))
  )

  dgp <- list(
    df       = df,
    true_att = 1.0,
    true_qst = true_qst,
    meta     = list(
      dgp_id        = "synth_dummy",
      type          = "synthetic",
      structural_te = df$structural_te
    )
  )

  expect_invisible(cs_check_dgp_synthetic(dgp))
})

test_that("cs_check_dgp_synthetic fails if core columns are missing", {
  df <- tibble::tibble(
    y = c(1, 2, 3),
    w = c(0, 1, 0)
    # y0, y1, p are missing
  )

  true_qst <- tibble::tibble(
    tau   = cs_tau_oracle,
    value = rep(0, length(cs_tau_oracle))
  )

  dgp <- list(
    df       = df,
    true_att = 0.5,
    true_qst = true_qst,
    meta     = list(
      dgp_id        = "synth_dummy",
      type          = "synthetic",
      structural_te = rep(1, nrow(df))
    )
  )

  expect_error(
    cs_check_dgp_synthetic(dgp),
    class = "causalstress_dgp_error"
  )
})

test_that("cs_check_dgp_synthetic fails if w is not binary", {
  df <- tibble::tibble(
    y             = c(1, 2, 3),
    w             = c(0, 2, 1),  # invalid: contains 2
    y0            = c(1, 2, 3),
    y1            = c(2, 3, 4),
    p             = c(0.3, 0.4, 0.5),
    structural_te = c(1, 1, 1),
    X1            = c(-1, 0, 1)
  )

  true_qst <- tibble::tibble(
    tau   = cs_tau_oracle,
    value = rep(0, length(cs_tau_oracle))
  )

  dgp <- list(
    df       = df,
    true_att = 0.5,
    true_qst = true_qst,
    meta     = list(
      dgp_id        = "synth_dummy",
      type          = "synthetic",
      structural_te = df$structural_te
    )
  )

  expect_error(
    cs_check_dgp_synthetic(dgp),
    class = "causalstress_dgp_error"
  )
})

test_that("synthetic covariates must be uppercase, consecutive, and one-based", {
  make_dgp <- function(covariate_names) {
    n <- 5L
    df <- tibble::tibble(
      y = seq_len(n),
      w = c(0, 1, 0, 1, 0),
      y0 = seq_len(n),
      y1 = seq_len(n) + 1,
      p = rep(0.5, n),
      structural_te = rep(1, n)
    )
    for (name in covariate_names) {
      df[[name]] <- seq_len(n)
    }
    list(
      df = df,
      true_att = 1,
      true_qst = tibble::tibble(
        tau = cs_tau_oracle,
        value = rep(1, length(cs_tau_oracle))
      ),
      meta = list(
        dgp_id = "synth_covariate_probe",
        type = "synthetic",
        structural_te = df$structural_te
      )
    )
  }

  valid <- make_dgp(c("X1", "X2"))
  expect_invisible(CausalStress:::cs_check_dgp_covariates(valid$df))
  expect_invisible(cs_check_dgp_synthetic(valid))

  invalid_names <- list(
    lowercase = "x1",
    gap = c("X1", "X3"),
    zero_based = "X0",
    zero_padded = "X01",
    bare_x = "X",
    non_numeric = "Xfoo",
    absent = character(0)
  )

  for (case in names(invalid_names)) {
    expect_error(
      cs_check_dgp_synthetic(make_dgp(invalid_names[[case]])),
      class = "causalstress_dgp_error",
      info = case
    )
  }
})
