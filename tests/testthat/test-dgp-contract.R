# Tests for cs_check_dgp_synthetic() contract helper.
#
# These tests enforce that all synthetic DGPs satisfy the Constitution:
# - DGPs must return a list (df, true_att, true_qst, meta).
# - df must contain {y, w, y0, y1, structural_te} without NA.
# - w must be binary {0, 1}.
# - true_att must be a finite numeric scalar.
# - true_qst must be a tibble(tau, value) on the oracle tau grid.
# - meta$type must be "synthetic".
#
# Relevant specs:
# - CAUSAL_STRESS_CONSTITUTION.md: Synthetic DGP and potential outcome rules
# - CAUSAL_STRESS_DGP_REGISTRY_1.3.0.md: DGP contract
# - CAUSALSTRESS_DESIGN_v0.3.0.md: DGP output contract & runner assumptions


test_that("cs_check_dgp_synthetic accepts a valid synthetic DGP", {
  df <- tibble::tibble(
    y             = c(1, 2, 3),
    w             = c(0, 1, 0),
    y0            = c(1, 2, 3),
    y1            = c(1, 3, 3),
    structural_te = c(0, 1, 0)
  )

  true_qst <- tibble::tibble(
    tau   = cs_tau_oracle,
    value = rep(0.5, length(cs_tau_oracle))
  )

  dgp <- list(
    df       = df,
    true_att = 0.5,
    true_qst = true_qst,
    meta     = list(
      dgp_id = "synth_dummy",
      type   = "synthetic"
    )
  )

  expect_invisible(cs_check_dgp_synthetic(dgp))
})

test_that("cs_check_dgp_synthetic fails if core columns are missing", {
  df <- tibble::tibble(
    y = c(1, 2, 3),
    w = c(0, 1, 0)
    # y0, y1, structural_te are missing
  )

  dgp <- list(
    df       = df,
    true_att = 0.5,
    true_qst = tibble::tibble(tau = cs_tau_oracle, value = rep(0, length(cs_tau_oracle))),
    meta     = list(dgp_id = "synth_dummy", type = "synthetic")
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
    y1            = c(1, 3, 3),
    structural_te = c(0, 1, 0)
  )

  dgp <- list(
    df       = df,
    true_att = 0.5,
    true_qst = tibble::tibble(tau = cs_tau_oracle, value = rep(0, length(cs_tau_oracle))),
    meta     = list(dgp_id = "synth_dummy", type = "synthetic")
  )

  expect_error(
    cs_check_dgp_synthetic(dgp),
    class = "causalstress_dgp_error"
  )
})
