# Tests for truth helper functions (cs_true_att, cs_true_qst, cs_tau_oracle).
#
# These tests enforce the CausalStress Constitution:
# - Synthetic DGPs must define oracle truth on the canonical tau grid
#   (0.01–0.99 in steps of 0.01).
# - ATT and QST truth must be computed from potential outcomes (y0, y1)
#   and the treated group (W = 1), not from observed data alone.
#
# Relevant specs:
# - CAUSAL_STRESS_CONSTITUTION.md: Synthetic data & truth semantics
# - CAUSALSTRESS_DESIGN_v0.3.0.md: Truth helpers (ATT, QST) contract


test_that("cs_tau_oracle is defined correctly", {
  expect_true(is.numeric(cs_tau_oracle))
  expect_length(cs_tau_oracle, 99)
  expect_equal(cs_tau_oracle[1], 0.01)
  expect_equal(cs_tau_oracle[99], 0.99)
})

test_that("cs_set_rng produces deterministic RNG state", {
  cs_set_rng(123)
  x1 <- rnorm(5)

  cs_set_rng(123)
  x2 <- rnorm(5)

  expect_equal(x1, x2)
})

test_that("cs_true_att computes ATT over treated units", {
  structural_te <- c(1, 2, 3, 4)
  w <- c(1, 0, 1, 0)
  expected <- mean(c(1, 3))
  result <- cs_true_att(structural_te, w)
  expect_equal(result, expected)
})

test_that("cs_true_qst computes correct QST values for simple case", {
  y0 <- c(1, 2, 3, 4)
  y1 <- c(2, 3, 4, 5)
  w  <- c(1, 0, 1, 0)

  res <- cs_true_qst(y0, y1, w, tau = 0.5)

  expect_true(is.data.frame(res) || tibble::is_tibble(res))
  expect_named(res, c("tau", "value"))
  expect_equal(res$tau, 0.5)

  expected_q1 <- median(c(2, 4))
  expected_q0 <- median(c(1, 3))
  expected_value <- expected_q1 - expected_q0

  expect_equal(res$value, expected_value)
})
