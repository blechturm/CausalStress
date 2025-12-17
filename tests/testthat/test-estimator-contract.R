# Tests for cs_check_estimator_output() contract helper.
#
# These tests enforce that estimator outputs are well-formed:
# - Estimators must return a list(att, qst, meta).
# - att must contain a finite numeric scalar estimate.
# - qst must be NULL (ATT-only) or tibble(tau, value) if provided.
# - meta$estimator_id must be a character scalar.
#
# Relevant specs:
# - CAUSAL_STRESS_CONSTITUTION.md: Estimator neutrality & fairness
# - CAUSALSTRESS_DESIGN_v0.3.0.md: Estimator output contract


test_that("cs_check_estimator_output accepts ATT-only estimator without qst", {
  res <- list(
    att  = list(estimate = 1.23),
    qst  = NULL,
    meta = list(estimator_id = "DummyATT")
  )

  expect_invisible(cs_check_estimator_output(res, require_qst = FALSE))
})

test_that("cs_check_estimator_output accepts QST-capable estimator when qst is present", {
  tau_req <- 0.5
  res <- list(
    att  = data.frame(estimate = 0.5),
    qst  = tibble::tibble(
      tau   = c(tau_req),
      value = c(1.0)
    ),
    meta = list(estimator_id = "DummyQST")
  )

  expect_invisible(
    cs_check_estimator_output(res, require_qst = TRUE, tau = tau_req)
  )
})

test_that("cs_check_estimator_output fails if estimate is missing", {
  res <- list(
    att  = list(),  # no estimate
    qst  = NULL,
    meta = list(estimator_id = "BrokenEstimator")
  )

  expect_error(
    cs_check_estimator_output(res),
    class = "causalstress_contract_error"
  )
})

test_that("cs_check_estimator_output fails if qst is required but missing", {
  res <- list(
    att  = list(estimate = 0.5),
    qst  = NULL,
    meta = list(estimator_id = "BrokenQST")
  )

  expect_error(
    cs_check_estimator_output(res, require_qst = TRUE),
    class = "causalstress_estimator_error"
  )
})
