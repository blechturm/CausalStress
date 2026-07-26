test_that("tmle_att helper extracts ATT rather than ATE", {
  fit <- list(
    estimates = list(
      ATE = list(psi = 99, CI = c(98, 100)),
      ATT = list(psi = 2, CI = c(1, 3))
    )
  )

  expect_equal(cs_tmle_att_component(fit, "psi"), 2)
  expect_equal(cs_tmle_att_component(fit, "CI"), c(1, 3))
})

test_that("tmle_att helper fails loudly if ATT is unavailable", {
  fit <- list(estimates = list(ATE = list(psi = 99)))

  expect_error(
    cs_tmle_att_component(fit, "psi"),
    "did not contain an ATT estimate"
  )
})
