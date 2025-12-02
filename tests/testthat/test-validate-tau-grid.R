test_that("cs_validate_tau_grid passes with canonical grid", {
  truth <- tibble::tibble(tau = cs_tau_oracle, value = rep(0, length(cs_tau_oracle)))
  expect_invisible(cs_validate_tau_grid(truth))
})

test_that("cs_validate_tau_grid errors when tau column missing", {
  truth <- tibble::tibble(value = 1:3)
  expect_error(cs_validate_tau_grid(truth), "tau")
})

test_that("cs_validate_tau_grid errors on wrong values or order", {
  truth <- tibble::tibble(tau = rev(cs_tau_oracle), value = rep(0, length(cs_tau_oracle)))
  expect_error(cs_validate_tau_grid(truth), "canonical")

  truth2 <- tibble::tibble(tau = c(cs_tau_oracle, 1.1), value = rep(0, length(cs_tau_oracle) + 1))
  expect_error(cs_validate_tau_grid(truth2), "canonical")
})

test_that("placebo truth with canonical grid passes", {
  truth <- tibble::tibble(tau = cs_tau_oracle, value = rep(0, length(cs_tau_oracle)))
  expect_invisible(cs_validate_tau_grid(truth))
})
