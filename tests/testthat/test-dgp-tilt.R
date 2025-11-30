test_that("synth_tilt_mild satisfies synthetic DGP contract", {
  dgp <- dgp_synth_tilt_mild(n = 500, seed = 123)
  expect_invisible(cs_check_dgp_synthetic(dgp))
})

test_that("synth_tilt_mild is reproducible for same seed", {
  d1 <- dgp_synth_tilt_mild(n = 400, seed = 42)
  d2 <- dgp_synth_tilt_mild(n = 400, seed = 42)

  expect_identical(d1$df, d2$df)
  expect_identical(d1$true_att, d2$true_att)
  expect_identical(d1$true_qst, d2$true_qst)
  expect_identical(d1$meta$structural_te, d2$meta$structural_te)
})

test_that("synth_tilt_mild propensity is tilted (non-baseline distribution)", {
  dgp <- dgp_synth_tilt_mild(n = 5000, seed = 1)
  p   <- dgp$df$p
  expect_gt(sd(p), 0.1)
  expect_gt(mean(p < 0.3), 0.08)
  expect_gt(mean(p > 0.7), 0.07)
})

test_that("synth_tilt_mild is registered", {
  reg <- cs_dgp_registry()
  row <- reg[reg$dgp_id == "synth_tilt_mild", , drop = FALSE]
  expect_equal(nrow(row), 1L)
  expect_equal(row$type, "synthetic")
  expect_true(nchar(row$description) > 0)
})
