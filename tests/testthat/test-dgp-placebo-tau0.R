test_that("dgp_synth_placebo_tau0 returns a valid sharp-null DGP", {
  dgp <- dgp_synth_placebo_tau0(n = 500, seed = 123)

  expect_invisible(cs_check_dgp_synthetic(dgp))
  expect_identical(dgp$df$y0, dgp$df$y1)
  expect_true(all(dgp$df$structural_te == 0))
  expect_true(all(dgp$meta$structural_te == 0))
  expect_equal(dgp$true_att, 0, tolerance = 1e-10)
  expect_equal(dgp$true_qst$tau, cs_tau_oracle)
  expect_true(all(dgp$true_qst$value == 0))
})

test_that("dgp_synth_placebo_tau0 is reproducible with fixed seed", {
  dgp1 <- dgp_synth_placebo_tau0(n = 200, seed = 42)
  dgp2 <- dgp_synth_placebo_tau0(n = 200, seed = 42)

  expect_identical(dgp1$df, dgp2$df)
  expect_identical(dgp1$true_att, dgp2$true_att)
  expect_identical(dgp1$true_qst, dgp2$true_qst)
  expect_identical(dgp1$meta$structural_te, dgp2$meta$structural_te)
})
