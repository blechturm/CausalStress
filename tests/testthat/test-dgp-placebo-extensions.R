test_that("synth_placebo_nonlinear satisfies synthetic DGP contract and sharp null", {
  dgp <- dgp_synth_placebo_nonlinear(n = 500, seed = 111)
  expect_invisible(cs_check_dgp_synthetic(dgp))
  expect_true(all(dgp$df$structural_te == 0))
  expect_true(all(dgp$meta$structural_te == 0))
  expect_identical(dgp$df$y0, dgp$df$y1)
  expect_equal(dgp$true_att, 0)
  expect_true(all(dgp$true_qst$value == 0))
  expect_equal(dgp$true_qst$tau, cs_tau_oracle)
})

test_that("synth_placebo_heavytail satisfies synthetic DGP contract and sharp null", {
  dgp <- dgp_synth_placebo_heavytail(n = 500, seed = 222)
  expect_invisible(cs_check_dgp_synthetic(dgp))
  expect_true(all(dgp$df$structural_te == 0))
  expect_true(all(dgp$meta$structural_te == 0))
  expect_identical(dgp$df$y0, dgp$df$y1)
  expect_equal(dgp$true_att, 0)
  expect_true(all(dgp$true_qst$value == 0))
  expect_equal(dgp$true_qst$tau, cs_tau_oracle)
})

test_that("synth_placebo_tilted satisfies synthetic DGP contract and sharp null", {
  dgp <- dgp_synth_placebo_tilted(n = 500, seed = 333)
  expect_invisible(cs_check_dgp_synthetic(dgp))
  expect_true(all(dgp$df$structural_te == 0))
  expect_true(all(dgp$meta$structural_te == 0))
  expect_identical(dgp$df$y0, dgp$df$y1)
  expect_equal(dgp$true_att, 0)
  expect_true(all(dgp$true_qst$value == 0))
  expect_equal(dgp$true_qst$tau, cs_tau_oracle)
})

test_that("placebo DGPs are reproducible", {
  d1 <- dgp_synth_placebo_nonlinear(n = 300, seed = 99)
  d2 <- dgp_synth_placebo_nonlinear(n = 300, seed = 99)
  expect_identical(d1$df, d2$df)
  expect_identical(d1$true_att, d2$true_att)
  expect_identical(d1$true_qst, d2$true_qst)

  h1 <- dgp_synth_placebo_heavytail(n = 300, seed = 98)
  h2 <- dgp_synth_placebo_heavytail(n = 300, seed = 98)
  expect_identical(h1$df, h2$df)
  expect_identical(h1$true_att, h2$true_att)
  expect_identical(h1$true_qst, h2$true_qst)

  t1 <- dgp_synth_placebo_tilted(n = 300, seed = 97)
  t2 <- dgp_synth_placebo_tilted(n = 300, seed = 97)
  expect_identical(t1$df, t2$df)
  expect_identical(t1$true_att, t2$true_att)
  expect_identical(t1$true_qst, t2$true_qst)
})

test_that("placebo DGPs are registered", {
  reg <- cs_dgp_registry()
  ids <- c(
    "synth_placebo_nonlinear",
    "synth_placebo_heavytail",
    "synth_placebo_tilted"
  )
  expect_true(all(ids %in% reg$dgp_id))
})
