test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

test_that("dgp_synth_baseline is reproducible given a seed", {
  n <- 150L
  seed <- 999

  dgp1 <- dgp_synth_baseline(n = n, seed = seed)
  dgp2 <- dgp_synth_baseline(n = n, seed = seed)

  # Data frame and truth must be identical
  expect_equal(dgp1$df, dgp2$df)
  expect_equal(dgp1$true_att, dgp2$true_att)
  expect_equal(dgp1$true_qst, dgp2$true_qst)
  expect_equal(dgp1$meta$structural_te, dgp2$meta$structural_te)
})
