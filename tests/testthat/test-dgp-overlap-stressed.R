test_that("synth_overlap_stressed satisfies synthetic DGP contract", {
  dgp <- dgp_synth_overlap_stressed(n = 500, seed = 123)
  expect_invisible(cs_check_dgp_synthetic(dgp))
})

test_that("synth_overlap_stressed is reproducible for same seed", {
  d1 <- dgp_synth_overlap_stressed(n = 500, seed = 42)
  d2 <- dgp_synth_overlap_stressed(n = 500, seed = 42)

  expect_identical(d1$df, d2$df)
  expect_identical(d1$true_att, d2$true_att)
  expect_identical(d1$true_qst, d2$true_qst)
  expect_identical(d1$meta$structural_te, d2$meta$structural_te)
})

test_that("synth_overlap_stressed produces extreme propensities", {
  dgp <- dgp_synth_overlap_stressed(n = 5000, seed = 1)
  p   <- dgp$df$p

  share_low  <- mean(p < 0.05)
  share_high <- mean(p > 0.95)

  expect_gt(share_low,  0.05)
  expect_gt(share_high, 0.05)
})

test_that("synth_overlap_stressed is registered", {
  reg <- cs_dgp_registry()
  row <- reg[reg$dgp_id == "synth_overlap_stressed", , drop = FALSE]
  expect_equal(nrow(row), 1L)
  expect_equal(row$type, "synthetic")
  expect_true(nchar(row$description) > 0)
})
