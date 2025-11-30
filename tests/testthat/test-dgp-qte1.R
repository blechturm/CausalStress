test_that("dgp_synth_qte1 satisfies synthetic DGP contract", {
  dgp <- dgp_synth_qte1(n = 500, seed = 123)
  expect_invisible(cs_check_dgp_synthetic(dgp))
})

test_that("dgp_synth_qte1 has sign-flip structural tau", {
  dgp <- dgp_synth_qte1(n = 800, seed = 321)
  te  <- dgp$df$structural_te
  tab <- table(te)
  expect_true(all(names(tab) %in% c("-1", "1")))
  expect_length(tab, 2L)
  expect_true(all(tab > 0))
})

test_that("dgp_synth_qte1 is reproducible with fixed seed", {
  d1 <- dgp_synth_qte1(n = 300, seed = 42)
  d2 <- dgp_synth_qte1(n = 300, seed = 42)

  expect_identical(d1$df, d2$df)
  expect_identical(d1$true_att, d2$true_att)
  expect_identical(d1$true_qst, d2$true_qst)
  expect_identical(d1$meta$structural_te, d2$meta$structural_te)
})

test_that("dgp_synth_qte1 QST is not flat", {
  dgp <- dgp_synth_qte1(n = 800, seed = 777)
  v <- dgp$true_qst$value
  expect_true(sd(v) > 0)
})
