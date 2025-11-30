test_that("synth_placebo_kangschafer satisfies synthetic DGP contract and sharp null", {
  out <- dgp_synth_placebo_kangschafer(n = 200, seed = 123)
  df  <- out$df

  expect_invisible(cs_check_dgp_synthetic(out))
  expect_true(is.data.frame(df))
  expect_equal(nrow(df), 200)
  expect_true(all(df$w %in% c(0, 1)))
  expect_true(all(df$structural_te == 0))
  expect_identical(df$y0, df$y1)
  expect_equal(out$true_att, 0, tolerance = 1e-8)
  expect_true(all(out$true_qst$value == 0))
  expect_equal(out$true_qst$tau, cs_tau_oracle)
  expect_equal(out$meta$dgp_id, "synth_placebo_kangschafer")
  expect_equal(out$meta$type, "synthetic")
  expect_length(out$meta$structural_te, nrow(df))
})

test_that("synth_placebo_kangschafer is reproducible for same seed", {
  d1 <- dgp_synth_placebo_kangschafer(n = 150, seed = 99)
  d2 <- dgp_synth_placebo_kangschafer(n = 150, seed = 99)

  expect_identical(d1$df, d2$df)
  expect_identical(d1$true_att, d2$true_att)
  expect_identical(d1$true_qst, d2$true_qst)
  expect_identical(d1$meta$structural_te, d2$meta$structural_te)
})

test_that("synth_placebo_kangschafer is registered", {
  reg <- cs_dgp_registry()
  row <- reg[reg$dgp_id == "synth_placebo_kangschafer", , drop = FALSE]
  expect_equal(nrow(row), 1L)
  expect_equal(row$type, "synthetic")
  expect_true(nchar(row$description) > 0)
  expect_true(row$version %in% c("1.3.0", "1.4.0"))
})
