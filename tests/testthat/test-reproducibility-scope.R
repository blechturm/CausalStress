test_that("BLAS-sensitive DGP is bitwise identical on the same substrate", {
  out1 <- dgp_synth_hd_sparse_plm_v150(n = 40L, seed = 20260613L, include_truth = FALSE)
  out2 <- dgp_synth_hd_sparse_plm_v150(n = 40L, seed = 20260613L, include_truth = FALSE)

  expect_identical(out1$df, out2$df)
  expect_identical(out1$true_att, out2$true_att)
  expect_identical(out1$true_qst, out2$true_qst)
  # Current DGP meta is deterministic; if session/timestamp fields are added,
  # compare the science fields explicitly instead of dropping this check.
  expect_identical(out1$meta, out2$meta)
})
