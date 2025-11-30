test_that("synth_hd_sparse_plm structure and truth", {
  out <- dgp_synth_hd_sparse_plm(n = 200, seed = 123)
  df  <- out$df

  expect_s3_class(df, "tbl_df")
  expect_equal(nrow(df), 200)

  needed_cols <- c("y", "w", "y0", "y1", "p", "structural_te", paste0("X", 1:50))
  expect_true(all(needed_cols %in% names(df)))
  expect_true(all(df$w %in% c(0, 1)))
  expect_true(all(df$structural_te == 1))
  expect_true(all(abs(df$y1 - (df$y0 + 1)) < 1e-8))
  expect_true(abs(out$true_att - 1) < 1e-6)
  expect_true(all(abs(out$true_qst$value - 1) < 1e-6))
})

test_that("synth_hd_sparse_plm has correlated covariates", {
  out <- dgp_synth_hd_sparse_plm(n = 2000, seed = 1)
  df  <- out$df
  cors <- stats::cor(as.matrix(df[paste0("X", 1:5)]))
  expect_true(all(abs(diag(cors) - 1) < 0.1))
  expect_true(cors[1, 2] > cors[1, 3])
  expect_true(cors[1, 3] > cors[1, 4])
})

test_that("synth_hd_sparse_plm is reproducible for same seed", {
  d1 <- dgp_synth_hd_sparse_plm(n = 150, seed = 42)
  d2 <- dgp_synth_hd_sparse_plm(n = 150, seed = 42)

  expect_identical(d1$df, d2$df)
  expect_identical(d1$true_att, d2$true_att)
  expect_identical(d1$true_qst, d2$true_qst)
  expect_identical(d1$meta$structural_te, d2$meta$structural_te)
})

test_that("estimators behave reasonably on synth_hd_sparse_plm", {
  board <- pins::board_temp()
  runs <- cs_run_grid(
    dgp_ids       = "synth_hd_sparse_plm",
    estimator_ids = c("lm_att", "ipw_att"),
    n             = 300,
    seeds         = 1:3,
    bootstrap     = FALSE,
    board         = board
  )

  sumry <- cs_summarise_runs(runs)
  expect_true(all(abs(sumry$mean_true_att - 1) < 1e-6))
  expect_true(all(abs(sumry$mean_error) < 0.5))
})
