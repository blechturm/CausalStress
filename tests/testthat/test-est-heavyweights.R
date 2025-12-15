if (!requireNamespace("bartCause", quietly = TRUE)) testthat::skip("bartCause not installed")
if (!requireNamespace("tmle", quietly = TRUE)) testthat::skip("tmle not installed")

test_that("bart_att returns ATT with CI", {
  dgp <- dgp_synth_baseline(n = 50, seed = 123)
  df <- cs_airlock(dgp$df, oracle_allowed = FALSE)

  res <- est_bart_att(df = df, config = list(seed = 1))
  expect_type(res, "list")
  expect_true(is.numeric(res$att$estimate))
  expect_true(is.numeric(res$att$ci_lo))
  expect_true(is.numeric(res$att$ci_hi))
  expect_null(res$qst)
  expect_identical(res$meta$estimator_id, "bart_att")
})

test_that("tmle_att returns ATT-like output structure", {
  dgp <- dgp_synth_baseline(n = 50, seed = 321)
  df <- cs_airlock(dgp$df, oracle_allowed = FALSE)

  res <- est_tmle_att(df = df, config = list())
  expect_type(res, "list")
  expect_true(is.numeric(res$att$estimate))
  expect_true(is.numeric(res$att$ci_lo))
  expect_true(is.numeric(res$att$ci_hi))
  expect_null(res$qst)
  expect_identical(res$meta$estimator_id, "tmle_att")
  expect_true(!is.null(res$meta$warnings))
})
