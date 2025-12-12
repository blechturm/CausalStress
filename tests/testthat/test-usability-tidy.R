test_that("cs_tidy preserves qst list column", {
  fake_run <- list(
    att = list(estimate = 1),
    qst = tibble::tibble(tau = c(0.1, 0.9), estimate = c(-1, 1)),
    meta = list(dgp_id = "d", estimator_id = "gengc", n = 50L, seed = 123L)
  )

  tidy <- cs_tidy(fake_run)
  expect_s3_class(tidy, "tbl_df")
  expect_true("qst" %in% names(tidy))
  expect_true(is.list(tidy$qst))
})

test_that("cs_collect_qst works on raw run objects", {
  raw_run <- list(
    att = list(estimate = 0.5),
    qst = tibble::tibble(tau = c(0.25, 0.75), estimate = c(0, 1)),
    meta = list(dgp_id = "d", estimator_id = "gengc", n = 20L, seed = 99L)
  )

  out <- cs_collect_qst(raw_run)
  expect_s3_class(out, "tbl_df")
  expect_true(all(c("tau", "estimate") %in% names(out)))
})

test_that("cs_tidy handles lists of runs (campaigns)", {
  run1 <- list(att = list(estimate = 1), meta = list(dgp_id = "d", estimator_id = "e1", n = 10L, seed = 1L))
  run2 <- list(att = list(estimate = 2), meta = list(dgp_id = "d", estimator_id = "e2", n = 10L, seed = 2L))

  tidy <- cs_tidy(list(run1, run2))
  expect_s3_class(tidy, "tbl_df")
  expect_equal(nrow(tidy), 2)
  expect_true(all(c("dgp_id", "estimator_id") %in% names(tidy)))
})
