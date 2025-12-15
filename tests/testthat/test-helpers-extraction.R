test_that("cs_extract_estimator_result handles list att and value qst", {
  res <- list(
    att = list(estimate = 1.23),
    qst = tibble::tibble(tau = c(0.1, 0.2), value = c(1, 2))
  )
  extracted <- cs_extract_estimator_result(res)
  expect_type(extracted$att, "double")
  expect_equal(extracted$att, 1.23)
  expect_s3_class(extracted$qst, "tbl_df")
  expect_true("estimate" %in% names(extracted$qst))
})

test_that("cs_extract_estimator_result handles tibble att and estimate qst", {
  res <- list(
    att = tibble::tibble(estimate = 5),
    qst = tibble::tibble(tau = c(0.3, 0.4), estimate = c(3, 4))
  )
  extracted <- cs_extract_estimator_result(res)
  expect_equal(extracted$att, 5)
  expect_equal(extracted$qst$estimate, c(3, 4))
})

test_that("cs_extract_estimator_result is robust to missing components", {
  res <- list(att = NULL, qst = NULL)
  extracted <- cs_extract_estimator_result(res)
  expect_true(is.na(extracted$att))
  expect_null(extracted$qst)
})
