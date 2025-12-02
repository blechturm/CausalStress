test_that("suite registry has expected shape", {
  reg <- CausalStress:::cs_suite_registry()
  expect_s3_class(reg, "tbl_df")
  expect_true(all(c("suite_id", "dgp_ids", "description") %in% names(reg)))
})

test_that("cs_get_suite returns placebo members", {
  placebo <- cs_get_suite("placebo")
  expect_true(is.character(placebo))
  expect_true("synth_placebo_tau0" %in% placebo)
})

test_that("cs_get_suite errors for unknown suite", {
  expect_error(cs_get_suite("does_not_exist"), class = "causalstress_registry_error")
})

test_that("cs_run_suite forwards to grid", {
  # check defaults in formals (no heavy run)
  expect_equal(formals(cs_run_suite)$seeds, quote(1:50))
  expect_equal(formals(cs_run_suite)$n, 2000)

  res <- cs_run_suite(
    suite_id      = "placebo",
    estimator_ids = "lm_att",
    n             = 10,
    seeds         = 1L,
    bootstrap     = FALSE
  )
  expect_s3_class(res, "tbl_df")
  expect_equal(unique(res$n), 10L)
  expect_true(all(res$estimator_id == "lm_att"))
})
