test_that("science payload excludes provenance while provenance/meta flatten expose max_runtime", {
  res <- cs_run_single(
    dgp_id       = "synth_baseline",
    estimator_id = "lm_att",
    n            = 30,
    seed         = 1L,
    max_runtime  = Inf
  )

  sci <- cs_science_payload(res)
  expect_true(is.list(sci))
  expect_false("provenance" %in% names(sci))
  expect_false("max_runtime" %in% names(sci$meta))

  prov <- cs_provenance(res)
  expect_true("max_runtime" %in% names(prov))

  flat <- cs_meta_flatten(res)
  expect_s3_class(flat, "tbl_df")
  expect_true("max_runtime" %in% names(flat))
})

