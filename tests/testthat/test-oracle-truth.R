test_that("cs_get_oracle_qst returns qst for synth_baseline", {
  res <- cs_get_oracle_qst("synth_baseline")
  expect_s3_class(res, "tbl_df")
  expect_equal(nrow(res), length(cs_tau_oracle))
})

test_that("placebo oracle returns zeros", {
  res <- cs_get_oracle_qst("synth_placebo_tau0")
  expect_true(all(res$value == 0))
})

test_that("oracle cache returns identical results on repeat", {
  res1 <- cs_get_oracle_qst("synth_baseline")
  res2 <- cs_get_oracle_qst("synth_baseline")
  expect_identical(res1, res2)
})

test_that("oracle call does not disturb global RNG state", {
  set.seed(42)
  before <- .Random.seed
  cs_get_oracle_qst("synth_baseline")
  after <- .Random.seed
  expect_identical(before, after)
})
