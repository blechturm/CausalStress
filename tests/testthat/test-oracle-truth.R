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
  dgp_id <- "synth_baseline"
  version <- "1.3.0"
  cache_dir <- tempfile("cs_oracle_cache_")
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  cache_file <- file.path(cache_dir, paste0("truth_", dgp_id, "_", version, ".qs"))
  if (file.exists(cache_file)) unlink(cache_file)

  res1 <- cs_get_oracle_qst(dgp_id, version = version, cache_dir = cache_dir)
  expect_true(file.exists(cache_file))
  mtime1 <- file.info(cache_file)$mtime

  res2 <- cs_get_oracle_qst(dgp_id, version = version, cache_dir = cache_dir)
  mtime2 <- file.info(cache_file)$mtime

  expect_identical(res1, res2)
  expect_equal(mtime1, mtime2)
})

test_that("oracle call does not disturb global RNG state", {
  set.seed(42)
  before <- .Random.seed
  cs_get_oracle_qst("synth_baseline")
  after <- .Random.seed
  expect_identical(before, after)
})
