test_that("cs_get_oracle_qst returns qst for synth_baseline", {
  res <- cs_get_oracle_qst("synth_baseline", version = "1.6.0")
  expect_s3_class(res, "tbl_df")
  expect_equal(nrow(res), length(cs_tau_oracle))
})

test_that("placebo oracle returns zeros", {
  res <- cs_get_oracle_qst("synth_placebo_tau0")
  expect_true(all(res$value == 0))
})

test_that("oracle cache returns identical results on repeat", {
  dgp_id <- "synth_baseline"
  version <- "1.6.0"
  cache_dir <- tempfile("cs_oracle_cache_")
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  desc <- CausalStress:::cs_oracle_algorithm_descriptor(dgp_id, version, cs_tau_oracle, CausalStress:::ORACLE_N)
  fp <- CausalStress:::cs_oracle_algorithm_fingerprint(desc)
  cache_file <- CausalStress:::cs_oracle_cache_file(cache_dir, dgp_id, version, fp)
  if (file.exists(cache_file)) unlink(cache_file)

  res1 <- cs_get_oracle_qst(dgp_id, version = version, cache_dir = cache_dir)
  expect_true(file.exists(cache_file))
  mtime1 <- file.info(cache_file)$mtime

  res2 <- cs_get_oracle_qst(dgp_id, version = version, cache_dir = cache_dir)
  mtime2 <- file.info(cache_file)$mtime

  expect_identical(res1, res2)
  expect_equal(mtime1, mtime2)
})

test_that("oracle cache fails closed on stale RDS metadata", {
  dgp_id <- "synth_placebo_tau0"
  version <- "1.3.0"
  cache_dir <- tempfile("cs_oracle_cache_")
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  desc <- CausalStress:::cs_oracle_algorithm_descriptor(dgp_id, version, cs_tau_oracle, CausalStress:::ORACLE_N)
  fp <- CausalStress:::cs_oracle_algorithm_fingerprint(desc)
  cache_file <- CausalStress:::cs_oracle_cache_file(cache_dir, dgp_id, version, fp)

  stale <- list(
    oracle_algorithm_fingerprint = "stale",
    oracle_algorithm_descriptor = list(stale = TRUE),
    truth = tibble::tibble(tau_id = "bad", tau = 0.5, value = 999)
  )
  saveRDS(stale, cache_file, version = 3)

  expect_error(
    cs_get_oracle_qst(dgp_id, version = version, cache_dir = cache_dir),
    "invalid structure or identity",
    class = "causalstress_oracle_error"
  )
  expect_identical(readRDS(cache_file), stale)
})

test_that("legacy oracle qs cache is untouched and recomputed as RDS", {
  dgp_id <- "synth_placebo_tau0"
  version <- "1.3.0"
  cache_dir <- tempfile("cs_oracle_cache_")
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  desc <- CausalStress:::cs_oracle_algorithm_descriptor(
    dgp_id,
    version,
    cs_tau_oracle,
    CausalStress:::ORACLE_N
  )
  fp <- CausalStress:::cs_oracle_algorithm_fingerprint(desc)
  cache_file <- CausalStress:::cs_oracle_cache_file(cache_dir, dgp_id, version, fp)
  legacy_file <- sub("\\.rds$", ".qs", cache_file)
  legacy_bytes <- charToRaw("immutable legacy oracle cache")
  writeBin(legacy_bytes, legacy_file)

  res <- cs_get_oracle_qst(dgp_id, version = version, cache_dir = cache_dir)

  expect_true(all(res$value == 0))
  expect_true(file.exists(cache_file))
  expect_identical(readBin(legacy_file, "raw", n = file.info(legacy_file)$size), legacy_bytes)
  payload <- readRDS(cache_file)
  expect_identical(payload$oracle_algorithm_fingerprint, fp)
  expect_identical(payload$oracle_algorithm_descriptor, desc)
})

test_that("oracle call does not disturb global RNG state", {
  set.seed(42)
  before <- .Random.seed
  cs_get_oracle_qst("synth_baseline", version = "1.6.0")
  after <- .Random.seed
  expect_identical(before, after)
})
