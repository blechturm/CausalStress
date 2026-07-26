#!/usr/bin/env Rscript

pkgload::load_all(".", quiet = TRUE)

strict <- cs_validate_dgp_registry(strict = TRUE)
validated <- cs_validate_registry()

cat("registry_strict_rows=", nrow(strict), "\n", sep = "")
cat("validate_registry_rows=", nrow(validated), "\n", sep = "")
cat("all_valid=", all(validated$valid), "\n", sep = "")

stopifnot(nrow(strict) == nrow(validated))
stopifnot(all(validated$valid))

testthat::test_file("tests/testthat/test-validate-dgp-registry.R", stop_on_failure = TRUE)
testthat::test_file("tests/testthat/test-v018-dgp-sidecar-consistency.R", stop_on_failure = TRUE)
testthat::test_file("tests/testthat/test-validate-all.R", stop_on_failure = TRUE)
testthat::test_file("tests/testthat/test-validate-dgp.R", stop_on_failure = TRUE)
