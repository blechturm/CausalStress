#!/usr/bin/env Rscript

sink("ci-substrate.txt", split = TRUE)
on.exit(sink(), add = TRUE)

pkgload::load_all(".", quiet = TRUE)

cat("R.version=", R.version.string, "\n", sep = "")
cat("platform=", R.version$platform, "\n", sep = "")
cat("libPaths=", paste(.libPaths(), collapse = "; "), "\n", sep = "")
cat("ambient_RNGkind=", paste(RNGkind(), collapse = " / "), "\n", sep = "")

ext <- extSoftVersion()
cat(
  "extSoftVersion=",
  paste(names(ext), ext, sep = "=", collapse = "; "),
  "\n",
  sep = ""
)

thread_vars <- c(
  "OMP_NUM_THREADS",
  "OPENBLAS_NUM_THREADS",
  "MKL_NUM_THREADS",
  "BLAS_NUM_THREADS",
  "VECLIB_MAXIMUM_THREADS",
  "RCPP_PARALLEL_NUM_THREADS"
)
cat(
  "thread_env=",
  paste(thread_vars, Sys.getenv(thread_vars, unset = "<unset>"), sep = "=", collapse = "; "),
  "\n",
  sep = ""
)

governed_rng <- cs_with_mandated_rng(20260618L, RNGkind())
cat("governed_generation_RNGkind=", paste(governed_rng, collapse = " / "), "\n", sep = "")

out1 <- dgp_synth_baseline_v160(n = 250L, seed = 20260618L, include_truth = TRUE)
out2 <- dgp_synth_baseline_v160(n = 250L, seed = 20260618L, include_truth = TRUE)
fields <- c("df", "true_att", "true_qst", "meta")
identical_fields <- vapply(
  fields,
  function(field) identical(out1[[field]], out2[[field]]),
  logical(1)
)

cat("include_truth_bitwise=", all(identical_fields), "\n", sep = "")
cat(paste(names(identical_fields), identical_fields, sep = "=", collapse = "\n"), "\n")

stopifnot(all(identical_fields))
