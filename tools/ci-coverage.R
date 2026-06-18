#!/usr/bin/env Rscript

if (!requireNamespace("covr", quietly = TRUE)) {
  stop("Package 'covr' is required for coverage CI.", call. = FALSE)
}

coverage <- covr::package_coverage(
  path = ".",
  type = "tests",
  quiet = FALSE
)

coverage_percent <- as.numeric(covr::percent_coverage(coverage))
summary_lines <- c(
  sprintf("coverage_percent=%.2f", coverage_percent),
  sprintf("coverage_entries=%s", length(coverage))
)

writeLines(summary_lines, "coverage-summary.txt")
saveRDS(coverage, "coverage.rds")
cat(paste(summary_lines, collapse = "\n"), "\n")
