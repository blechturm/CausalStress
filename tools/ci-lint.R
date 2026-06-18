#!/usr/bin/env Rscript

if (!requireNamespace("lintr", quietly = TRUE)) {
  stop("Package 'lintr' is required for lint CI.", call. = FALSE)
}

ci_internal_function_names <- function(path = "R") {
  files <- list.files(path, pattern = "[.][rR]$", full.names = TRUE)
  names <- character()

  for (file in files) {
    lines <- readLines(file, warn = FALSE)
    matches <- regexec(
      "^\\s*([.A-Za-z_][.A-Za-z0-9_]*)\\s*<-\\s*function\\s*\\(",
      lines,
      perl = TRUE
    )
    hits <- regmatches(lines, matches)
    hits <- hits[lengths(hits) == 2L]
    if (length(hits) > 0L) {
      names <- c(names, vapply(hits, `[[`, character(1), 2L))
    }
  }

  unique(names)
}

ci_lint_line <- function(lint) {
  paste(as.character(lint), collapse = " | ")
}

ci_lint_message <- function(lint) {
  parts <- as.character(lint)
  if (length(parts) >= 5L) parts[[5L]] else paste(parts, collapse = " | ")
}

ci_is_internal_helper_false_positive <- function(lint, internal_functions) {
  message <- ci_lint_message(lint)
  matches <- regexec(
    "no visible global function definition for '([^']+)'",
    message,
    perl = TRUE
  )
  hit <- regmatches(message, matches)[[1L]]
  length(hit) == 2L && hit[[2L]] %in% internal_functions
}

linters <- list(
  object_usage_linter = lintr::object_usage_linter()
)

raw_lints <- lintr::lint_package(linters = linters)
internal_functions <- ci_internal_function_names()
ignored <- vapply(
  raw_lints,
  ci_is_internal_helper_false_positive,
  logical(1),
  internal_functions = internal_functions
)
lints <- raw_lints[!ignored]
ignored_lints <- raw_lints[ignored]
header <- c(
  sprintf("lint_count=%s", length(lints)),
  sprintf("ignored_internal_helper_false_positives=%s", length(ignored_lints))
)

if (length(lints) == 0L) {
  writeLines(header, "lint-results.txt")
  saveRDS(lints, "lint-results.rds")
  cat(paste(header, collapse = "\n"), "\n")
  quit(status = 0)
}

lint_lines <- vapply(lints, ci_lint_line, character(1))
ignored_lines <- vapply(ignored_lints, ci_lint_line, character(1))
writeLines(c(header, lint_lines, ignored_lines), "lint-results.txt")
saveRDS(lints, "lint-results.rds")
cat(paste(header, collapse = "\n"), "\n")
cat(paste(lint_lines, collapse = "\n"), "\n")
quit(status = 1)
