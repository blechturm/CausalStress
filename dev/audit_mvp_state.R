# Ensure design directory exists
if (!dir.exists("design")) {
  dir.create("design", recursive = TRUE)
}

# Discover R files
r_files <- list.files("R", pattern = "\\.[rR]$", full.names = TRUE)

parse_functions <- function(path) {
  lines <- readLines(path, warn = FALSE)
  defs_idx <- grep("^([a-zA-Z0-9_.]+)\\s*<-\\s*function\\s*\\(", lines)
  if (length(defs_idx) == 0) {
    return(NULL)
  }

  funs <- character(length(defs_idx))
  titles <- rep(NA_character_, length(defs_idx))

  for (i in seq_along(defs_idx)) {
    line <- lines[defs_idx[i]]
    funs[i] <- sub("^([a-zA-Z0-9_.]+)\\s*<-\\s*function\\s*\\(.*$", "\\1", line)

    if (defs_idx[i] > 1L) {
      prev_line <- lines[defs_idx[i] - 1L]
      if (grepl("^#'", prev_line)) {
        titles[i] <- trimws(sub("^#'\\s*", "", prev_line))
      }
    }
  }

  data.frame(
    file = path,
    filename = basename(path),
    fun = funs,
    title = titles,
    stringsAsFactors = FALSE
  )
}

fun_df_list <- lapply(r_files, parse_functions)
fun_df <- do.call(rbind, fun_df_list)

# Parse NAMESPACE exports
ns_lines <- readLines("NAMESPACE", warn = FALSE)
export_lines <- grep("^export\\(", ns_lines, value = TRUE)
exported_fns <- sub("^export\\(([^)]+)\\).*", "\\1", export_lines)

if (is.null(fun_df) || nrow(fun_df) == 0) {
  fun_df <- data.frame(
    file = character(),
    filename = character(),
    fun = character(),
    title = character(),
    stringsAsFactors = FALSE
  )
}

fun_df$exported <- fun_df$fun %in% exported_fns

exported_tbl <- fun_df[fun_df$exported, , drop = FALSE]
internal_tbl <- fun_df[!fun_df$exported, , drop = FALSE]

# Discover testthat files
test_files <- list.files("tests/testthat", pattern = "\\.[rR]$", full.names = FALSE)

# Build Markdown
lines <- c(
  "# CausalStress MVP Code Audit",
  "",
  paste0("Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  "",
  "## 1. Exported functions",
  ""
)

if (nrow(exported_tbl) > 0) {
  lines <- c(
    lines,
    "| Function | File | Title |",
    "|---------|------|-------|"
  )
  for (i in seq_len(nrow(exported_tbl))) {
    fn <- exported_tbl$fun[i]
    fil <- exported_tbl$filename[i]
    ttl <- exported_tbl$title[i]
    if (is.na(ttl)) ttl <- ""
    lines <- c(lines, paste0("| ", fn, " | ", fil, " | ", ttl, " |"))
  }
} else {
  lines <- c(lines, "_No exported functions found._")
}

lines <- c(lines, "", "## 2. Internal helpers", "")

if (nrow(internal_tbl) > 0) {
  lines <- c(
    lines,
    "| Function | File | Title |",
    "|---------|------|-------|"
  )
  for (i in seq_len(nrow(internal_tbl))) {
    fn <- internal_tbl$fun[i]
    fil <- internal_tbl$filename[i]
    ttl <- internal_tbl$title[i]
    if (is.na(ttl)) ttl <- ""
    lines <- c(lines, paste0("| ", fn, " | ", fil, " | ", ttl, " |"))
  }
} else {
  lines <- c(lines, "_No internal helpers found._")
}

lines <- c(lines, "", "## 3. Testthat files", "")

if (length(test_files) > 0) {
  for (tf in test_files) {
    lines <- c(lines, paste0("- ", tf))
  }
} else {
  lines <- c(lines, "_No tests/testthat files found._")
}

out_path <- file.path("design", "CAUSALSTRESS_MVP_AUDIT.md")
writeLines(lines, out_path)
message("Wrote audit to: ", out_path)
