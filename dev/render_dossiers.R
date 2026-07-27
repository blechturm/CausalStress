# Validate and render the 12 registry-keyed DGP dossiers through Quarto.
# Usage:
#   Rscript dev/render_dossiers.R --validate-only
#   Rscript dev/render_dossiers.R --output-dir=docs/dgp

args <- commandArgs(trailingOnly = TRUE)
validate_only <- "--validate-only" %in% args
output_arg <- grep("^--output-dir=", args, value = TRUE)
if (length(output_arg) > 1L) {
  stop("Supply at most one --output-dir argument.", call. = FALSE)
}
output_dir <- if (length(output_arg)) {
  sub("^--output-dir=", "", output_arg)
} else {
  file.path("docs", "dgp")
}

if (!file.exists("DESCRIPTION") || !dir.exists(file.path("inst", "dgp_meta"))) {
  stop("Run dev/render_dossiers.R from the CausalStress project root.", call. = FALSE)
}

if (requireNamespace("pkgload", quietly = TRUE)) {
  pkgload::load_all(quiet = TRUE, export_all = FALSE, helpers = FALSE)
} else if (!requireNamespace("CausalStress", quietly = TRUE)) {
  stop("Install pkgload or CausalStress before validating dossiers.", call. = FALSE)
}

registry <- get("cs_dgp_registry", envir = asNamespace("CausalStress"))()
dgp_ids <- unique(registry$dgp_id)
if (length(dgp_ids) != 12L || anyDuplicated(dgp_ids)) {
  stop("The installed registry must contain exactly 12 unique DGP IDs.", call. = FALSE)
}

meta_dir <- file.path("inst", "dgp_meta")
expected_yml <- paste0(dgp_ids, ".yml")
expected_qmd <- paste0(dgp_ids, ".qmd")
actual_yml <- list.files(meta_dir, pattern = "[.]yml$", recursive = FALSE)
actual_qmd <- list.files(meta_dir, pattern = "[.]qmd$", recursive = FALSE)

if (anyDuplicated(tolower(c(actual_yml, actual_qmd)))) {
  stop("DGP sidecar names must be unique even on case-insensitive filesystems.", call. = FALSE)
}

missing <- c(
  setdiff(expected_yml, actual_yml),
  setdiff(expected_qmd, actual_qmd)
)
extra <- c(
  setdiff(actual_yml, c("schema.yml", expected_yml)),
  setdiff(actual_qmd, expected_qmd)
)
if (length(missing)) {
  stop("Missing registered DGP sidecars: ", paste(missing, collapse = ", "), call. = FALSE)
}
if (length(extra)) {
  stop("Unregistered top-level DGP sidecars: ", paste(extra, collapse = ", "), call. = FALSE)
}

metadata <- lapply(file.path(meta_dir, expected_yml), yaml::read_yaml)
metadata_ids <- vapply(metadata, `[[`, character(1), "dgp_id")
metadata_status <- vapply(metadata, `[[`, character(1), "status")
if (!identical(metadata_ids, dgp_ids)) {
  stop("Each YAML sidecar dgp_id must match its registry-keyed filename.", call. = FALSE)
}
if (!identical(as.integer(table(metadata_status)[c("experimental", "stable")]), c(10L, 2L))) {
  stop("DGP reports must identify exactly 10 experimental and 2 stable IDs.", call. = FALSE)
}

if (validate_only) {
  message("Validated 12 registry-keyed YAML/QMD dossier pairs (2 stable, 10 experimental).")
  quit(save = "no", status = 0L)
}

description <- read.dcf("DESCRIPTION")
required_cli <- unname(description[1L, "Config/CausalStress/QuartoCLI"])
if (!requireNamespace("quarto", quietly = TRUE) || !quarto::quarto_available()) {
  stop("The governed Quarto CLI is required to render DGP dossiers.", call. = FALSE)
}
if (!identical(as.character(quarto::quarto_version()), required_cli)) {
  stop(
    "Quarto CLI ", required_cli, " is required; found ", quarto::quarto_version(), ".",
    call. = FALSE
  )
}

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
cache_job <- paste0("job-", Sys.getpid())
cache_root <- file.path("..", "..", ".quarto", "dossier-cache", cache_job)
staging_dir <- file.path(meta_dir, "_dossier_output", cache_job)
dir.create(staging_dir, recursive = TRUE, showWarnings = FALSE)

for (i in seq_along(dgp_ids)) {
  dgp_id <- dgp_ids[[i]]
  output_file <- paste0(dgp_id, ".html")
  message("Rendering ", dgp_id, " with Quarto ", required_cli, "...")

  # Each quarto_render() call starts a fresh Quarto/knitr process. The sidecar
  # seed remains explicit in the document, and any enabled chunk cache is
  # isolated to this render job rather than written beside the source.
  quarto::quarto_render(
    input = file.path(meta_dir, paste0(dgp_id, ".qmd")),
    output_file = output_file,
    execute_params = list(meta = metadata[[i]], dgp_id = dgp_id),
    metadata = list(
      title = paste("DGP Dossier:", dgp_id),
      knitr = list(
        opts_chunk = list(
          cache.path = gsub("\\\\", "/", file.path(cache_root, paste0(dgp_id, "-")))
        )
      )
    ),
    quarto_args = c("--output-dir", normalizePath(staging_dir, winslash = "/")),
    quiet = FALSE
  )

  rendered <- file.path(staging_dir, output_file)
  if (!file.exists(rendered)) {
    stop("Quarto did not create expected dossier output: ", rendered, call. = FALSE)
  }

  support_name <- paste0(dgp_id, "_files")
  target_html <- file.path(output_dir, output_file)
  target_support <- file.path(output_dir, support_name)
  unlink(c(target_html, target_support), recursive = TRUE)
  if (!file.copy(rendered, target_html, overwrite = TRUE)) {
    stop("Could not copy rendered dossier to ", target_html, ".", call. = FALSE)
  }
  staging_support <- file.path(staging_dir, support_name)
  if (dir.exists(staging_support) &&
      !file.copy(staging_support, output_dir, recursive = TRUE)) {
    stop("Could not copy rendered dossier support files for ", dgp_id, ".", call. = FALSE)
  }
}

rendered <- list.files(output_dir, pattern = "[.]html$", recursive = FALSE)
if (!setequal(rendered, paste0(dgp_ids, ".html"))) {
  stop("Rendered dossier output is missing or contains unregistered HTML files.", call. = FALSE)
}

unlink(file.path(".quarto", "dossier-cache", cache_job), recursive = TRUE)
unlink(staging_dir, recursive = TRUE)
staging_parent <- dirname(staging_dir)
if (!length(list.files(staging_parent, all.files = TRUE, no.. = TRUE))) {
  unlink(staging_parent)
}
message("Rendered all 12 registry-keyed DGP dossiers to ", output_dir, ".")
