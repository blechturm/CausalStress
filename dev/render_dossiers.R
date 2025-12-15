# Render DGP dossiers (Layer 2 + 3 sidecars)
# Usage: Rscript dev/render_dossiers.R (from project root)

suppressPackageStartupMessages({
  library(yaml)
  library(rmarkdown)
  library(glue)
  library(purrr)
  library(fs)
  library(cli)
})

or_else <- function(x, y) if (is.null(x)) y else x

get_registry_ids <- function() {
  # Try exported first
  reg_fn <- NULL
  if (exists("cs_dgp_registry", where = asNamespace("CausalStress"), inherits = FALSE)) {
    reg_fn <- get("cs_dgp_registry", envir = asNamespace("CausalStress"))
  } else if (exists("cs_dgp_registry", where = "package:CausalStress", inherits = FALSE)) {
    reg_fn <- get("cs_dgp_registry", envir = getNamespace("CausalStress"))
  }
  if (is.null(reg_fn)) {
    stop("cs_dgp_registry not found; load CausalStress before rendering dossiers.")
  }
  reg_fn()$dgp_id
}

render_dossier <- function(dgp_id, force = FALSE, output_dir = "inst/dossiers") {
  yaml_path <- path("inst", "dgp_meta", paste0(dgp_id, ".yml"))
  rmd_path  <- path("inst", "dgp_meta", paste0(dgp_id, ".Rmd"))
  output_dir <- path_norm(output_dir)
  output_file <- path(output_dir, paste0(dgp_id, ".html"))

  if (!file_exists(yaml_path)) {
    cli::cli_warn(glue("No YAML metadata found for {dgp_id}; skipping."))
    return(invisible(NULL))
  }
  if (!file_exists(rmd_path)) {
    cli::cli_warn(glue("No Rmd narrative found for {dgp_id}; skipping."))
    return(invisible(NULL))
  }

  dir_create(output_dir, recurse = TRUE)

  if (!force && file_exists(output_file)) {
    info <- file_info(c(yaml_path, rmd_path, output_file))
    newest_input <- max(info$modification_time[1:2], na.rm = TRUE)
    if (info$modification_time[3] > newest_input) {
      cli::cli_alert_success(glue("Skipping {dgp_id} (up to date)"))
      return(invisible(output_file))
    }
  }

  meta <- yaml::read_yaml(yaml_path)

  cli::cli_alert(glue("Rendering dossier for {dgp_id}"))
  res <- tryCatch(
    rmarkdown::render(
      input = rmd_path,
      output_file = path_file(output_file),
      output_dir = output_dir,
      params = list(meta = meta, dgp_id = dgp_id),
      quiet = TRUE
    ),
    error = function(e) {
      cli::cli_warn(glue("Failed to render {dgp_id}: {conditionMessage(e)}"))
      return(NULL)
    }
  )

  if (!is.null(res)) {
    cli::cli_alert_success(glue("Generated {output_file}"))
  }
  invisible(res)
}

render_all_dossiers <- function(force = FALSE) {
  dir_create(path("inst", "dossiers"), recurse = TRUE)

  dgp_ids <- get_registry_ids()
  cli::cli_alert(glue("Rendering {length(dgp_ids)} dossiers..."))

  outputs <- map(dgp_ids, render_dossier, force = force)
  n_ok <- sum(map_lgl(outputs, ~ !is.null(.x)))
  cli::cli_alert_success(glue("Complete: {n_ok}/{length(dgp_ids)} dossiers rendered"))
  invisible(outputs)
}

if (identical(environmentName(topenv()), "R_GlobalEnv") || !interactive()) {
  render_all_dossiers(force = FALSE)
}
