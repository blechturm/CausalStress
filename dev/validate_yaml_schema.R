# Validate YAML metadata against schema
# Usage: Rscript dev/validate_yaml_schema.R (from project root)

suppressPackageStartupMessages({
  library(yaml)
  library(purrr)
  library(glue)
  library(fs)
  library(cli)
})

or_else <- function(x, y) if (is.null(x)) y else x

schema_path <- path("inst", "dgp_meta", "schema.yml")
if (!file_exists(schema_path)) {
  stop("Schema file not found at inst/dgp_meta/schema.yml")
}

schema <- yaml::read_yaml(schema_path)

validate_stress_profile <- function(dgp_id, stress_profile, schema) {
  errors <- character()

  for (field in names(schema$stress_profile_schema)) {
    value <- stress_profile[[field]]
    allowed <- schema$stress_profile_schema[[field]]$values

    if (is.null(value)) {
      errors <- c(errors, glue("{field} is missing"))
    } else if (!value %in% allowed) {
      errors <- c(errors, glue("{field}='{value}' not in [{paste(allowed, collapse = ', ')}]"))
    }
  }

  if (length(errors)) {
    cli::cli_abort(c(
      glue("Validation failed for {dgp_id}:"),
      paste0("• ", errors)
    ))
  }
}

validate_one <- function(yml_path) {
  meta <- yaml::read_yaml(yml_path)
  dgp_id <- or_else(meta$dgp_id, path_ext_remove(path_file(yml_path)))

  # stress_profile validation
  validate_stress_profile(dgp_id, or_else(meta$stress_profile, list()), schema)

  # difficulty stars
  stars <- or_else(meta$difficulty$stars, NA)
  if (!is.numeric(stars) || length(stars) != 1 || is.na(stars) || stars < 1 || stars > 5) {
    cli::cli_abort(glue("{dgp_id}: difficulty$stars must be an integer between 1 and 5"))
  }
  if (abs(stars - round(stars)) > .Machine$double.eps^0.5) {
    cli::cli_abort(glue("{dgp_id}: difficulty$stars must be an integer"))
  }

  # status
  allowed_status <- schema$status_schema$values
  status_val <- or_else(meta$status, NA_character_)
  if (is.na(status_val) || !status_val %in% allowed_status) {
    cli::cli_abort(glue("{dgp_id}: status='{status_val}' not in [{paste(allowed_status, collapse = ', ')}]"))
  }

  cli::cli_alert_success(glue("{dgp_id} ok"))
}

validate_all_yaml <- function() {
  yml_files <- dir_ls(path("inst", "dgp_meta"), glob = "*.yml")
  yml_files <- yml_files[path_file(yml_files) != "schema.yml"]

  if (!length(yml_files)) {
    cli::cli_warn("No DGP YAML files found (only schema present).")
    return(invisible(TRUE))
  }

  walk(yml_files, validate_one)
  cli::cli_alert_success("✓ All YAML files valid")
  invisible(TRUE)
}

if (identical(environmentName(topenv()), "R_GlobalEnv") || !interactive()) {
  validate_all_yaml()
}
