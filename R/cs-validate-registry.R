#' Validate the DGP registry structure
#'
#' Checks that `cs_dgp_registry()` returns entries with the expected schema,
#' required columns, and basic generator properties. Intended primarily as an
#' internal guard; in v0.1.3 this is called on package load with warnings only.
#'
#' @param strict Logical; if `TRUE`, abort on errors. If `FALSE`, emit warnings.
#'   Defaults to `FALSE` for startup validation.
#'
#' @return Invisible `TRUE` on success.
#' @export
cs_validate_dgp_registry <- function(strict = FALSE) {
  reg <- cs_dgp_registry()
  required_cols <- c(
    "dgp_id", "type", "generator", "version", "description",
    "status", "rationale", "date_status_changed", "design_spec"
  )
  missing_cols <- setdiff(required_cols, names(reg))
  if (length(missing_cols) > 0) {
    cli::cli_abort("DGP registry missing required columns: {toString(missing_cols)}.")
  }

  # basic schema checks ------------------------------------------------------
  allowed_types <- c("synthetic", "real")
  bad_type <- setdiff(unique(reg$type), allowed_types)
  if (length(bad_type) > 0) {
    cli::cli_abort("DGP registry contains unsupported type(s): {toString(bad_type)}.")
  }
  bad_status <- setdiff(unique(reg$status), .valid_dgp_status)
  if (length(bad_status) > 0) {
    cli::cli_abort("DGP registry contains unsupported status value(s): {toString(bad_status)}.")
  }

  # uniqueness and stability invariants -------------------------------------
  dup_key <- reg[duplicated(reg[, c("dgp_id", "version")]), , drop = FALSE]
  if (nrow(dup_key) > 0) {
    cli::cli_abort("DGP registry contains duplicate (dgp_id, version) entries: {toString(unique(dup_key$dgp_id))}.")
  }
  stable_counts <- table(reg$dgp_id[reg$status == "stable"])
  if (any(stable_counts > 1L)) {
    bad <- names(stable_counts)[stable_counts > 1L]
    cli::cli_abort("DGP registry has more than one stable version for: {toString(bad)}.")
  }

  # semver validation -------------------------------------------------------
  version_chr <- as.character(reg$version)
  malformed <- !grepl("^\\d+\\.\\d+\\.\\d+$", version_chr)
  if (any(malformed)) {
    cli::cli_abort("DGP registry contains malformed semantic version(s): {toString(reg$version[malformed])}.")
  }

  # rationale required for deprecated/invalidated ---------------------------
  bad_rationale <- reg$status %in% c("deprecated", "invalidated") &
    (is.na(reg$rationale) | reg$rationale == "")
  if (any(bad_rationale)) {
    cli::cli_abort("Deprecated/invalidated DGPs must include a rationale.")
  }

  # warn if stable rows span multiple design specs --------------------------
  stable_specs <- unique(reg$design_spec[reg$status == "stable"])
  if (length(stable_specs) > 1) {
    cli::cli_warn("Stable DGPs span multiple design_spec versions: {toString(stable_specs)}.")
  }

  # generator sanity --------------------------------------------------------
  for (i in seq_len(nrow(reg))) {
    gen <- reg$generator[[i]]
    if (!is.function(gen)) {
      cli::cli_abort("Registry entry {reg$dgp_id[i]} has non-function generator.")
    }
  }

  # YAML sidecar consistency (minimal executable meta) -----------------------
  for (i in seq_len(nrow(reg))) {
    id <- reg$dgp_id[[i]]
    ver <- reg$version[[i]]

    yaml_path <- system.file("dgp_meta", paste0(id, ".yml"), package = "CausalStress")
    if (yaml_path == "") {
      yaml_path <- file.path("inst", "dgp_meta", paste0(id, ".yml"))
    }
    if (!file.exists(yaml_path)) next

    yml <- try(yaml::read_yaml(yaml_path), silent = TRUE)
    if (inherits(yml, "try-error")) {
      msg <- paste0("Failed to read YAML sidecar for ", id, ": ", yaml_path)
      if (strict) cli::cli_abort(msg) else cli::cli_warn(msg)
      next
    }

    y_noise <- yml$stress_profile$noise %||% NA_character_
    y_eff <- yml$stress_profile$effect %||% NA_character_

    exec <- try(cs_dgp_executable_meta(id, ver), silent = TRUE)
    if (inherits(exec, "try-error")) {
      msg <- paste0("Executable meta mapping missing for ", id, " v", ver, ".")
      if (strict) cli::cli_abort(msg) else cli::cli_warn(msg)
      next
    }

    if (!identical(as.character(y_noise), as.character(exec$noise_family))) {
      msg <- glue::glue(
        "YAML sidecar mismatch for {id} v{ver}: noise='{y_noise}' but executable meta says '{exec$noise_family}'."
      )
      if (strict) cli::cli_abort(msg) else cli::cli_warn(msg)
    }
    if (!identical(as.character(y_eff), as.character(exec$effect_type))) {
      msg <- glue::glue(
        "YAML sidecar mismatch for {id} v{ver}: effect='{y_eff}' but executable meta says '{exec$effect_type}'."
      )
      if (strict) cli::cli_abort(msg) else cli::cli_warn(msg)
    }
  }

  # optional lightweight generator checks for synthetic types ---------------
  check_fun <- function(expr) {
    if (strict) {
      force(expr)
    } else {
      try(expr, silent = TRUE)
      NULL
    }
  }

  for (i in seq_len(nrow(reg))) {
    # Check formals include n
    fmls <- formals(reg$generator[[i]])
    if (is.null(fmls$n)) {
      cli::cli_warn("Generator for {reg$dgp_id[i]} does not declare argument `n`.")
    }
    if (identical(reg$type[i], "synthetic")) {
      check_fun({
        g <- reg$generator[[i]](n = 5, seed = 1L)
        needed <- c("df", "true_att", "true_qst", "meta")
        miss <- setdiff(needed, names(g))
        if (length(miss) > 0) {
          cli::cli_warn("Generator {reg$dgp_id[i]} output missing: {toString(miss)}.")
        } else {
          req_cols <- c("y0", "y1", "p", "structural_te")
          if (!all(req_cols %in% names(g$df))) {
            cli::cli_warn("Generator {reg$dgp_id[i]} df missing columns: {toString(setdiff(req_cols, names(g$df)))}.")
          }
        }
      })
    }
  }

  invisible(reg)
}
