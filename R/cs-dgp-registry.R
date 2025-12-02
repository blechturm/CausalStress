#' Internal DGP registry
#'
#' This registry defines all available data-generating processes (DGPs)
#' for CausalStress. Each row corresponds to one DGP and specifies its
#' identifier, type, generator function, version, and a short description.
#'
#' @noRd
cs_dgp_registry <- function() {
  tibble::tibble(
    dgp_id = c(
      "synth_baseline",
      "synth_heavytail",
      "synth_placebo_tau0",
      "synth_qte1",
      "synth_nonlinear_heteroskedastic",
      "synth_overlap_stressed",
      "synth_tilt_mild",
      "synth_placebo_nonlinear",
      "synth_placebo_heavytail",
      "synth_placebo_tilted",
      "synth_placebo_kangschafer",
      "synth_hd_sparse_plm"
    ),
    type = c(
      "synthetic",
      "synthetic",
      "synthetic",
      "synthetic",
      "synthetic",
      "synthetic",
      "synthetic",
      "synthetic",
      "synthetic",
      "synthetic",
      "synthetic",
      "synthetic"
    ),
    generator = list(
      dgp_synth_baseline_v130,
      dgp_synth_heavytail_v130,
      dgp_synth_placebo_tau0_v130,
      dgp_synth_qte1_v130,
      dgp_synth_nonlinear_heteroskedastic_v130,
      dgp_synth_overlap_stressed_v130,
      dgp_synth_tilt_mild_v130,
      dgp_synth_placebo_nonlinear_v130,
      dgp_synth_placebo_heavytail_v130,
      dgp_synth_placebo_tilted_v130,
      dgp_synth_placebo_kangschafer_v140,
      dgp_synth_hd_sparse_plm_v140
    ),
    version = c(
      "1.3.0",
      "1.3.0",
      "1.3.0",
      "1.3.0",
      "1.3.0",
      "1.3.0",
      "1.3.0",
      "1.3.0",
      "1.3.0",
      "1.3.0",
      "1.4.0",
      "1.4.0"
    ),
    description = c(
      "Baseline linear DGP with Gaussian noise (sanity check).",
      "Same linear signal as synth_baseline but with heavy-tailed noise: 0.8 * N(0, 0.5) + 0.2 * Cauchy(0, 1); robustness (L2 break).",
      "Sharp-null placebo: tau(X) = 0 with Y1 identical to Y0 pathwise.",
      "Sign-flip QTE DGP: tau(X)=+1 if X1>0, -1 otherwise; Student-t noise (df=4, sigma=0.5).",
      "Nonlinear heteroskedastic DGP: sine/quadratic mu0, tau=1, sigma(X)=0.3+0.2|X3|.",
      "Overlap-stressed DGP: same outcome/noise as synth_baseline; propensity plogis(3*X1 + 3*X2) to push p toward 0/1.",
      "Mildly tilted propensity: baseline outcomes/noise, propensity plogis(0.45*X1 - 0.3*X2 - 0.25*X4).",
      "Placebo nonlinear: mu0 = sin(X1) + cos(X2), sharp null Y1 == Y0.",
      "Placebo heavy-tail: baseline mu0, heavy-tailed epsilon (0.8 N + 0.2 Cauchy), sharp null.",
      "Placebo tilted: baseline mu0/noise, strong tilt propensity plogis(1*X1 + 1.2*X2), sharp null.",
      "Kang-Schafer misspecification placebo: tau=0, linear in latent Z, nonlinear observed X.",
      "High-dim sparse partially linear: correlated Gaussian X (p=50), sparse outcome/PS, tau=1."
    ),
    status = c(
      "stable",      # synth_baseline
      "stable",      # synth_heavytail
      rep("experimental", 10)
    ),
    rationale = c(
      "Validated in v0.1.x",
      "Validated in v0.1.x",
      rep("Pending human validation", 10)
    ),
    date_status_changed = c(
      "2025-11-30",
      "2025-11-30",
      rep("2025-11-30", 10)
    ),
    design_spec = c(
      "1.3.0",
      "1.3.0",
      "1.3.0",
      "1.3.0",
      "1.3.0",
      "1.3.0",
      "1.3.0",
      "1.3.0",
      "1.3.0",
      "1.3.0",
      "1.4.0",
      "1.4.0"
    )
  )
}

.valid_dgp_status <- c("stable", "experimental", "deprecated", "invalidated")

# Internal resolver used by cs_get_dgp()
cs_resolve_dgp <- function(reg, dgp_id, version = NULL, status = "stable", quiet = FALSE) {
  if (!is.character(dgp_id) || length(dgp_id) != 1L) {
    rlang::abort("`dgp_id` must be a character scalar.", class = "causalstress_registry_error")
  }
  if (!is.null(status) && !status %in% .valid_dgp_status) {
    rlang::abort("`status` must be one of: {toString(.valid_dgp_status)}.", class = "causalstress_registry_error")
  }

  reg_id <- reg[reg$dgp_id == dgp_id, , drop = FALSE]
  if (nrow(reg_id) == 0) {
    rlang::abort(
      paste0("Unknown dgp_id: ", dgp_id, ". Use cs_dgp_registry() to inspect available DGPs."),
      class = "causalstress_registry_error"
    )
  }

  pick_highest_semver <- function(df) {
    version_chr <- as.character(df$version)
    parsed <- tryCatch(base::package_version(version_chr), error = function(...) NULL)
    if (is.null(parsed)) {
      rlang::abort("Malformed semantic version in registry.", class = "causalstress_registry_error")
    }
    max_ver <- max(parsed)
    winners <- which(as.character(parsed) == as.character(max_ver))
    if (length(winners) != 1L) {
      rlang::abort("Multiple candidates share the same semantic version; registry must be fixed.", class = "causalstress_registry_error")
    }
    df[winners, , drop = FALSE]
  }

  warn_status <- function(row) {
    st <- row$status
    if (quiet) return(invisible(NULL))
    if (st == "experimental") {
      rlang::warn(paste0("Using experimental DGP: ", row$dgp_id, " v", row$version))
    } else if (st %in% c("deprecated", "invalidated")) {
      rationale <- row$rationale
      rationale <- ifelse(is.na(rationale) || rationale == "", "rationale not provided", rationale)
      rlang::warn(paste0(
        "Using ", st, " DGP: ", row$dgp_id, " v", row$version,
        ". Rationale: ", rationale
      ))
    }
    invisible(NULL)
  }

  # version specified: ignore status filtering for selection
  if (!is.null(version)) {
    row <- reg_id[reg_id$version == version, , drop = FALSE]
    if (nrow(row) == 0) {
      rlang::abort(
        paste0("Unknown version ", version, " for dgp_id ", dgp_id, "."),
        class = "causalstress_registry_error"
      )
    }
    if (nrow(row) > 1) {
      rlang::abort("Registry contains duplicate (dgp_id, version) entries.", class = "causalstress_registry_error")
    }
    warn_status(row)
    return(row)
  }

  # default status when NULL
  if (is.null(status)) status <- "stable"

  select_status <- function(df, st, allow_warn = TRUE) {
    cand <- df[df$status == st, , drop = FALSE]
    if (nrow(cand) == 0) {
      return(NULL)
    }
    row <- pick_highest_semver(cand)
    if (allow_warn) warn_status(row)
    row
  }

  if (status == "stable") {
    row <- select_status(reg_id, "stable", allow_warn = FALSE)
    if (!is.null(row)) return(row)
    # fallback to experimental
    row <- select_status(reg_id, "experimental", allow_warn = !quiet)
    if (!is.null(row)) {
      if (!quiet) rlang::warn(paste0("No stable version for ", dgp_id, "; using latest experimental."))
      return(row)
    }
    rlang::abort(paste0("No stable or experimental version available for ", dgp_id, "."), class = "causalstress_registry_error")
  }

  # explicit status selection
  row <- select_status(reg_id, status, allow_warn = !quiet)
  if (is.null(row)) {
    rlang::abort(
      paste0("No versions with status '", status, "' for ", dgp_id, "."),
      class = "causalstress_registry_error"
    )
  }
  row
}

#' Get a DGP descriptor by ID/version/status
#'
#' Look up a DGP in the internal registry and return a descriptor containing
#' metadata and the generator function, with deterministic selection semantics.
#'
#' @param dgp_id Character scalar, the DGP identifier.
#' @param version Character scalar or NULL. If provided, selects exact version.
#' @param status Character scalar or NULL. If version is NULL, selection uses
#'   status fallback (defaults to "stable").
#' @param quiet Logical; if FALSE (default), emit warnings for non-stable status.
#'
#' @return A single-row tibble entry from `cs_dgp_registry()`.
#' @export
cs_get_dgp <- function(dgp_id, version = NULL, status = "stable", quiet = FALSE) {
  reg <- cs_dgp_registry()
  cs_resolve_dgp(reg = reg, dgp_id = dgp_id, version = version, status = status, quiet = quiet)
}
