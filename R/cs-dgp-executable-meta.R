#' Deterministic executable metadata for DGPs (manual mapping)
#'
#' This helper provides a minimal, authoritative mapping from `(dgp_id, version)`
#' to descriptive fields used for validating YAML sidecars, without inspecting
#' generated data and without modifying published DGP generator code.
#'
#' @param dgp_id Character scalar DGP id.
#' @param version Character scalar DGP version string (must match registry).
#'
#' @return A list with fields `noise_family` and `effect_type`.
#' @export
cs_dgp_executable_meta <- function(dgp_id, version) {
  if (!is.character(dgp_id) || length(dgp_id) != 1L) {
    rlang::abort("`dgp_id` must be a character scalar.", class = "causalstress_contract_error")
  }
  if (!is.character(version) || length(version) != 1L) {
    rlang::abort("`version` must be a character scalar.", class = "causalstress_contract_error")
  }

  key <- paste0(dgp_id, "@", version)

  # Fields must use the canonical YAML vocabulary in inst/dgp_meta/schema.yml:
  # noise ∈ {gaussian, heavy, heteroskedastic, bounded}
  # effect ∈ {constant, linear, nonlinear, heterogeneous}
  map <- list(
    "synth_baseline@1.3.0" = list(noise_family = "gaussian", effect_type = "linear"),
    "synth_heavytail@1.3.0" = list(noise_family = "heavy", effect_type = "linear"),
    "synth_placebo_tau0@1.3.0" = list(noise_family = "gaussian", effect_type = "constant"),
    "synth_qte1@1.3.0" = list(noise_family = "heavy", effect_type = "heterogeneous"),
    "synth_nonlinear_heteroskedastic@1.3.0" = list(noise_family = "heteroskedastic", effect_type = "constant"),
    "synth_overlap_stressed@1.3.0" = list(noise_family = "gaussian", effect_type = "linear"),
    "synth_tilt_mild@1.3.0" = list(noise_family = "gaussian", effect_type = "linear"),
    "synth_placebo_nonlinear@1.3.0" = list(noise_family = "gaussian", effect_type = "constant"),
    "synth_placebo_heavytail@1.3.0" = list(noise_family = "heavy", effect_type = "constant"),
    "synth_placebo_tilted@1.3.0" = list(noise_family = "gaussian", effect_type = "constant"),

    "synth_placebo_kangschafer@1.4.0" = list(noise_family = "gaussian", effect_type = "constant"),
    "synth_hd_sparse_plm@1.4.0" = list(noise_family = "gaussian", effect_type = "constant")
  )

  if (!key %in% names(map)) {
    rlang::abort(
      message = glue::glue("No executable meta mapping for {dgp_id} v{version}."),
      class = "causalstress_registry_error"
    )
  }

  map[[key]]
}

