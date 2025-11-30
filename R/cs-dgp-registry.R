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
      dgp_synth_baseline,
      dgp_synth_heavytail,
      dgp_synth_placebo_tau0,
      dgp_synth_qte1,
      dgp_synth_nonlinear_heteroskedastic,
      dgp_synth_overlap_stressed,
      dgp_synth_tilt_mild,
      dgp_synth_placebo_nonlinear,
      dgp_synth_placebo_heavytail,
      dgp_synth_placebo_tilted,
      dgp_synth_placebo_kangschafer,
      dgp_synth_hd_sparse_plm
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
      "Kang–Schafer misspecification placebo: tau=0, linear in latent Z, nonlinear observed X.",
      "High-dim sparse partially linear: correlated Gaussian X (p=50), sparse outcome/PS, tau=1."
    )
  )
}

#' Get a DGP descriptor by ID
#'
#' Look up a synthetic DGP in the internal registry and return a descriptor
#' containing metadata and the generator function.
#'
#' @param dgp_id Character scalar, the DGP identifier (e.g., "synth_baseline").
#'
#' @return A list with elements:
#'   - dgp_id: character scalar
#'   - type: character scalar (e.g., "synthetic")
#'   - generator: function(n, seed = NULL) returning a DGP object
#'   - registry_version: character scalar
#'   - description: character scalar
#'
#' @export
cs_get_dgp <- function(dgp_id) {
  if (!is.character(dgp_id) || length(dgp_id) != 1L) {
    rlang::abort(
      message = "`dgp_id` must be a character scalar.",
      class   = "causalstress_registry_error"
    )
  }

  reg <- cs_dgp_registry()
  row <- reg[reg$dgp_id == dgp_id, , drop = FALSE]

  if (nrow(row) == 0) {
    rlang::abort(
      message = paste0(
        "Unknown dgp_id: ", dgp_id, ". ",
        "Use cs_dgp_registry() to inspect available DGPs."
      ),
      class   = "causalstress_registry_error"
    )
  }

  list(
    dgp_id     = row$dgp_id,
    type       = row$type,
    generator  = row$generator[[1L]],
    version    = row$version,
    description = row$description
  )
}
