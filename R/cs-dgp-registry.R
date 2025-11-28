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
      "synth_heavytail"
    ),
    type = c(
      "synthetic",
      "synthetic"
    ),
    generator = list(
      dgp_synth_baseline,
      dgp_synth_heavytail
    ),
    version = c(
      "1.3.0",
      "1.3.0"
    ),
    description = c(
      "Baseline linear DGP with Gaussian noise (sanity check).",
      "Same linear signal as synth_baseline but with heavy-tailed noise: 0.8 * N(0, 0.5) + 0.2 * Cauchy(0, 1); robustness (L2 break)."
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
