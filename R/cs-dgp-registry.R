#' @noRd
cs_dgp_registry <- function() {
  tibble::tibble(
    dgp_id = "synth_baseline",
    type = "synthetic",
    generator = list(dgp_synth_baseline),
    registry_version = "1.3.0",
    description = "Baseline 5D Gaussian DGP with linear treatment effect and moderate overlap."
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
    dgp_id = row$dgp_id,
    type = row$type,
    generator = row$generator[[1L]],
    registry_version = row$registry_version,
    description = row$description
  )
}
