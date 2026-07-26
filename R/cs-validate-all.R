#' Validate all registered DGPs
#'
#' Runs `cs_validate_dgp()` over every DGP in the registry.
#'
#' @return A tibble with one row per DGP and summary metrics.
#' @export
cs_validate_registry <- function() {
  reg <- cs_dgp_registry()

  res_list <- lapply(seq_len(nrow(reg)), function(i) {
    id <- reg$dgp_id[[i]]
    version <- reg$version[[i]]
    gen <- reg$generator[[i]]
    val <- cs_validate_dgp(gen, verbose = FALSE)
    tibble::tibble(
      dgp_id = id,
      version = version,
      valid = val$valid,
      cv_true_att = val$cv_true_att,
      mean_naive_bias = val$mean_naive_bias,
      schema = val$checks[["schema"]],
      determinism = val$checks[["determinism"]]
    )
  })

  dplyr::bind_rows(res_list)
}
