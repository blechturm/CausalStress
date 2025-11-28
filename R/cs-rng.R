#' Set CausalStress RNG state
#'
#' Configures the random number generator to use a fixed backend and optionally seeds it.
#'
#' @param seed Optional seed passed to `set.seed()`.
#' @return Invisibly returns `NULL`.
#' @export
cs_set_rng <- function(seed = NULL) {
  suppressWarnings(
    RNGkind("Mersenne-Twister", "Inversion", "Rounding")
  )
  if (!is.null(seed)) set.seed(seed)
  invisible(NULL)
}

