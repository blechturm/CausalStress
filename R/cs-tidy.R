#' Tidy CausalStress run results
#'
#' Universal converter that transforms any CausalStress result object
#' (Single Run, List of Runs, or existing Tibble) into a standard tidy format.
#'
#' @param x A result object from `cs_run_single()`, `cs_run_seeds()`, or a list thereof.
#' @return A tibble with standard performance columns.
#' @export
cs_tidy <- function(x) {
  if (inherits(x, "data.frame")) {
    return(tibble::as_tibble(x))
  }

  if (is.list(x) && !is.null(x$att) && !is.null(x$meta) && !is.data.frame(x)) {
    return(cs_result_to_row(x))
  }

  if (is.list(x) && length(x) > 0 &&
      is.list(x[[1]]) && !is.null(x[[1]]$att) && !is.null(x[[1]]$meta)) {
    return(dplyr::bind_rows(lapply(x, cs_result_to_row)))
  }

  rlang::abort(
    "cs_tidy() does not recognize this object format.",
    class = "causalstress_type_error"
  )
}

#' @rdname cs_tidy
#' @export
cs_tidy_run <- cs_tidy
