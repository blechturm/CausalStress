#' Null-coalescing helper
#'
#' Returns `x` if it is not `NULL`, otherwise returns `y`.
#'
#' @param x Value to test.
#' @param y Fallback if `x` is `NULL`.
#' @return `x` or `y`.
#' @name null_coalesce
#' @export
`%||%` <- function(x, y) {
  if (!is.null(x)) x else y
}

#' Normalize estimator result components (internal)
#'
#' Extracts ATT and QST from an estimator result, handling list/data.frame
#' variants and renaming `value` to `estimate` for QST.
#' @noRd
cs_extract_estimator_result <- function(res) {
  outputs <- cs_normalize_estimator_outputs(res)
  att_val <- outputs$att$estimate %||% NA_real_
  qst_tbl <- outputs$qst %||% NULL

  list(att = att_val, qst = qst_tbl)
}
