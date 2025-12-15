#' Null-coalescing helper
#'
#' Returns `x` if it is not `NULL`, otherwise returns `y`.
#'
#' @param x Value to test.
#' @param y Fallback if `x` is `NULL`.
#' @return `x` or `y`.
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
  att_raw <- res$att %||% list(estimate = NA_real_)
  if (is.data.frame(att_raw)) {
    att_val <- att_raw[["estimate"]] %||% NA_real_
  } else {
    att_val <- att_raw$estimate %||% NA_real_
  }

  qst_raw <- res$qst
  if (is.null(qst_raw)) {
    qst_tbl <- NULL
  } else {
    if ("value" %in% names(qst_raw) && !"estimate" %in% names(qst_raw)) {
      qst_raw <- dplyr::rename(qst_raw, estimate = value)
    }
    qst_tbl <- tibble::as_tibble(qst_raw)
  }

  list(att = att_val, qst = qst_tbl)
}
