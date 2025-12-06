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

