#' Internal helper: enforce Airlock (strip oracle columns for non-oracle estimators)
#'
#' @noRd
cs_airlock <- function(df, oracle_allowed) {
  if (isTRUE(oracle_allowed)) {
    return(df)
  }

  drop <- c("y0", "y1", "p", "structural_te")
  keep <- setdiff(names(df), drop)

  df2 <- df[, keep, drop = FALSE]

  attr(df2, "structural_te") <- NULL
  attr(df2, "params") <- NULL

  df2
}
