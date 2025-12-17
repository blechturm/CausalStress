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

  # Strip all attributes except the data.frame essentials to prevent
  # attribute-based truth leakage (side-channel).
  attrs <- attributes(df2)
  keep_attrs <- c("names", "row.names", "class")
  attributes(df2) <- attrs[intersect(names(attrs), keep_attrs)]

  df2
}
