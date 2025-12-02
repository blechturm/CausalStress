.onLoad <- function(libname, pkgname) {
  opts <- options()
  opts_default <- list(causalstress.validate_on_load = TRUE)
  to_set <- setdiff(names(opts_default), names(opts))
  if (length(to_set) > 0) {
    options(opts_default[to_set])
  }

  if (isTRUE(getOption("causalstress.validate_on_load", TRUE))) {
    cs_validate_dgp_registry(strict = FALSE)
  }
}
