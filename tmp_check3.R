devtools::load_all(quiet=TRUE)
tie_reg <- tibble::tibble(
  dgp_id = c("foo", "foo"),
  type = "synthetic",
  generator = list(function(n, seed = NULL) list(), function(n, seed = NULL) list()),
  version = c("1.0.0", "1.0.0"),
  description = "desc",
  status = c("stable", "stable"),
  rationale = c("", ""),
  date_status_changed = NA_character_,
  design_spec = "1.0.0"
)
cs_resolve_dgp(tie_reg, dgp_id='foo', status='stable', quiet=TRUE)
