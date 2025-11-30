devtools::load_all(quiet=TRUE)
reg <- tibble::tibble(
  dgp_id = c("foo","foo","foo"),
  type = "synthetic",
  generator = list(function(n,seed=NULL) list(), function(n,seed=NULL) list(), function(n,seed=NULL) list()),
  version = c("1.0.0","1.0.0","1.0.0"),
  description = "desc",
  status = c("stable","stable","stable"),
  rationale = "",
  date_status_changed = NA_character_,
  design_spec = "1.0.0"
)
print(reg)
res <- try(cs_resolve_dgp(reg, dgp_id='foo', status='stable', quiet=TRUE))
print(res)
