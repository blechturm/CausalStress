devtools::load_all(quiet=TRUE)
make_resolve_reg <- function() {
  tibble::tibble(
    dgp_id = c("foo", "foo", "foo", "bar"),
    type = "synthetic",
    generator = list(
      function(n, seed = NULL) list(),
      function(n, seed = NULL) list(),
      function(n, seed = NULL) list(),
      function(n, seed = NULL) list()
    ),
    version = c("1.0.0", "1.1.0", "0.9.0", "1.0.0"),
    description = "desc",
    status = c("stable", "experimental", "deprecated", "stable"),
    rationale = c("", "", "old", ""),
    date_status_changed = NA_character_,
    design_spec = "1.0.0"
  )
}
reg <- make_resolve_reg()
reg <- reg[reg$dgp_id == "foo", , drop = FALSE]
reg <- rbind(reg, reg[1, , drop = FALSE])
print(reg)
cs_resolve_dgp(reg, dgp_id='foo', status='stable', quiet=TRUE)
