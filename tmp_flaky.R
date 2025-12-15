library(CausalStress)
flaky_env <- new.env(parent = emptyenv())
flaky_env$main_done <- FALSE
flaky_env$counter <- 0
flaky_estimator <- function(df, config = list(), tau = cs_tau_oracle, ...) {
  if (isTRUE(flaky_env$main_done)) {
    flaky_env$counter <- flaky_env$counter + 1
    if (flaky_env$counter <= 15) stop('flaky failure')
  } else {
    flaky_env$main_done <- TRUE
  }
  list(att = list(estimate = 0), qst = NULL, meta = list(estimator_id = 'flaky_boot', oracle = FALSE, supports_qst = FALSE))
}
cs_register_estimator(
  estimator_id = 'flaky_boot',
  type = 'gcomp',
  generator = flaky_estimator,
  oracle = FALSE,
  supports_qst = FALSE,
  description = 'Flaky',
  source = 'test',
  requires_pkgs = character(0)
)
res <- cs_run_single(
  dgp_id = 'synth_baseline',
  estimator_id = 'flaky_boot',
  n = 100,
  seed = 1,
  bootstrap = TRUE,
  B = 20,
  parallel = FALSE,
  show_progress = FALSE
)
print(res$meta$n_boot_ok)
print(res$meta$warnings)
