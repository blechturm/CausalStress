# Pre-Release Validation for CausalStress v0.1.4
#
# Runs Gatekeeper (placebo) and Signal (S-curve) validations for the GenGC
# estimator with robust settings. Assumes GenGC is installed.

#library(CausalStress)
devtools::load_all()
library(dplyr)
library(ggplot2)
library(pins)

board <- pins::board_temp()

robust_config <- list(
  num_trees = 1000,
  min_node_size = 20,  # Regularize to prevent overfitting noise
  n_draws = 2000       # Sufficient for smooth tails
)

# -------------------------------------------------------------------
# Experiment 1: Gatekeeper (Placebo)
# -------------------------------------------------------------------
gate_runs <- cs_run_grid(
  dgp_ids       = "synth_placebo_tau0",
  estimator_ids = "gengc",
  n             = 1000,
  seeds         = 1:100,
  bootstrap     = TRUE,
  B             = 200,
  config        = robust_config,
  board         = board,
  skip_existing = TRUE
)

gate_report <- cs_summarise_gatekeeper(gate_runs)

dir.create("dev", showWarnings = FALSE, recursive = TRUE)
sink("dev/validation_gatekeeper.txt")
print(gate_report)
sink()

# -------------------------------------------------------------------
# Experiment 2: Signal (S-curve)
# -------------------------------------------------------------------
signal_runs <- cs_run_grid(
  dgp_ids       = "synth_qte1",
  estimator_ids = "gengc",
  n             = 2000,
  seeds         = 1:5,
  bootstrap     = TRUE,
  B             = 200,
  config        = robust_config,
  board         = board,
  skip_existing = TRUE
)

# Plot S-curve (expect low quantiles < 0, high quantiles > 0)
signal_qst <- signal_runs %>%
  tidyr::unnest(.data$result) %>%
  tidyr::unnest(.data$qst)

p_signal <- cs_plot_qst(signal_qst)
ggplot2::ggsave("dev/validation_signal.png", plot = p_signal, width = 8, height = 6, dpi = 300)

message("Validation complete. Reports written to dev/validation_gatekeeper.txt and dev/validation_signal.png")

