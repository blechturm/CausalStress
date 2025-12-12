# ==============================================================================
# Campaign: Placebo Suite Validation (Gatekeeper)
# ==============================================================================

# 1. Thread caps (Best practice)
Sys.setenv(OMP_NUM_THREADS = 1, MKL_NUM_THREADS = 1, OPENBLAS_NUM_THREADS = 1)

library(CausalStress)
library(future)
library(pins)
library(dplyr)

# ------------------------------------------------------------------------------
# 1. CONFIGURATION
# ------------------------------------------------------------------------------
SEEDS <- 1:500
N     <- 1000
B     <- 100    # Reduced B for faster validation (Gatekeeper doesn't need B=200)

# CRITICAL: Uncomment this and force single-threading explicitly
config_gengc <- list(
  num_trees   = 1000,
  n_draws     = 2000,
  num_threads = 1    # Force ranger to stay in its lane
)

# ------------------------------------------------------------------------------
# 2. INFRASTRUCTURE SETUP (FIXED)
# ------------------------------------------------------------------------------
# FIX: Use normalizePath to ensure workers write to the correct physical location
board_path <- normalizePath("_experiments/pins_board", mustWork = FALSE)
dir.create(board_path, recursive = TRUE, showWarnings = FALSE)
board_path <- normalizePath(board_path) # Resolve full path
board <- board_folder(board_path, versioned = TRUE)

staging_dir <- normalizePath("_experiments/staging_placebo", mustWork = FALSE)
dir.create(staging_dir, recursive = TRUE, showWarnings = FALSE)
staging_dir <- normalizePath(staging_dir) # Resolve full path

message(sprintf("📂 Board Absolute Path:   %s", board_path))
message(sprintf("🛡️  Staging Absolute Path: %s", staging_dir))

# Parallel backend
plan(multisession, workers = 20) # Leave 2 cores for OS/Overhead
on.exit(plan(sequential), add = TRUE)

# ------------------------------------------------------------------------------
# 3. EXECUTION
# ------------------------------------------------------------------------------
message("🔄 Checking for staged files...")
cs_gather_results(board, staging_dir)

t_start <- Sys.time()

# Use 'cs_run_campaign' for WIDE execution (prevents DGP barriers)
results <- cs_run_campaign(
  dgp_ids       = cs_get_suite("placebo"),
  estimator_ids = "gengc",
  n             = N,
  seeds         = SEEDS,
  config        = config_gengc,  # Pass the config!
  bootstrap     = TRUE,
  B             = B,
  parallel      = TRUE,
  staging_dir   = staging_dir,
  board         = board,
  show_progress = TRUE,
  skip_existing = TRUE
)

t_end <- Sys.time()
message(sprintf("\n✅ Campaign Finished in %.2f hours", as.numeric(t_end - t_start, units="hours")))
# ------------------------------------------------------------------------------
# 4. ANALYSIS: THE GATEKEEPER
# ------------------------------------------------------------------------------
message("\n⚖️  Summoning the Gatekeeper...")
verdict <- cs_summarise_gatekeeper(results)
print(verdict)
readr::write_rds(verdict, "_experiments/gatekeeper_verdict.rds")

library(pins)
library(dplyr)
library(CausalStress)

# point to the same board path you used when running
board_path <- normalizePath("_experiments/pins_board", mustWork = TRUE)
board <- board_folder(board_path, versioned = TRUE)

# load all runs from the board
pins <- pin_list(board)
runs <- lapply(pins, pin_read, board = board)

# flatten to a tibble and run the gatekeeper
results <- cs_tidy(runs)
verdict <- cs_summarise_gatekeeper(results)
print(verdict)

cs_summarise_gatekeeper(runs)

# ------------------------------------------------------------------------------ 
# 5. VISUALIZATION & DIAGNOSTICS
# ------------------------------------------------------------------------------ 
# Tidy flatten
placebo_tidy <- cs_tidy(results)

# Identify failing runs/estimators (ATT and QST)
if (is.list(verdict) && !is.null(verdict$att)) {
  att_failures <- verdict$att %>%
    dplyr::filter(status == "FAIL")
  if (nrow(att_failures) > 0) {
    message("\nATT Failures detected:")
    print(att_failures)
  } else {
    message("\nATT: PASS (no failures).") 
  }
}

if (is.list(verdict) && !is.null(verdict$qst)) {
  qst_failures <- verdict$qst %>%
    dplyr::filter(status == "FAIL")
  if (nrow(qst_failures) > 0) {
    message("\nQST Failures detected:")
    print(qst_failures)
  } else {
    message("\nQST: PASS (no failures).")
  }
}

# Collect and plot QST curves (if any)
message("\n📈 Collecting QST results for visualization...")
qst_df <- cs_collect_qst(results)

if (nrow(qst_df) > 0) {
  qst_plot <- cs_plot_qst(qst_df)
  print(qst_plot)
  ggplot2::ggsave("_experiments/placebo_qst.png", qst_plot, width = 10, height = 6, dpi = 150)
} else {
  message("No QST data to plot.")
}

# Gatekeeper visuals (coverage heatmap, ATT error)
p_gate <- cs_plot_placebo(placebo_tidy) +
  ggplot2::ggtitle("Gatekeeper: Placebo Suite")
ggplot2::ggsave("_experiments/placebo_gatekeeper.png", p_gate, width = 10, height = 6, dpi = 150)

p_att <- cs_plot_att_error(placebo_tidy) +
  ggplot2::ggtitle("ATT Error (Placebo Suite)")
ggplot2::ggsave("_experiments/placebo_att_error.png", p_att, width = 10, height = 6, dpi = 150)

# Timing diagnostics
placebo_tidy %>%
  dplyr::select(dgp_id, estimator_id, seed,
                run_time_dgp, run_time_est, run_time_total) %>%
  readr::write_rds("_experiments/placebo_timing.rds")

# Gatekeeper culprits for quick inspection
gate <- cs_summarise_gatekeeper(results, threshold = 0.90)
if (!is.null(gate$culprits)) {
  print(gate$culprits)
}

# Export tidy results for further analysis
readr::write_rds(placebo_tidy, "_experiments/placebo_results_tidy.rds")


future::availableCores()
future::nbrOfWorkers()
Sys.getenv(c("FUTURE_AVAILABLE_CORES","FUTURE_WORKERS","MC_CORES","NUMBER_OF_PROCESSORS"))


tasks <- tidyr::expand_grid(
  dgp_id       = cs_get_suite("placebo"),
  estimator_id = "gengc",
  seed         = SEEDS
)

missing <- tasks[!purrr::pmap_lgl(
  tasks[, c("dgp_id", "estimator_id", "seed")],
  ~ CausalStress:::cs_pin_exists(board, ..1, ..2, N, ..3)
), , drop = FALSE]

nrow(missing)
head(missing)


missing %>%
  dplyr::count(dgp_id)


