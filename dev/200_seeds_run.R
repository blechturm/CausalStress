# run_full_campaign.R
# ------------------------------------------------------------------------------
# Objective: Run ALL registered DGPs x ALL registered Estimators
# Hardware:  Intel i9-12900K (Using 20/24 threads)
# ------------------------------------------------------------------------------

remove.packages("CausalStress")
unlink(file.path(.libPaths()[1], "CausalStress"), recursive = TRUE, force = TRUE)
devtools::install(upgrade = "never")

# 1. Setup
library(CausalStress)
library(pins)
library(dplyr)
library(future)

# Define storage locations
staging_dir <- "C:/Simulations/CausalStress_Staging_v3" 
board_path  <- "C:/Simulations/CausalStress_Board_v3"

if (!dir.exists(staging_dir)) dir.create(staging_dir, recursive = TRUE)
board <- pins::board_folder(board_path)

# ------------------------------------------------------------------------------
# 2. The Manifest (Extracted from Source Code)
# ------------------------------------------------------------------------------

# From R/cs-dgp-registry.R
all_dgps <- c(
  "synth_baseline",
  "synth_heavytail",
  "synth_placebo_tau0",
  "synth_qte1",
  "synth_nonlinear_heteroskedastic",
  "synth_overlap_stressed",
  "synth_tilt_mild",
  "synth_placebo_nonlinear",
  "synth_placebo_heavytail",
  "synth_placebo_tilted",
  "synth_placebo_kangschafer",
  "synth_hd_sparse_plm"
)

# From R/cs-estimator-registry.R
all_ests <- c(
  "oracle_att",
  "lm_att",
  "ipw_att",
  "gengc",
  "gengc_dr",
  "grf_dr_att",
  "bart_att",     # Requires bartCause
  "tmle_att"      # Requires tmle, SuperLearner
)

message(sprintf("Found %d DGPs and %d Estimators manually.", length(all_dgps), length(all_ests)))

# ------------------------------------------------------------------------------
# 3. The Flight Plan
# ------------------------------------------------------------------------------
plan <- cs_plan_campaign(
  dgp_list = all_dgps,
  estimator_list = all_ests,
  n_seeds = 200,          # 200 Seeds
  batch_size = 15,        # 1 Batch = 15 Runs
  campaign_seed = 2024,
  strategy_map = list(
    defaults = list(
      n = 1000,           # Sample Size
      ci_method = "default"
    ),
    overrides = list(
      gengc_dr = list(
        ci_method = "none"
      )
    )
  )
)

message(sprintf("Campaign Generated: %d Batches Total.", nrow(plan)))

# Add this near library(CausalStress)
progressr::handlers("cli")

# ------------------------------------------------------------------------------
# 4. Execute (Optimized for i9-12900K)
# ------------------------------------------------------------------------------
# We use the new wrapper if available, otherwise manual parallel
if (exists("cs_run_campaign")) {
  message("Starting cs_run_campaign (Smart Wrapper)...")
  cs_run_campaign(
  plan = plan, 
  staging_dir = staging_dir, 
  board = board, 
  workers = 20
)
} else {
  # Fallback if Ticket 11 isn't merged yet
  message("Starting Manual Parallel Loop...")
  future::plan(future::multisession, workers = 20)
  
  # Filter for Resume Logic manually
  done_files <- list.files(staging_dir)
  done_ids <- as.integer(stringr::str_extract(done_files, "(?<=batch_)\\d+"))
  todo_ids <- setdiff(plan$batch_id, done_ids)
  
  if (length(todo_ids) > 0) {
    furrr::future_walk(todo_ids, function(id) {
      cs_run_batch(id, plan, staging_dir)
    }, .progress = TRUE, .options = furrr::furrr_options(seed = TRUE))
  }
}

# ------------------------------------------------------------------------------
# 5. Consolidate
# ------------------------------------------------------------------------------
cs_consolidate(staging_dir, board)
print(cs_audit(board))
