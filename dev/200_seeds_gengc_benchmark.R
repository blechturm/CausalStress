# ------------------------------------------------------------------------------
# BENCHMARK: GenGC "Dual Engine" Complete Shootout (200 Seeds)
# Objective: Compare Vanilla vs. Screened vs. Doubly-Robust variants
# ------------------------------------------------------------------------------

remove.packages("CausalStress")
unlink(file.path(.libPaths()[1], "CausalStress"), recursive = TRUE, force = TRUE)
devtools::install(upgrade = "never")

library(CausalStress)
library(pins)
library(dplyr)
library(future)

# 1. Setup Storage
# ------------------------------------------------------------------------------
staging_dir <- "C:/Simulations/CausalStress_GenGC_Benchmark_v5" 
board_path  <- "C:/Simulations/CausalStress_GenGC_Benchmark_v5_Board"

if (!dir.exists(staging_dir)) dir.create(staging_dir, recursive = TRUE)
board <- pins::board_folder(board_path)

# 2. Dynamic Registration: Adding DR Contenders
# ------------------------------------------------------------------------------
message("Registering GenGC variants (Standard + Doubly-Robust)...")

# --- Standard G-Computation Variants ---
cs_register_estimator("gengc_qr_vanilla", "gcomp", est_gengc, supports_qst = TRUE)
cs_register_estimator("gengc_qr_screen",  "gcomp", est_gengc, supports_qst = TRUE)
cs_register_estimator("gengc_rf_screen",  "gcomp", est_gengc, supports_qst = TRUE)
cs_register_estimator("gengc_auto",       "gcomp", est_gengc, supports_qst = TRUE)

# --- Doubly-Robust (DR) Variants ---
# These point to est_gengc_dr which uses the augmented inverse probability weighting logic
cs_register_estimator(
  estimator_id = "gengc_dr_qr",
  type         = "dr",
  generator    = est_gengc_dr, 
  supports_qst = TRUE,
  description  = "GenGC DR with Linear QR Engine"
)

cs_register_estimator(
  estimator_id = "gengc_dr_rf_screen",
  type         = "dr",
  generator    = est_gengc_dr, 
  supports_qst = TRUE,
  description  = "GenGC DR with Screened Forest Engine"
)

# 3. Campaign Configuration
# ------------------------------------------------------------------------------
target_dgps <- c(
  "synth_baseline", "synth_heavytail", "synth_placebo_tau0", "synth_qte1",
  "synth_nonlinear_heteroskedastic", "synth_overlap_stressed", "synth_tilt_mild",
  "synth_placebo_nonlinear", "synth_placebo_heavytail", "synth_placebo_tilted",
  "synth_placebo_kangschafer", "synth_hd_sparse_plm"
)

target_estimators <- c(
  "oracle_att", 
  "lm_att", 
  "gengc",
  "gengc_dr",
  "gengc_qr_screen",
  "gengc_rf_screen",
  "gengc_auto",
  "gengc_dr_qr",
  "gengc_dr_rf_screen",
  "grf_dr_att",
  "bart_att",     # Requires bartCause
  "tmle_att"      # Requires tmle, SuperLearner
)



plan <- cs_plan_campaign(
  dgp_list = target_dgps,
  estimator_list = target_estimators,
  n_seeds = 200,        
  batch_size = 20,      
  campaign_seed = 2025,
  strategy_map = list(
    defaults = list(
      n = 1000,
      ci_method = "none" # Fast point estimates
    ),
    overrides = list(
      # Standard G-Comp
      gengc           = list(method = "qrf", screen = FALSE),
      gengc_qr_screen = list(method = "qr",  screen = TRUE),
      gengc_rf_screen = list(method = "qrf", screen = TRUE),
      #gengc_auto      = list(num_trees = "auto", n_draws = "auto"),
      
      # Doubly-Robust
      gengc_dr           = list(method = "qrf", screen = FALSE), # Vanilla DR
      gengc_dr_qr        = list(method = "qr",  screen = TRUE),
      gengc_dr_rf_screen = list(method = "qrf", screen = TRUE)
    )
  )
)

# 4. Execute (Optimized for i9-12900K)
# ------------------------------------------------------------------------------
progressr::handlers("cli")
cs_run_campaign(plan = plan, staging_dir = staging_dir, board = board, workers = 20)

# 5. Consolidate
# ------------------------------------------------------------------------------
cs_consolidate(staging_dir, board)