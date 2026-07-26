# Demo: v0.1.9 Scalable Batching (Plan -> Run -> Consolidate -> Tidy)
# Run from project root. Uses a temp board + temp staging.

#library(CausalStress)

devtools::load_all()
library(pins)
library(dplyr)

# ------------------------------------------------------------------------------
# 1) Plan a small batched campaign (strategy map + shuffle)
# ------------------------------------------------------------------------------
plan <- cs_plan_campaign(
  dgp_list = "synth_baseline",
  estimator_list = c("lm_att", "ipw_att"),
  n_seeds = 100,
  batch_size = 20,
  campaign_seed = 123,
  strategy_map = list(
    defaults = list(
      n = 5000,
      ci_method = "bootstrap",
      n_boot = 50
    ),
    overrides = list(
      ipw_att = list(ci_method = "bootstrap", n_boot = 30)
    )
  )
)

cat("\n--- Plan ---\n")
print(plan)

# ------------------------------------------------------------------------------
# 2) Run the plan (campaign runner handles resume + parallel)
# ------------------------------------------------------------------------------
staging_dir <- file.path(tempdir(), "cs_v019_staging_demo")
dir.create(staging_dir, recursive = TRUE, showWarnings = FALSE)

cat("\n--- Running Plan (resume + parallel) ---\n")
cs_run_campaign(
  plan = plan,
  staging_dir = staging_dir,
  board = NULL,
  workers = 2
)

# ------------------------------------------------------------------------------
# 3) Consolidate to pins board (coordinator side)
# ------------------------------------------------------------------------------
board <- pins::board_temp()
n_ok <- cs_consolidate(staging_dir, board)
cat("\n--- Consolidate ---\n")
cat("batches consolidated:", n_ok, "\n")
cat("pins:", paste(pins::pin_list(board), collapse = ", "), "\n")

# ------------------------------------------------------------------------------
# 4) Read + Tidy batch results (1 batch)
# ------------------------------------------------------------------------------
batch_results <- cs_read_batch(board, plan$batch_id[[1]])
tidy_batch <- cs_tidy_batch(batch_results)

cat("\n--- Tidy Batch (first batch) ---\n")
print(
  tidy_batch %>%
    select(dgp_id, estimator_id, seed, est_att, att_ci_lo, att_ci_hi) %>%
    arrange(estimator_id, seed)
)

# ------------------------------------------------------------------------------
# 5) Audit board (1 row per batch)
# ------------------------------------------------------------------------------
cat("\n--- Audit ---\n")
print(cs_audit(board))

# ------------------------------------------------------------------------------
# 6) Resume behavior (no work when already consolidated)
# ------------------------------------------------------------------------------
cat("\n--- Resume (should be no remaining work) ---\n")
cs_run_campaign(
  plan = plan,
  staging_dir = staging_dir,
  board = board,
  workers = 2
)
