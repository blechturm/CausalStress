# ==============================================================================
# SMOKE TEST: Native CI & Provenance Refactor (v0.1.7)
# ==============================================================================
# Goal: Fast verification (2 seeds) across ALL estimators, exercising:
#   - Lightweights + GenGC: refit bootstrap CIs
#   - Heavyweights (GRF/TMLE/BART): native CIs (package-provided)
#
# Note: cs_run_campaign takes a single `config` for all estimators.
# We therefore run two sub-campaigns (bootstrap + native) and then bind results.

library(CausalStress)
library(pins)
library(future)
library(dplyr)

# Wide & shallow thread caps (avoid oversubscription)
Sys.setenv(
  OMP_NUM_THREADS = 1,
  MKL_NUM_THREADS = 1,
  OPENBLAS_NUM_THREADS = 1,
  VECLIB_MAXIMUM_THREADS = 1
)

# 1. Config: Test All Available Estimators
# ------------------------------------------------------------------------------
# We test both Lightweights (native R) and Heavyweights (external pkgs).
# The tryCatch ensures we only run what is installed/registered.
candidates <- c(
  "lm_att", "ipw_att",              # Lightweights
  "gengc", "gengc_dr",              # GenGC (Refit Bootstrap)
  "grf_dr_att",                     # GRF (Native/Bootstrap)
  "tmle_att",                       # TMLE (Native/Bootstrap)
  "bart_att"                        # BART (Native/Bootstrap)
)

estimator_ids <- candidates[sapply(candidates, function(id) {
  tryCatch({ cs_get_estimator(id); TRUE }, error = function(...) FALSE)
})]

# Split into two groups so we can request CI methods per group.
light_boot_ids <- intersect(estimator_ids, c("lm_att", "ipw_att", "gengc", "gengc_dr"))
heavy_native_ids <- intersect(estimator_ids, c("grf_dr_att", "tmle_att", "bart_att"))

# Pick a single fast DGP
dgp_ids <- c("synth_baseline")

# 2. Infrastructure
# ------------------------------------------------------------------------------
# Use distinct boards/staging dirs so we don't overwrite pins between runs
mk_board <- function(suffix) {
  board_path  <- normalizePath(file.path("_experiments", paste0("pins_board_test_", suffix)), mustWork = FALSE)
  staging_dir <- normalizePath(file.path("_experiments", paste0("staging_test_", suffix)), mustWork = FALSE)
  dir.create(board_path,  recursive = TRUE, showWarnings = FALSE)
  dir.create(staging_dir, recursive = TRUE, showWarnings = FALSE)
  list(
    board = board_folder(board_path, versioned = TRUE),
    staging_dir = staging_dir,
    board_path = board_path
  )
}

# 3. Parallel Execution
# ------------------------------------------------------------------------------
plan(multisession, workers = 4) # Slightly more workers for heavier load
on.exit(plan(sequential), add = TRUE)

message(">>> Starting Full Smoke Test")
message("    DGPs: ", paste(dgp_ids, collapse=", "))
message("    Ests (bootstrap): ", paste(light_boot_ids, collapse=", "))
message("    Ests (native): ", paste(heavy_native_ids, collapse=", "))

# 4. Run Campaign
# ------------------------------------------------------------------------------
seeds <- 1:2
n <- 500
n_boot <- 20

results_boot <- NULL
results_native <- NULL

if (length(light_boot_ids) > 0) {
  infra <- mk_board("bootstrap")
  message(">>> Running bootstrap CI sub-campaign (n_boot=", n_boot, ")")
  results_boot <- cs_run_campaign(
    dgp_ids       = dgp_ids,
    estimator_ids = light_boot_ids,
    seeds         = seeds,
    n             = n,
    config        = list(
      ci_method   = "bootstrap",
      n_boot      = n_boot,
      num_threads = 1L
    ),
    board         = infra$board,
    staging_dir   = infra$staging_dir,
    parallel      = TRUE,
    show_progress = TRUE,
    skip_existing = FALSE
  )
}

if (length(heavy_native_ids) > 0) {
  infra <- mk_board("native")
  message(">>> Running native CI sub-campaign (package-provided)")
  results_native <- cs_run_campaign(
    dgp_ids       = dgp_ids,
    estimator_ids = heavy_native_ids,
    seeds         = seeds,
    n             = n,
    config        = list(
      ci_method   = "native",
      num_threads = 1L
    ),
    board         = infra$board,
    staging_dir   = infra$staging_dir,
    parallel      = TRUE,
    show_progress = TRUE,
    skip_existing = FALSE
  )
}

results <- dplyr::bind_rows(
  if (!is.null(results_boot)) results_boot else tibble::tibble(),
  if (!is.null(results_native)) results_native else tibble::tibble()
)

message("\n>>> Verifying Results Structure...")

# 5. Verification
# ------------------------------------------------------------------------------
tidy_res <- cs_tidy(results)

# A. Check CIs (All should have values, no NAs)
print("--- Point Estimates & CIs (Head) ---")
tidy_res %>% 
  select(dgp_id, estimator_id, seed, est_att, att_ci_lo, att_ci_hi) %>% 
  arrange(estimator_id, seed) %>% 
  print(n = 14)

# B. Provenance Check (The critical part)
print("--- Provenance Metadata (n_boot_ok) ---")
# Check if n_boot_ok is populated (should be >0 for bootstrap estimators; 0 for native)
results %>%
  select(estimator_id, seed, n_boot_ok) %>%
  arrange(estimator_id, seed) %>%
  print(n = 14)

message(">>> Test Complete. Check above for missing CIs or 0 boot successes.")


results %>%
  select(estimator_id, seed, run_time_dgp, log)


results %>%
  glimpse()
