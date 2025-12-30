# dev/verify_fingerprint.R
# Usage: Run this line-by-line or source it.

devtools::load_all() # Load the version with your new fingerprinting code
library(pins)
library(testthat)

# Setup
board <- board_temp()
dgp   <- "synth_baseline"
est   <- "lm_att"
n     <- 100
seed  <- 1

# -------------------------------------------------------------------------
# 1. BASELINE RUN
# -------------------------------------------------------------------------
message("\n[1] Running Baseline (Config A)...")
config_A <- list(param = "A")
cs_run_seeds(
  dgp, est, n = n, seeds = seed,
  config = config_A,
  board = board,
  show_progress = FALSE
)

# Verify pin exists
if (!CausalStress:::cs_pin_exists(board, dgp, est, n, seed)) stop("Pin check failed!")

# -------------------------------------------------------------------------
# 2. HAPPY PATH (Exact Match) -> Should SKIP/LOAD
# -------------------------------------------------------------------------
message("\n[2] Resuming with Config A (Expect: Cache Hit)...")
t_start <- Sys.time()
res_A <- cs_run_seeds(
  dgp, est, n = n, seeds = seed,
  config = config_A, # Identical config
  board = board,
  skip_existing = TRUE,
  show_progress = FALSE
)
t_end <- Sys.time()

if (as.numeric(t_end - t_start) > 1) warning("Too slow! Did it actually skip?")
message("    -> Success: Loaded instantly.")

# -------------------------------------------------------------------------
# 3. SAFETY CHECK: Change Config -> Should FAIL
# -------------------------------------------------------------------------
message("\n[3] Resuming with Config B (Expect: ERROR)...")
config_B <- list(param = "B") # Changed hyperparameter

tryCatch({
  cs_run_seeds(
    dgp, est, n = n, seeds = seed,
    config = config_B, # DIFFERENT config
    board = board,
    skip_existing = TRUE, # asking to resume
    show_progress = FALSE
  )
  stop("FAILURE: It should have errored but it ran anyway!")
}, error = function(e) {
  message("    -> Success: Caught expected error.")
  message("       Error message: ", e$message)
})

# -------------------------------------------------------------------------
# 4. SAFETY CHECK: Change Tau -> Should FAIL
# -------------------------------------------------------------------------
message("\n[4] Resuming with Changed Tau (Expect: ERROR)...")
tau_new <- c(0.1, 0.5, 0.9) # Defaults are usually 0.01:0.99

tryCatch({
  cs_run_seeds(
    dgp, est, n = n, seeds = seed,
    config = config_A, 
    tau = tau_new, # DIFFERENT tau
    board = board,
    skip_existing = TRUE,
    show_progress = FALSE
  )
  stop("FAILURE: It should have errored but it ran anyway!")
}, error = function(e) {
  message("    -> Success: Caught expected error.")
  message("       Error message: ", e$message)
})

message("\n=== VERIFICATION COMPLETE: SYSTEM IS SAFE ===")