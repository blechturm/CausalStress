# in a fresh R session
remove.packages("CausalStress")
unlink(file.path(.libPaths()[1], "CausalStress"), recursive = TRUE, force = TRUE)

# reinstall
devtools::install(upgrade = "never")


# Demo: Parallel + Stage & Gather persistence
# Requirements: CausalStress installed, furrr/future, qs, pins available.

library(CausalStress)
library(future)
library(pins)


# If you have RhpcBLASctl: RhpcBLASctl::blas_set_num_threads(1)


# Configure future backend (user controls the plan)
plan(multisession, workers = 20)
on.exit(plan(sequential), add = TRUE)

board <- board_temp()
staging_dir <- file.path(tempdir(), "cs_stage_demo")
seeds <- 1:10

message("Recovering any staged files from prior runs...")
dir.create(staging_dir, recursive = TRUE, showWarnings = FALSE)
cs_gather_results(board, staging_dir)

message("Running in parallel with staging enabled...")
t_parallel <- system.time({
  res <- cs_run_seeds(
    dgp_id        = "synth_baseline",
    estimator_id  = "gengc",
    n             = 2000,
    seeds         = seeds,
    bootstrap     = TRUE,
    B             = 100,
    parallel      = TRUE,
    staging_dir   = staging_dir,
    board         = board,
    show_progress = TRUE
  )
})

message("Flushing staged files to board...")
gathered <- cs_gather_results(board, staging_dir)

message("\nTiming (elapsed seconds):")
print(t_parallel["elapsed"])

message("\nGathered files: ", gathered)
message("Pins stored: ", length(pin_list(board)))

message("\nFirst few rows (parallel run):")
print(head(res))



