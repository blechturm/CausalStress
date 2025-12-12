# Demonstration of parallel execution for cs_run_seeds with an expensive estimator (GenGC)
#
# Requires GenGC to be installed and CausalStress to be installed (workers load from library).

if (!requireNamespace("GenGC", quietly = TRUE)) {
  stop("GenGC is not installed; install it before running this demo.")
}

library(CausalStress)
library(future)

# Choose a backend. multisession works on most OSes.
plan(multisession, workers = 4)
on.exit(plan(sequential), add = TRUE)

seeds <- 1:4

message("Running serial (gengc)...")
t_serial <- system.time({
  serial <- cs_run_seeds(
    dgp_id        = "synth_qte1",
    estimator_id  = "gengc",
    n             = 800,
    seeds         = seeds,
    bootstrap     = TRUE,
    B             = 200,
    parallel      = FALSE,
    show_progress = FALSE
  )
})

message("Running parallel (gengc)...")
t_parallel <- system.time({
  parallel_res <- cs_run_seeds(
    dgp_id        = "synth_qte1",
    estimator_id  = "gengc",
    n             = 800,
    seeds         = seeds,
    bootstrap     = TRUE,
    B             = 200,
    parallel      = TRUE,    # <-- enable parallel mapping
    show_progress = TRUE
  )
})

message("\nTiming (elapsed, seconds):")
print(rbind(serial = t_serial["elapsed"], parallel = t_parallel["elapsed"]))

message("\nReproducibility check (est_att):")
print(all.equal(serial$est_att, parallel_res$est_att))

message("\nFirst few rows (parallel):")
print(head(parallel_res))
