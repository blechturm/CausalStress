# ==============================================================================
# SMOKE TEST: Mixed CI Strategies (defaults + per-estimator overrides)
# ==============================================================================
# This script verifies that a single `cs_run_campaign()` call can run a mixed
# CI strategy across estimators using:
#   - `defaults`  (global estimator config)
#   - `overrides` (per-estimator config overrides)
#
# It runs two campaigns:
#   A) Explicit overrides: bootstrap for lightweights, native for heavyweights.
#   B) Defaults only: relies on each estimator's default CI behavior (should
#      still produce CIs without overrides).
#
# Run from project root (recommended). Optional heavy estimators require their
# packages installed (grf, tmle, SuperLearner, bartCause, GenGC).

# In a fresh R session, you can reinstall to ensure you're testing the latest local code.
# Comment these out if you prefer `devtools::load_all()` workflows.
remove.packages("CausalStress")
unlink(file.path(.libPaths()[1], "CausalStress"), recursive = TRUE, force = TRUE)
devtools::install(upgrade = "never")

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

# 1) Estimator selection (only include those actually available)
candidates <- c(
  "lm_att", "ipw_att",
  "gengc", "gengc_dr",
  "grf_dr_att",
  "tmle_att",
  "bart_att"
)

estimator_ids <- candidates[vapply(
  candidates,
  function(id) {
    ok <- tryCatch({ cs_get_estimator(id); TRUE }, error = function(...) FALSE)
    isTRUE(ok)
  },
  logical(1)
)]

light_boot_ids <- intersect(estimator_ids, c("lm_att", "ipw_att", "gengc", "gengc_dr"))
heavy_native_ids <- intersect(estimator_ids, c("grf_dr_att", "tmle_att", "bart_att"))

dgp_ids <- "synth_baseline"
seeds <- 1:2
n <- 500
n_boot <- 20

plan(multisession, workers = 4)
on.exit(plan(sequential), add = TRUE)

message(">>> Estimators available: ", paste(estimator_ids, collapse = ", "))
message(">>> Light (bootstrap): ", paste(light_boot_ids, collapse = ", "))
message(">>> Heavy (native): ", paste(heavy_native_ids, collapse = ", "))

# Use separate boards so results can't mix between A and B.
mk_board <- function(suffix) {
  board_path <- normalizePath(file.path("_experiments", paste0("pins_board_ci_", suffix)), mustWork = FALSE)
  staging_dir <- normalizePath(file.path("_experiments", paste0("staging_ci_", suffix)), mustWork = FALSE)
  dir.create(board_path, recursive = TRUE, showWarnings = FALSE)
  dir.create(staging_dir, recursive = TRUE, showWarnings = FALSE)
  list(
    board = pins::board_folder(board_path, versioned = TRUE),
    staging_dir = staging_dir
  )
}

# ------------------------------------------------------------------------------
# Campaign A: Explicit overrides (mixed-method)
# ------------------------------------------------------------------------------
infra_a <- mk_board("strategy_map")

overrides <- c(
  setNames(rep(list(list(ci_method = "bootstrap")), length(light_boot_ids)), light_boot_ids),
  setNames(rep(list(list(ci_method = "native")), length(heavy_native_ids)), heavy_native_ids)
)

message(">>> Campaign A: Overrides (mixed bootstrap/native)")
res_a <- cs_run_campaign(
  dgp_ids       = dgp_ids,
  estimator_ids = estimator_ids,
  seeds         = seeds,
  n             = n,
  defaults      = list(
    n_boot      = n_boot,
    num_threads = 1L
  ),
  overrides     = overrides,
  board         = infra_a$board,
  staging_dir   = infra_a$staging_dir,
  parallel      = TRUE,
  experimental_parallel = TRUE,
  show_progress = TRUE,
  skip_existing = FALSE
)

tidy_a <- cs_tidy(res_a)

cat("\n--- Campaign A: estimates + CI ---\n")
print(
  tidy_a %>%
    select(
      dgp_id, estimator_id, seed,
      est_att, att_ci_lo, att_ci_hi,
      att_ci_method, att_ci_type, att_ci_level,
      att_ci_valid, att_ci_fail_code,
      n_boot_ok, log
    ) %>%
    arrange(estimator_id, seed),
  n = Inf
)

# ------------------------------------------------------------------------------
# Campaign B: Defaults only (no overrides)
# ------------------------------------------------------------------------------
infra_b <- mk_board("defaults")

message(">>> Campaign B: Defaults only (estimator defaults)")
res_b <- cs_run_campaign(
  dgp_ids       = dgp_ids,
  estimator_ids = estimator_ids,
  seeds         = seeds,
  n             = n,
  defaults      = list(
    n_boot      = n_boot,
    num_threads = 1L
  ),
  board         = infra_b$board,
  staging_dir   = infra_b$staging_dir,
  parallel      = TRUE,
  experimental_parallel = TRUE,
  show_progress = TRUE,
  skip_existing = FALSE
)

tidy_b <- cs_tidy(res_b)

cat("\n--- Campaign B: estimates + CI (defaults) ---\n")
print(
  tidy_b %>%
    select(
      dgp_id, estimator_id, seed,
      est_att, att_ci_lo, att_ci_hi,
      att_ci_method, att_ci_type, att_ci_level,
      att_ci_valid, att_ci_fail_code,
      n_boot_ok, log
    ) %>%
    arrange(estimator_id, seed),
  n = Inf
)

message("\n>>> Done.")

# ------------------------------------------------------------------------------
# QST CI smoke check (GenGC only)
# ------------------------------------------------------------------------------
# For GenGC, bootstrap CI should cover both ATT and the full QST curve using the
# same refit-bootstrap fits. This checks that qst_ci_lo/qst_ci_hi are present.
if ("gengc" %in% estimator_ids) {
  qst_a <- cs_collect_qst(res_a) %>%
    dplyr::filter(.data$estimator_id == "gengc") %>%
    dplyr::filter(.data$tau_id %in% c("0.1", "0.9")) %>%
    dplyr::select(
      dgp_id, estimator_id, seed, tau_id, tau,
      estimate,
      qst_ci_lo, qst_ci_hi,
      qst_n_boot_ok, qst_ci_method, qst_ci_fail_code
    ) %>%
    dplyr::arrange(.data$seed, .data$tau_id)

  cat("\n--- GenGC QST (tau_id=0.1,0.9): estimate + CI ---\n")
  print(qst_a, n = Inf)

  if (nrow(qst_a) > 0) {
    ok <- is.finite(qst_a$qst_ci_lo) & is.finite(qst_a$qst_ci_hi) & (qst_a$qst_ci_lo <= qst_a$qst_ci_hi)
    if (!all(ok)) stop("GenGC QST CI smoke check failed: invalid bounds.")
  }
}

tidy_b %>% glimpse()
tidy_a %>% glimpse()

# ==============================================================================
# v0.1.8 Showcase: Patch-Only Reproducibility & Audit Features
# ==============================================================================
cat("\n\n=== v0.1.8 Showcase ===\n")

# 1) Experimental parallel gating + "warn once per call"
cat("\n[1] Parallel gating\n")
cat("Attempting parallel run without experimental_parallel (should error)...\n")
print(try(cs_run_seeds(
  dgp_id = "synth_baseline",
  estimator_id = "lm_att",
  n = 50,
  seeds = 1:1,
  parallel = TRUE,
  show_progress = FALSE,
  quiet = TRUE
), silent = TRUE))

cat("\nRunning experimental parallel (should warn once per call)...\n")
warning_count <- 0L
board_v018 <- pins::board_temp()
staging_v018 <- file.path(tempdir(), "cs_stage_v018_showcase")
dir.create(staging_v018, recursive = TRUE, showWarnings = FALSE)
withCallingHandlers(
  cs_run_seeds(
    dgp_id = "synth_baseline",
    estimator_id = "lm_att",
    n = 50,
    seeds = 1:1,
    board = board_v018,
    staging_dir = staging_v018,
    parallel = TRUE,
    experimental_parallel = TRUE,
    config = list(num_threads = 8L),
    show_progress = FALSE,
    quiet = TRUE
  ),
  warning = function(w) {
    if (inherits(w, "causalstress_experimental_parallel")) warning_count <<- warning_count + 1L
    invokeRestart("muffleWarning")
  }
)
cat("Experimental-parallel warnings seen (expected 1): ", warning_count, "\n", sep = "")

# 2) Provenance vs science payload accessors + fingerprint schema (incl. max_runtime)
cat("\n[2] Science payload vs provenance\n")
pin_name <- "results__dgp=synth_baseline__est=lm_att__n=50__seed=1"
obj <- pins::pin_read(board_v018, pin_name)

cat("meta$config_fingerprint_schema: ", obj$meta$config_fingerprint_schema, "\n", sep = "")
cat("provenance$max_runtime: ", obj$provenance$max_runtime, "\n", sep = "")
cat("provenance$experimental_parallel: ", obj$provenance$experimental_parallel, "\n", sep = "")
cat("provenance$parallel_backend: ", obj$provenance$parallel_backend, "\n", sep = "")
cat("provenance$requested_num_threads: ", obj$provenance$requested_num_threads, "\n", sep = "")
cat("provenance$effective_num_threads: ", obj$provenance$effective_num_threads, "\n", sep = "")

payload <- cs_science_payload(obj)
prov <- cs_provenance(obj)
flat <- cs_meta_flatten(obj)
cat("payload contains provenance? ", "provenance" %in% names(payload), "\n", sep = "")
cat("provenance contains max_runtime? ", isTRUE("max_runtime" %in% names(prov)), "\n", sep = "")
cat("flat contains max_runtime? ", isTRUE("max_runtime" %in% names(flat)), "\n", sep = "")

# 3) Fingerprint safety: max_runtime participates in identity
cat("\n[3] Fingerprint safety (max_runtime participates)\n")
cat("Attempting resume with different max_runtime (should fingerprint-mismatch error)...\n")
print(try(cs_run_seeds(
  dgp_id = "synth_baseline",
  estimator_id = "lm_att",
  n = 50,
  seeds = 1:1,
  board = board_v018,
  skip_existing = TRUE,
  max_runtime = 0.5,
  show_progress = FALSE,
  quiet = TRUE
), silent = TRUE))

# 4) DGP executable meta mapping (runner-side; no DGP generator changes)
cat("\n[4] DGP executable meta mapping\n")
dgp_desc <- cs_get_dgp("synth_baseline", quiet = TRUE)
print(cs_dgp_executable_meta("synth_baseline", dgp_desc$version))
