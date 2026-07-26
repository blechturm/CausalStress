# p2_lane3_survivorship.R
# PASS 2 · Lane STATISTICS · SCOPE 4 (survivorship / failure denominators).
#
# Rebuilds the pass-1 DEFECTIVE probe. Pass-1 passed structured result-LISTS to
# cs_summarise_runs (which requires a flattened tibble) and errored, so it never
# actually tested denominator honesty. Here we register a deterministic
# partial-failure estimator, run it through cs_run_grid (which produces the
# flattened per-seed tibble cs_summarise_runs consumes), and inspect whether the
# summary exposes the failures or silently averages survivors.
#
# horizon.md l.535-542 DEFERS this to v0.3.0: "extend the public summaries ...
# to report planned/attempted/succeeded/failed/timeout/missing denominators
# instead of silent na.rm = TRUE means -- the survivorship gap identified in the
# 2026-07-20 external review."
Sys.setenv(R_USER_CACHE_DIR = file.path(tempdir(), "cs_audit_cache"))
suppressMessages(suppressWarnings(pkgload::load_all(".", quiet = TRUE)))
`%||%` <- function(a, b) if (is.null(a)) b else a

# Deterministic partial-failure estimator: errors on EVEN run seeds, valid lm
# g-computation on ODD seeds. The runner injects the run seed as config$seed
# (cs_run_single sets config_local$seed <- seed), so the failure is deterministic
# and reproducible across the grid.
est_flaky <- function(df, config = list(), tau = NULL) {
  s <- config$seed %||% NA_integer_
  if (!is.na(s) && s %% 2L == 0L) {
    stop("deterministic probe failure on even seed")
  }
  xcols <- grep("^X", names(df), value = TRUE)
  fit <- stats::lm(stats::reformulate(c("w", xcols), "y"), data = df)
  list(
    outputs = list(att = list(estimate = unname(coef(fit)["w"]),
                              estimand_target_id = "att")),
    meta = list(estimator_id = "est_flaky", version = "0", ci_type = "none")
  )
}

invisible(tryCatch(
  cs_register_estimator(estimator_id = "est_flaky", type = "gcomp",
                        generator = est_flaky, version = "0", supports_qst = FALSE),
  error = function(e) message("register: ", conditionMessage(e))
))

seeds <- 1:8   # 4 odd (succeed) + 4 even (fail)
grid <- cs_run_grid(dgp_ids = "synth_baseline", estimator_ids = "est_flaky",
                    n = 300, seeds = seeds, bootstrap = FALSE,
                    config = list(ci_method = "none"), show_progress = FALSE)

cat("=============== SCOPE 4: SURVIVORSHIP / FAILURE DENOMINATORS ===============\n")
cat(sprintf("planned seeds: %d\n", length(seeds)))
cat("per-seed success flags (from the flattened grid tibble):\n")
print(data.frame(seed = grid$seed, success = grid$success,
                 att_error = round(grid$att_error, 4),
                 att_ci_width = grid$att_ci_width))
n_success <- sum(isTRUE_vec <- vapply(grid$success, isTRUE, logical(1)))
cat(sprintf("attempted=%d  success=TRUE=%d  failed=%d\n",
            nrow(grid), n_success, nrow(grid) - n_success))

cat("\n--- cs_summarise_runs on a grid CONTAINING failures ---\n")
sm <- cs_summarise_runs(grid)
cat("summary columns: ", paste(names(sm), collapse = ", "), "\n")
print(as.data.frame(sm))

cat("\n--- Denominator honesty interrogation ---\n")
cat(sprintf("n_runs reported by summary:                 %d\n", sm$n_runs))
cat(sprintf("actual successful runs:                     %d\n", n_success))
cat("Does the summary expose a success/failure count column? ",
    any(grepl("success|fail|attempt|planned|n_ok|n_valid", names(sm), ignore.case = TRUE)), "\n")
cat(sprintf("mean_error value: %s   (NA => a single failed seed NA-poisons the mean; no na.rm on error cols)\n",
            format(sm$mean_error)))
cat(sprintf("mean_abs_error value: %s\n", format(sm$mean_abs_error)))
cat(sprintf("mean_att_covered value: %s   (uses na.rm=TRUE -> averages survivors)\n",
            format(sm$mean_att_covered)))
cat(sprintf("mean_att_ci_width value: %s   (uses na.rm=TRUE -> averages survivors)\n",
            format(sm$mean_att_ci_width)))

cat("\n--- Contrast: cs_summarise_runs on the SURVIVORS-ONLY subset ---\n")
survivors <- grid[vapply(grid$success, isTRUE, logical(1)), ]
sm_surv <- cs_summarise_runs(survivors)
cat(sprintf("survivors-only: n_runs=%d  mean_error=%.4f  mean_abs_error=%.4f\n",
            sm_surv$n_runs, sm_surv$mean_error, sm_surv$mean_abs_error))
cat("Observation: the full-grid summary and the survivors-only summary both report\n")
cat("a single n_runs with no failed-seed denominator; the reader cannot tell from\n")
cat("cs_summarise_runs alone that 4/8 seeds failed.\n")

cat("\nDone.\n")
