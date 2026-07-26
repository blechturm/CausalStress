# PASS 2 — Lane 1 (estimands) — Metric-regime enforcement check
#
# CENTRAL QUESTION (successor to pass-1's withdrawn aggregation finding):
# The docs steer users to QST (not mean-ATT) on the no-mean `synth_heavytail`
# DGP, but only in prose. `metric_invalid_for_regime` exists in the
# non-comparable-reason vocabulary (R/cs-contracts.R cs_non_comparable_reasons).
# Is "use QST here" ENFORCED (machine-readable), or PROSE-ONLY?
#
# EMPIRICAL claims exercised:
#  (1) Score a mean-targeting estimator (lm_att; att_hat = mean(y1_obs - y0_hat),
#      an L2 g-computation) via cs_run_single on synth_heavytail vs synth_baseline.
#      Inspect the ATT score-surface row on each. Is there ANY machine-readable
#      difference (score_status, non_comparable_reason, or any regime/difficulty
#      flag column), or is mean-ATT scored identically ("scored") on the no-mean
#      DGP as on the Gaussian baseline?
#  (2) Confirm `metric_invalid_for_regime` is NEVER emitted for heavytail ATT
#      (nor anywhere in the produced score surface across both DGPs), despite
#      being a declared vocabulary term.
#  (3) DEFINITIONAL support (empirical): the STRUCTURAL ATT/ATE truth is
#      well-posed (finite) on heavytail — true_att = mean(tau[w==1]),
#      tau = 1 + 0.5*X1 deterministic — while the mean-ESTIMATOR is not
#      (variance explosion across seeds). Both are nonetheless scored "scored".
#
# All randomness via CausalStress's mandated RNG through explicit seeds.
# Oracle cache redirected to tempdir() to respect the review write boundary.

Sys.setenv(R_USER_CACHE_DIR = file.path(tempdir(), "cs_audit_cache_p2l1"))
suppressMessages(pkgload::load_all(".", quiet = TRUE))

fail <- 0L
note <- function(ok, msg) {
  status <- if (isTRUE(ok)) "PASS" else "FAIL"
  if (!isTRUE(ok)) fail <<- fail + 1L
  cat(sprintf("[%s] %s\n", status, msg))
}

n <- 2000L
seed <- 202L

targets <- c("att", "ate", "qst", "cate")

run_ht <- cs_run_single(
  dgp_id = "synth_heavytail", estimator_id = "lm_att",
  n = n, seed = seed, quiet = TRUE,
  config = list(estimand_targets = targets, ci_method = "none")
)
run_bl <- cs_run_single(
  dgp_id = "synth_baseline", estimator_id = "lm_att",
  n = n, seed = seed, quiet = TRUE,
  config = list(estimand_targets = targets, ci_method = "none")
)

sc_ht <- run_ht$scores
sc_bl <- run_bl$scores

att_ht <- sc_ht[sc_ht$estimand_target_id == "att", ]
att_bl <- sc_bl[sc_bl$estimand_target_id == "att", ]

cat("== Part 1: ATT score-surface row, heavytail vs baseline (mean-targeting lm_att) ==\n")
cat("-- score-surface column names --\n")
cat(paste(names(sc_ht), collapse = ", "), "\n\n")

cat("-- heavytail ATT row (transposed) --\n")
print(t(att_ht))
cat("\n-- baseline ATT row (transposed) --\n")
print(t(att_bl))
cat("\n")

# (1) score_status identical and both "scored"
note(identical(att_ht$score_status, "scored"),
     sprintf("heavytail mean-ATT score_status == 'scored' (got '%s')", att_ht$score_status))
note(identical(att_bl$score_status, "scored"),
     sprintf("baseline  mean-ATT score_status == 'scored' (got '%s')", att_bl$score_status))
note(identical(att_ht$score_status, att_bl$score_status),
     "score_status IDENTICAL across no-mean heavytail and Gaussian baseline")

# non_comparable_reason NA on both (no regime flag)
note(is.na(att_ht$non_comparable_reason),
     sprintf("heavytail ATT non_comparable_reason is NA (got '%s')", att_ht$non_comparable_reason))
note(identical(att_ht$non_comparable_reason, att_bl$non_comparable_reason),
     "non_comparable_reason IDENTICAL (both NA) across heavytail and baseline")

# Is there ANY machine-readable regime/difficulty flag column at all?
regime_flag_cols <- grep("regime|difficult|stars|valid_for|moment|heavy|tail|robust",
                         names(sc_ht), ignore.case = TRUE, value = TRUE)
note(length(regime_flag_cols) == 0L,
     sprintf("score surface carries NO regime/difficulty flag column (found: %s)",
             if (length(regime_flag_cols) == 0L) "<none>" else paste(regime_flag_cols, collapse = ",")))

# Columns that differ between the two ATT rows: should be ONLY numeric value
# fields + identity/provenance (estimate/truth/error/abs_error/fingerprints/dgp),
# never a status/regime signal.
common <- intersect(names(att_ht), names(att_bl))
differing <- common[vapply(common, function(cc) {
  !identical(att_ht[[cc]], att_bl[[cc]])
}, logical(1))]
cat(sprintf("\nColumns differing between heavytail and baseline ATT rows:\n  %s\n",
            paste(differing, collapse = ", ")))
status_like <- intersect(differing, c("score_status", "non_comparable_reason", "truth_tier",
                                      "target_population", "scoring_population_id", "metric_id",
                                      "evaluation_policy", "target_level"))
note(length(status_like) == 0L,
     sprintf("NO status/semantic column differs between the two DGPs' ATT rows (differing semantic cols: %s)",
             if (length(status_like) == 0L) "<none>" else paste(status_like, collapse = ",")))

cat("\n== Part 2: metric_invalid_for_regime declared but never emitted ==\n")
note("metric_invalid_for_regime" %in% cs_non_comparable_reasons(),
     "metric_invalid_for_regime IS a declared non-comparable-reason vocabulary term")
emitted <- unique(c(sc_ht$non_comparable_reason, sc_bl$non_comparable_reason))
emitted <- emitted[!is.na(emitted)]
cat(sprintf("Reasons actually emitted across both full runs: %s\n",
            if (length(emitted) == 0L) "<none>" else paste(emitted, collapse = ", ")))
note(!("metric_invalid_for_regime" %in% emitted),
     "metric_invalid_for_regime is NEVER emitted for heavytail (nor baseline) in produced score surface")

cat("\n== Part 3: DEFINITIONAL — structural TRUTH well-posed vs mean-ESTIMATOR unstable ==\n")
seeds <- 1:20
nn <- 1000L
ht_true_att <- numeric(length(seeds))
ht_lm_err   <- numeric(length(seeds))
ht_or_err   <- numeric(length(seeds))
bl_lm_err   <- numeric(length(seeds))
for (i in seq_along(seeds)) {
  s <- seeds[i]
  r_lm <- cs_run_single("synth_heavytail", "lm_att", n = nn, seed = s, quiet = TRUE,
                        config = list(ci_method = "none"))
  r_or <- cs_run_single("synth_heavytail", "oracle_att", n = nn, seed = s, quiet = TRUE,
                        config = list(ci_method = "none"))
  r_bl <- cs_run_single("synth_baseline", "lm_att", n = nn, seed = s, quiet = TRUE,
                        config = list(ci_method = "none"))
  ht_true_att[i] <- r_lm$att$true
  ht_lm_err[i]   <- r_lm$att$error
  ht_or_err[i]   <- r_or$att$error
  bl_lm_err[i]   <- r_bl$att$error
}
cat(sprintf("heavytail true_att across 20 seeds: all finite = %s; range = [%.4f, %.4f]\n",
            all(is.finite(ht_true_att)), min(ht_true_att), max(ht_true_att)))
cat(sprintf("heavytail lm_att   |error|: median = %.3f, max = %.3f  (mean-estimator variance explosion)\n",
            median(abs(ht_lm_err)), max(abs(ht_lm_err))))
cat(sprintf("heavytail oracle_att |error|: max = %.3e  (structural truth layer stable)\n",
            max(abs(ht_or_err))))
cat(sprintf("baseline  lm_att   |error|: median = %.3f, max = %.3f  (Gaussian: well-behaved)\n",
            median(abs(bl_lm_err)), max(abs(bl_lm_err))))

note(all(is.finite(ht_true_att)),
     "STRUCTURAL ATT truth is FINITE on every heavytail seed (target well-posed)")
note(max(abs(ht_or_err)) < 1e-9,
     sprintf("oracle_att recovers structural truth ~exactly (max|err| = %.2e)", max(abs(ht_or_err))))
note(max(abs(ht_lm_err)) > 3 * max(abs(bl_lm_err)),
     sprintf("heavytail mean-ESTIMATOR error is far more extreme than baseline (ht max %.2f vs bl max %.2f)",
             max(abs(ht_lm_err)), max(abs(bl_lm_err))))

# truth availability: ATT and ATE both available (finite) on heavytail
d_ht <- dgp_synth_heavytail(n = nn, seed = 1L)
avail <- cs_truth_available_targets(d_ht)
cat(sprintf("\ncs_truth_available_targets(heavytail) = {%s}\n", paste(avail, collapse = ", ")))
note(all(c("att", "ate") %in% avail),
     "ATT and ATE truth are BOTH machine-available on heavytail (finite structural means) -> both scorable")

cat(sprintf("\n== RESULT: %s (%d failures) ==\n", if (fail == 0L) "ALL PASS" else "FAILURES", fail))
if (fail > 0L) quit(status = 1L)
