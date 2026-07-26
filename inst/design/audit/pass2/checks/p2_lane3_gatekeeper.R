# p2_lane3_gatekeeper.R
# PASS 2 · Lane STATISTICS · SCOPE 5 (gatekeeper).
# Confirm cs_summarise_gatekeeper (R/cs-gatekeeper.R):
#   (i)  a correct, conservative estimator with valid CIs PASSES the ATT gate
#        (coverage >= threshold) on a placebo DGP;
#   (ii) a CI-less estimator maps to UNVERIFIED (n_verified == 0 gate,
#        cs-gatekeeper.R l.40-54) rather than silently PASS/FAIL or crashing;
#   (iii) the QST 10/10-rule branch (cs-gatekeeper.R l.128-158) is present.
Sys.setenv(R_USER_CACHE_DIR = file.path(tempdir(), "cs_audit_cache"))
suppressMessages(suppressWarnings(pkgload::load_all(".", quiet = TRUE)))

cat("=============== SCOPE 5: GATEKEEPER (Article IV) ===============\n")

# (i) Conservative estimator WITH bootstrap CIs on a placebo DGP -> expect PASS.
cat("\n--- (i) conservative estimator (lm_att + bootstrap CIs) on placebo ---\n")
consv <- cs_run_grid(dgp_ids = "synth_placebo_tau0", estimator_ids = "lm_att",
                     n = 300, seeds = 1:12, bootstrap = TRUE, B = 120,
                     show_progress = FALSE)
gk_consv <- suppressMessages(cs_summarise_gatekeeper(consv, threshold = 0.90))
cat("ATT verdict:\n"); print(as.data.frame(gk_consv$att_verdict))

# (ii) CI-less estimator -> att_covered all NA -> n_verified == 0 -> UNVERIFIED.
cat("\n--- (ii) CI-less estimator (lm_att, ci_method='none') on placebo ---\n")
ciless <- cs_run_grid(dgp_ids = "synth_placebo_tau0", estimator_ids = "lm_att",
                      n = 300, seeds = 1:12, bootstrap = FALSE,
                      config = list(ci_method = "none"), show_progress = FALSE)
gk_ciless <- suppressMessages(cs_summarise_gatekeeper(ciless, threshold = 0.90))
cat("att_covered all NA in CI-less runs? ", all(is.na(ciless$att_covered)), "\n")
cat("ATT verdict:\n"); print(as.data.frame(gk_ciless$att_verdict))

cat("\n--- assertions ---\n")
pass_ok    <- identical(gk_consv$att_verdict$status[[1]], "PASS")
unver_ok   <- identical(gk_ciless$att_verdict$status[[1]], "UNVERIFIED")
nverif0_ok <- gk_ciless$att_verdict$n_verified[[1]] == 0L
cat(sprintf("conservative estimator PASSES ATT gate:            %s (coverage=%.3f)\n",
            pass_ok, gk_consv$att_verdict$coverage_rate[[1]]))
cat(sprintf("CI-less estimator -> UNVERIFIED (not PASS/crash):   %s\n", unver_ok))
cat(sprintf("CI-less n_verified == 0 (the l.40-54 gate fired):   %s\n", nverif0_ok))

# (iii) QST 10/10 branch present in source.
src <- readLines("R/cs-gatekeeper.R", warn = FALSE)
cat(sprintf("QST 10/10-rule branch present in cs-gatekeeper.R:   %s\n",
            any(grepl("10/10 rule|null_rejection_rate|run_fail_rate", src))))

cat("\nDone.\n")
