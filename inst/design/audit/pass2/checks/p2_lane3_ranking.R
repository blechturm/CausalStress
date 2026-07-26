# p2_lane3_ranking.R
# PASS 2 · Lane STATISTICS · SCOPE 2 (ranking stability on FINITE-VARIANCE DGPs).
#
# CORRECTLY SCOPED per triage INTENT-FIRST rule: a mean-based ranking is only
# well-posed where a population mean exists. synth_baseline (Gaussian noise) is
# finite-variance -> mean IS well-posed -> ranking SHOULD be stable across
# disjoint seed sets. synth_heavytail (0.8 N + 0.2 Cauchy) has NO finite mean;
# its ranking instability is the DGP's INTENDED "L2 break" demonstration
# (heavytail.Rmd s2/s4: "any L2/MSE estimator is not well-posed"; triage l18-22
# "the instability is the correct result") -- reported here as EXPECTED, not a
# defect.
Sys.setenv(R_USER_CACHE_DIR = file.path(tempdir(), "cs_audit_cache"))
suppressMessages(suppressWarnings(pkgload::load_all(".", quiet = TRUE)))

mae_by <- function(seeds) {
  out <- list()
  for (dg in c("synth_baseline", "synth_heavytail")) {
    for (es in c("lm_att", "ipw_att")) {
      r <- cs_run_seeds(dgp_id = dg, estimator_id = es, n = 400, seeds = seeds,
                        bootstrap = FALSE, show_progress = FALSE, quiet = TRUE,
                        config = list(ci_method = "none"))
      out[[paste(dg, es)]] <- mean(r$att_abs_error, na.rm = TRUE)
    }
  }
  unlist(out)
}

A <- mae_by(1:15)      # seed set A
B <- mae_by(101:115)   # disjoint seed set B

cat("=============== SCOPE 2: RANKING STABILITY (disjoint seed sets) ===============\n")
cat("mean|ATT error| -- seed set A (1:15):\n");   print(round(A, 4))
cat("mean|ATT error| -- seed set B (101:115):\n"); print(round(B, 4))

cat("\nWithin-DGP ordering of lm_att vs ipw_att, A vs B:\n")
for (dg in c("synth_baseline", "synth_heavytail")) {
  a_lm <- A[paste(dg, "lm_att")]; a_ip <- A[paste(dg, "ipw_att")]
  b_lm <- B[paste(dg, "lm_att")]; b_ip <- B[paste(dg, "ipw_att")]
  ord_a <- if (a_lm < a_ip) "lm<ipw" else "ipw<lm"
  ord_b <- if (b_lm < b_ip) "lm<ipw" else "ipw<lm"
  stable <- (a_lm < a_ip) == (b_lm < b_ip)
  tag <- if (dg == "synth_baseline") "[finite variance: mean well-posed]" else
         "[NO finite mean: instability is INTENDED L2 break]"
  cat(sprintf("  %-16s A:%-7s B:%-7s ordering_stable=%-5s  %s\n",
              dg, ord_a, ord_b, stable, tag))
}
cat("\nInterpretation: stability on synth_baseline is the meaningful check (a mean\n")
cat("exists there). Any flip on synth_heavytail is the designed demonstration and\n")
cat("must NOT be filed as an aggregation defect (pass-1's withdrawn error).\n")
cat("\nDone.\n")
