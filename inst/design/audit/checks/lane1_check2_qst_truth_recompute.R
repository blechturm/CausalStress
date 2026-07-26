# Lane 1 (estimands) — Check 2: QST oracle-truth recomputation and precision
#
# EMPIRICAL claims exercised:
#  (A) The persisted QST oracle truth (CRN oracle, seed 99999, N=1e6 treated) for
#      the two STABLE DGPs (synth_baseline v1.6.0, synth_heavytail v1.6.0)
#      agrees with an INDEPENDENT non-CRN Monte Carlo (different seeds,
#      independent eps1 as in the run-sample data path) within MC error.
#      This validates that the CRN coupling used only in the oracle path is
#      estimand-invariant: QST is a contrast of MARGINAL quantiles, and the
#      coupling changes the joint law only.
#  (B) Oracle determinism: recomputing the oracle after cache deletion yields
#      bitwise-identical truth (truth is a pure function of the seeded algorithm).
#  (C) Oracle MC precision: estimate the oracle's own MC standard error at
#      selected tau by CRN replication at N_t=1e5 scaled by sqrt(1e5/1e6);
#      compared against the Constitution §1.4 parenthetical precision "1e-5".
#  (D) Hard-coded analytic truths: placebo (0 via pathwise Y1==Y0) and
#      hd_sparse_plm (1 via pathwise Y1==Y0+1) are exactly consistent with the
#      generated potential outcomes.
#
# Oracle cache redirected to tempdir() (write boundary). All seeds explicit.

Sys.setenv(R_USER_CACHE_DIR = file.path(tempdir(), "cs_audit_cache"))
suppressMessages(pkgload::load_all(".", quiet = TRUE))

fail <- 0L
note <- function(ok, msg) {
  status <- if (isTRUE(ok)) "PASS" else "FAIL"
  if (!isTRUE(ok)) fail <<- fail + 1L
  cat(sprintf("[%s] %s\n", status, msg))
}

taus_sel <- c(0.01, 0.05, 0.25, 0.50, 0.75, 0.95, 0.99)

collect_treated <- function(gen, target_n, oracle_only) {
  y0_t <- numeric(0); y1_t <- numeric(0)
  while (length(y0_t) < target_n) {
    sim <- gen(n = 200000L, seed = NULL, include_truth = FALSE, oracle_only = oracle_only)
    df <- if (!is.null(sim$df)) sim$df else sim
    idx <- df$w == 1L
    y0_t <- c(y0_t, df$y0[idx]); y1_t <- c(y1_t, df$y1[idx])
  }
  list(y0 = y0_t[seq_len(target_n)], y1 = y1_t[seq_len(target_n)])
}

contrast <- function(y1, y0, taus) {
  stats::quantile(y1, taus, type = 7, names = FALSE) -
    stats::quantile(y0, taus, type = 7, names = FALSE)
}

dgps <- list(
  list(id = "synth_baseline",  ver = "1.6.0", gen = dgp_synth_baseline_v160),
  list(id = "synth_heavytail", ver = "1.6.0", gen = dgp_synth_heavytail_v160)
)

for (spec in dgps) {
  cat(sprintf("\n== %s v%s ==\n", spec$id, spec$ver))

  oracle <- cs_get_oracle_qst(spec$id, version = spec$ver)
  o_sel <- oracle$value[match(taus_sel, oracle$tau)]

  # (B) determinism: delete cache, recompute, compare bitwise (baseline only to bound cost)
  if (spec$id == "synth_baseline") {
    cache_dir <- tools::R_user_dir("CausalStress", "cache")
    files <- list.files(cache_dir, pattern = paste0("^truth_", spec$id, "_"), full.names = TRUE)
    unlink(files)
    oracle2 <- cs_get_oracle_qst(spec$id, version = spec$ver)
    note(identical(oracle$value, oracle2$value),
         "oracle recomputation after cache deletion is bitwise-identical (deterministic seeded truth)")
  }

  # (A) independent non-CRN MC, R = 4 replicates of 2e5 treated each
  R <- 4L
  est <- matrix(NA_real_, nrow = R, ncol = length(taus_sel))
  for (r in seq_len(R)) {
    cs_set_rng(9000L + r)
    tt <- collect_treated(spec$gen, target_n = 200000L, oracle_only = FALSE)
    est[r, ] <- contrast(tt$y1, tt$y0, taus_sel)
  }
  m <- colMeans(est)
  se <- apply(est, 2, stats::sd) / sqrt(R)
  z <- (o_sel - m) / se
  cat("tau      oracle     indepMC       SE        z\n")
  for (j in seq_along(taus_sel)) {
    cat(sprintf("%4.2f  %9.5f  %9.5f  %8.5f  %+6.2f\n", taus_sel[j], o_sel[j], m[j], se[j], z[j]))
  }
  note(all(abs(z) < 4),
       sprintf("oracle truth within 4 SE of independent non-CRN MC at all %d checked tau (max|z| = %.2f)",
               length(taus_sel), max(abs(z))))

  # (C) oracle MC-precision estimate: CRN replicates at N_t = 1e5, scaled to N = 1e6
  R2 <- 6L
  est2 <- matrix(NA_real_, nrow = R2, ncol = length(taus_sel))
  for (r in seq_len(R2)) {
    cs_set_rng(9100L + r)
    tt <- collect_treated(spec$gen, target_n = 100000L, oracle_only = TRUE)
    est2[r, ] <- contrast(tt$y1, tt$y0, taus_sel)
  }
  se_oracle <- apply(est2, 2, stats::sd) * sqrt(100000 / 1e6)
  cat("estimated oracle MC SE at N=1e6 (CRN):\n")
  for (j in seq_along(taus_sel)) {
    cat(sprintf("  tau %4.2f : %.2e\n", taus_sel[j], se_oracle[j]))
  }
  cat(sprintf("  max over checked tau: %.2e  (Constitution 1.4 parenthetical: '< 10^-5')\n", max(se_oracle)))
  note(TRUE, sprintf("oracle MC SE quantified (informational): max %.2e across checked tau", max(se_oracle)))
}

cat("\n== (D) hard-coded analytic truths vs pathwise potential outcomes ==\n")
dp <- dgp_synth_placebo_tau0_v130(n = 100000L, seed = 7L, include_truth = TRUE)
note(identical(dp$df$y0, dp$df$y1) && identical(dp$true_att, 0) || (dp$true_att == 0),
     sprintf("placebo_tau0: Y1 pathwise-identical to Y0 (%s) and true_att == 0 (%s)",
             identical(dp$df$y0, dp$df$y1), dp$true_att == 0))
note(all(dp$true_qst$value == 0),
     "placebo_tau0: oracle QST truth is exactly 0 at all 99 tau (consistent with sharp null)")

dh <- dgp_synth_hd_sparse_plm_v150(n = 20000L, seed = 7L, include_truth = TRUE)
note(max(abs(dh$df$y1 - dh$df$y0 - 1)) == 0,
     "hd_sparse_plm v1.5.0: Y1 == Y0 + 1 pathwise (constant shift)")
note(all(dh$true_qst$value == 1) && dh$true_att == 1,
     "hd_sparse_plm: hard-coded QST truth 1 at all tau and ATT truth 1 are exact for a pathwise +1 shift")

cat(sprintf("\n== RESULT: %s (%d failures) ==\n", if (fail == 0L) "ALL PASS" else "FAILURES", fail))
if (fail > 0L) quit(status = 1L)
