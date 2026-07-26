# Lane 1 (estimands) — Check 3: QST tier-asymmetry (finite-sample noise floor)
#
# QST truth is superpopulation-level (oracle over the law of (Y0,Y1) | W=1),
# while the estimator's curve is computed on the realized run sample of size n.
# EMPIRICAL question: at benchmark n, how large is the error of the INFEASIBLE
# sample oracle — the quantile contrast computed directly from the realized
# treated units' TRUE potential outcomes (both arms, no estimation) — against
# the frozen superpopulation truth? No feasible estimator has more information
# than this sample oracle, so its error is a floor proxy for the irreducible
# sampling component of measured QST "error" at that n.
#
# DGPs: the two stable ones (synth_baseline v1.6.0, synth_heavytail v1.6.0).
# n in {500, 2000}; 40 seeds each. Also reports the across-seed SD of the
# finite-sample ATT truth (per-run treated mean of structural_te), documenting
# why the per-run finite-sample ATT convention matters.
#
# Oracle cache redirected to tempdir() (write boundary). All seeds explicit.

Sys.setenv(R_USER_CACHE_DIR = file.path(tempdir(), "cs_audit_cache"))
suppressMessages(pkgload::load_all(".", quiet = TRUE))

taus_rep <- c(0.01, 0.05, 0.25, 0.50, 0.75, 0.95, 0.99)
grid <- cs_tau_oracle
seeds <- 301:340

dgps <- list(
  list(id = "synth_baseline",  ver = "1.6.0", gen = dgp_synth_baseline_v160),
  list(id = "synth_heavytail", ver = "1.6.0", gen = dgp_synth_heavytail_v160)
)

for (spec in dgps) {
  oracle <- cs_get_oracle_qst(spec$id, version = spec$ver)
  truth <- oracle$value
  cat(sprintf("\n== %s v%s : |truth| at reported tau: %s ==\n", spec$id, spec$ver,
              paste(sprintf("%.3f", truth[match(taus_rep, oracle$tau)]), collapse = " ")))

  for (n in c(500L, 2000L)) {
    abs_err <- matrix(NA_real_, nrow = length(seeds), ncol = length(grid))
    att_fs <- numeric(length(seeds))
    n_treated <- integer(length(seeds))
    for (i in seq_along(seeds)) {
      d <- spec$gen(n = n, seed = seeds[i], include_truth = FALSE)
      tr <- d$df$w == 1
      n_treated[i] <- sum(tr)
      q1 <- stats::quantile(d$df$y1[tr], grid, type = 7, names = FALSE)
      q0 <- stats::quantile(d$df$y0[tr], grid, type = 7, names = FALSE)
      abs_err[i, ] <- abs((q1 - q0) - truth)
      att_fs[i] <- mean(d$df$structural_te[tr])
    }
    sel <- match(taus_rep, grid)
    mean_ae <- colMeans(abs_err)
    med_ae <- apply(abs_err, 2, stats::median)
    cat(sprintf("\n-- n = %d (mean treated %.0f) : sample-oracle abs error vs frozen truth --\n",
                n, mean(n_treated)))
    cat("tau     mean|err|  median|err|\n")
    for (j in sel) {
      cat(sprintf("%4.2f   %9.4f   %9.4f\n", grid[j], mean_ae[j], med_ae[j]))
    }
    cat(sprintf("grid-average mean|err| = %.4f ; grid-average median|err| = %.4f\n",
                mean(mean_ae), mean(med_ae)))
    cat(sprintf("finite-sample ATT truth across seeds: mean %.4f, SD %.4f (per-run truth variation)\n",
                mean(att_fs), stats::sd(att_fs)))
  }
}

cat("\n[INFO] Interpretation: values above are the irreducible sampling floor proxy\n")
cat("[INFO] for QST scoring at each n; estimator abs_error at these n cannot be\n")
cat("[INFO] expected to fall below this floor, and tail rows (0.01/0.99) of\n")
cat("[INFO] heavy-tailed DGPs are floor-dominated at benchmark n.\n")
cat("\n== RESULT: informational quantification (no pass/fail thresholds) ==\n")
