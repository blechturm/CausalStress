# p2_lane3_metric_regime.R
# PASS 2 · Lane STATISTICS · SCOPE 1 (metric-regime enforcement) + SCOPE 3
# (aggregate-uncertainty column inventory).
#
# Question 1: Does the tool treat mean-based scoring (mean_error/RMSE) of ATT on
#   a NO-MEAN DGP (synth_heavytail, Cauchy mixture) any differently than on a
#   finite-variance DGP (synth_baseline)? Is there a warning, a
#   non_comparable_reason == "metric_invalid_for_regime", or a regime flag, or is
#   mean_error reported identically as if valid?
# Question 3: What uncertainty signal does cs_summarise_runs expose -- an SEM/CI/
#   rank-stability signal, or only dispersion (sd_error)?
#
# INTENT NOTE: the heavytail ATT ranking/RMSE instability is the DGP's INTENDED
# "L2 break" demonstration (heavytail.Rmd s2/s4). This check is NOT about whether
# instability occurs; it is about whether the SCORING MACHINERY flags the regime.
Sys.setenv(R_USER_CACHE_DIR = file.path(tempdir(), "cs_audit_cache"))
suppressMessages(suppressWarnings(pkgload::load_all(".", quiet = TRUE)))
`%||%` <- function(a, b) if (is.null(a)) b else a

run_capture_warnings <- function(dgp) {
  w <- character(0)
  res <- withCallingHandlers(
    cs_run_seeds(
      dgp_id        = dgp,
      estimator_id  = "lm_att",       # a MEAN-targeting ATT estimator (L2 g-comp)
      n             = 800,
      seeds         = 1:20,
      bootstrap     = FALSE,
      show_progress = FALSE,
      quiet         = TRUE,
      config        = list(ci_method = "none")
    ),
    warning = function(cond) {
      w <<- c(w, conditionMessage(cond)); invokeRestart("muffleWarning")
    }
  )
  list(res = res, warnings = w)
}

cat("=============== SCOPE 1: METRIC-REGIME ENFORCEMENT ===============\n")
cat("Reason vocabulary includes 'metric_invalid_for_regime'? ",
    "metric_invalid_for_regime" %in% cs_non_comparable_reasons(), "\n\n")

for (dg in c("synth_baseline", "synth_heavytail")) {
  cat("---------------- DGP:", dg, "----------------\n")
  out <- run_capture_warnings(dg)
  res <- out$res

  cat("regime-related warnings emitted during scoring: ",
      if (length(out$warnings) == 0) "<NONE>" else paste(out$warnings, collapse = " | "), "\n")

  # (a) summariser: is mean_error / RMSE reported as a plain number, no regime flag?
  sm <- cs_summarise_runs(res)
  rmse <- sqrt(mean(res$att_error^2, na.rm = TRUE))
  cat(sprintf("cs_summarise_runs: n_runs=%d  mean_error=%.4f  sd_error=%.4f  max_abs_error=%.4f\n",
              sm$n_runs, sm$mean_error, sm$sd_error, sm$max_abs_error))
  cat(sprintf("empirical RMSE(att_error) = %.4f\n", rmse))
  cat("summariser columns: ", paste(names(sm), collapse = ", "), "\n")
  has_regime_col <- any(grepl("regime|metric_valid|moment", names(sm), ignore.case = TRUE))
  cat("summariser carries any regime / moment / metric-validity column? ", has_regime_col, "\n")

  # (b) score surface: are heavytail ATT rows 'scored', or non_comparable w/ regime reason?
  sc <- cs_collect_scores(res)
  att_rows <- sc[sc$estimand_target_id == "att", ]
  cat("score_status tally (att rows):\n"); print(table(att_rows$score_status, useNA = "always"))
  cat("non_comparable_reason tally (att rows):\n")
  print(table(att_rows$non_comparable_reason, useNA = "always"))
  cat("count of rows with reason == 'metric_invalid_for_regime': ",
      sum(att_rows$non_comparable_reason == "metric_invalid_for_regime", na.rm = TRUE), "\n")

  # (c) passive descriptive flag exists in provenance but is not a scoring gate
  one <- cs_run_single(dgp_id = dg, estimator_id = "lm_att", n = 400, seed = 1,
                       quiet = TRUE, config = list(ci_method = "none"))
  cat("provenance$dgp_noise_family (descriptive only, NOT a scoring gate): ",
      one$provenance$dgp_noise_family %||% "<none>", "\n\n")
}

cat("=============== SCOPE 3: AGGREGATE-UNCERTAINTY COLUMN INVENTORY ===============\n")
sm_hint <- cs_summarise_runs(
  cs_run_seeds("synth_baseline", "lm_att", n = 400, seeds = 1:10,
               show_progress = FALSE, quiet = TRUE, config = list(ci_method = "none"))
)
cols <- names(sm_hint)
cat("cs_summarise_runs output columns:\n  ", paste(cols, collapse = ", "), "\n")
cat("dispersion column present (sd_error)?           ", "sd_error" %in% cols, "\n")
cat("standard-error-of-the-mean column present?      ",
    any(grepl("se_|sem|std_err|se_mean|mcse", cols, ignore.case = TRUE)), "\n")
cat("confidence-interval-of-the-mean columns?        ",
    any(grepl("ci_lo|ci_hi|lower|upper|conf", cols, ignore.case = TRUE)), "\n")
cat("rank-stability / cross-seed-stability column?   ",
    any(grepl("rank|stability|spearman", cols, ignore.case = TRUE)), "\n")
cat("Note: SEM would be sd_error/sqrt(n_runs) =",
    sprintf("%.4f", sm_hint$sd_error / sqrt(sm_hint$n_runs)),
    "but is NOT reported by the summariser.\n")

cat("\nDone.\n")
