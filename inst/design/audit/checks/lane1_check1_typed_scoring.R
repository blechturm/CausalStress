# Lane 1 (estimands) — Check 1: typed scoring, truth recomputation, no-cross-scoring
#
# EMPIRICAL claims exercised:
#  (A) Oracle-estimator probe: est_oracle_att (structural_te grant) scores with
#      exactly zero ATT and ATE error against runner-recorded truth on
#      synth_baseline (stable v1.6.0) over 5 seeds.
#  (B) Independent truth recomputation: ATT truth == mean(structural_te[w==1]),
#      ATE truth == mean(meta$structural_te), recomputed from a fresh generator
#      call at the same (dgp, version, seed); bitwise comparison.
#  (C) No-cross-scoring: ATT rows carry ATT truth (treated mean), ATE rows carry
#      ATE truth (full-sample mean), truths differ (population separation);
#      requested-but-unproduced targets (qst for oracle_att; ate for lm_att)
#      appear only as non_comparable rows with machine-readable reasons; CATE in
#      a mixed request yields target_not_implemented; CATE-only hard-rejects.
#  (D) Airlock shadow: default (no-grant) airlock removes y0/y1/p/structural_te.
#
# All randomness is via CausalStress's mandated RNG through explicit seeds.
# Oracle cache is redirected to tempdir() to respect the review write boundary.

Sys.setenv(R_USER_CACHE_DIR = file.path(tempdir(), "cs_audit_cache"))
suppressMessages(pkgload::load_all(".", quiet = TRUE))

fail <- 0L
note <- function(ok, msg) {
  status <- if (isTRUE(ok)) "PASS" else "FAIL"
  if (!isTRUE(ok)) fail <<- fail + 1L
  cat(sprintf("[%s] %s\n", status, msg))
}

n <- 2000L
seeds <- 101:105

cat("== Part A/B/C: oracle_att on synth_baseline (stable), targets att+ate+qst+cate ==\n")
for (s in seeds) {
  run <- cs_run_single(
    dgp_id = "synth_baseline", estimator_id = "oracle_att",
    n = n, seed = s, quiet = TRUE,
    config = list(estimand_targets = c("att", "ate", "qst", "cate"))
  )
  sc <- run$scores

  att_row  <- sc[sc$estimand_target_id == "att", ]
  ate_row  <- sc[sc$estimand_target_id == "ate", ]
  qst_row  <- sc[sc$estimand_target_id == "qst", ]
  cate_row <- sc[sc$estimand_target_id == "cate", ]

  note(nrow(att_row) == 1 && att_row$score_status == "scored",
       sprintf("seed %d: ATT row present and scored", s))
  note(nrow(ate_row) == 1 && ate_row$score_status == "scored",
       sprintf("seed %d: ATE row present and scored", s))
  note(is.finite(att_row$error) && abs(att_row$error) < 1e-12,
       sprintf("seed %d: oracle ATT error == 0 (got %.3e)", s, att_row$error))
  note(is.finite(ate_row$error) && abs(ate_row$error) < 1e-12,
       sprintf("seed %d: oracle ATE error == 0 (got %.3e)", s, ate_row$error))

  # Independent truth recomputation from a fresh generator call
  d <- dgp_synth_baseline_v160(n = n, seed = s, include_truth = FALSE)
  att_truth_indep <- mean(d$df$structural_te[d$df$w == 1])
  ate_truth_indep <- mean(d$meta$structural_te)
  note(identical(att_row$truth, att_truth_indep),
       sprintf("seed %d: scored ATT truth bitwise == independent treated-mean (%.6f)", s, att_truth_indep))
  note(identical(ate_row$truth, ate_truth_indep),
       sprintf("seed %d: scored ATE truth bitwise == independent full-sample mean (%.6f)", s, ate_truth_indep))

  # Population separation: ATT truth must NOT equal ATE truth (tilted treated pop)
  gap <- att_row$truth - ate_row$truth
  note(!identical(att_row$truth, ate_row$truth),
       sprintf("seed %d: ATT truth != ATE truth (finite-sample gap = %+.5f) — no cross-assignment possible silently", s, gap))
  note(!identical(att_row$truth, ate_truth_indep) && !identical(ate_row$truth, att_truth_indep),
       sprintf("seed %d: ATT row does not carry ATE truth and vice versa", s))

  # Unproduced / unimplemented targets: explicit non-comparable rows only
  note(nrow(qst_row) == 1 && qst_row$score_status == "non_comparable" &&
         identical(qst_row$non_comparable_reason, "estimator_not_produced"),
       sprintf("seed %d: QST (not produced by oracle_att) -> non_comparable/estimator_not_produced", s))
  note(nrow(cate_row) == 1 && cate_row$score_status == "non_comparable" &&
         identical(cate_row$non_comparable_reason, "target_not_implemented"),
       sprintf("seed %d: CATE (mixed request) -> non_comparable/target_not_implemented", s))
  note(all(is.na(qst_row$estimate)) && all(is.na(qst_row$truth)) &&
         all(is.na(cate_row$estimate)) && all(is.na(cate_row$truth)),
       sprintf("seed %d: non_comparable rows carry NA value fields (never a cross-scored value)", s))
  note(!any(sc$score_status == "scored" & sc$estimand_target_id %in% c("qst", "cate")),
       sprintf("seed %d: no scored rows exist for unproduced/unimplemented targets", s))
}

cat("\n== Part C2: lm_att requesting att+ate (produces att only) ==\n")
run2 <- cs_run_single(
  dgp_id = "synth_baseline", estimator_id = "lm_att",
  n = n, seed = 101L, quiet = TRUE,
  config = list(estimand_targets = c("att", "ate"))
)
sc2 <- run2$scores
att2 <- sc2[sc2$estimand_target_id == "att", ]
ate2 <- sc2[sc2$estimand_target_id == "ate", ]
note(nrow(att2) == 1 && att2$score_status == "scored" && is.finite(att2$error),
     sprintf("lm_att ATT scored against ATT truth (err = %+.4f)", att2$error))
note(nrow(ate2) == 1 && ate2$score_status == "non_comparable" &&
       identical(ate2$non_comparable_reason, "estimator_not_produced"),
     "lm_att ATE request -> non_comparable/estimator_not_produced (ATT output NOT cross-scored as ATE)")
note(is.na(ate2$estimate) && is.na(ate2$truth) && is.na(ate2$error),
     "lm_att ATE non_comparable row has NA estimate/truth/error")
note(identical(att2$scoring_population_id, "treated") &&
       identical(ate2$scoring_population_id, "full_generated_run_sample"),
     "scoring_population_id: att='treated', ate='full_generated_run_sample' (declared populations recorded)")

cat("\n== Part C3: CATE-only task hard-rejects before estimator execution ==\n")
err <- tryCatch(
  {
    cs_run_single(
      dgp_id = "synth_baseline", estimator_id = "lm_att",
      n = 500L, seed = 1L, quiet = TRUE,
      config = list(estimand_targets = "cate")
    )
    NULL
  },
  error = function(e) e
)
note(!is.null(err) && inherits(err, "causalstress_target_not_implemented_error"),
     sprintf("CATE-only request aborts with class causalstress_target_not_implemented_error (%s)",
             if (is.null(err)) "no error!" else class(err)[[1]]))

cat("\n== Part D: airlock shadow (default grant set is empty) ==\n")
d <- dgp_synth_baseline_v160(n = 200L, seed = 42L, include_truth = FALSE)
df_sane <- cs_airlock(d$df, config = list(), estimator_desc = list())
note(!any(c("y0", "y1", "p", "structural_te") %in% names(df_sane)),
     sprintf("airlocked frame drops y0/y1/p/structural_te (kept: %s)", paste(names(df_sane), collapse = ",")))

cat(sprintf("\n== RESULT: %s (%d failures) ==\n", if (fail == 0L) "ALL PASS" else "FAILURES", fail))
if (fail > 0L) quit(status = 1L)
