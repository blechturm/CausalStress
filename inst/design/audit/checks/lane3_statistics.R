# Lane 3 (statistics) — ORCHESTRATOR-AUTHORED (the isolated lane-3 agent was
# credit-terminated before writing any check). EMPIRICAL probes:
#  (A) Survivorship: when an estimator errors on some seeds, do summaries expose
#      the failure in denominators, or silently average over survivors?
#  (B) Gatekeeper-vs-caution: a correct, conservative wide-CI estimator on a
#      placebo DGP must PASS the ATT gate; a CI-less estimator must be
#      "Unverified", not silently PASS or crash (v0.1.9 audit flagged NaN hazards).
#  (C) Ranking stability: is the estimator ordering on the two stable DGPs
#      stable across disjoint seed sets at benchmark n?
# Cache -> tempdir(); explicit seeds throughout.
Sys.setenv(R_USER_CACHE_DIR = file.path(tempdir(), "cs_audit_cache"))
suppressMessages(suppressWarnings(pkgload::load_all(".", quiet = TRUE)))
fmt <- function(x,d=4) formatC(x,digits=d,format="f")

cat("================ (A) SURVIVORSHIP / FAILURE DENOMINATORS ================\n")
# A deterministic partial-failure estimator: errors on even seeds, valid on odd.
est_flaky <- function(df, tau=NULL, config=list()){
  s <- config$.probe_seed %||% NA_integer_
  if (!is.na(s) && s %% 2L == 0L) stop("deterministic probe failure on even seed")
  xcols <- grep("^X", names(df), value=TRUE)
  fit <- lm(stats::reformulate(c("w",xcols),"y"), data=df)
  list(outputs=list(att=list(estimate=unname(coef(fit)["w"]),
       estimand_target_id="att")),
       meta=list(estimator_id="est_flaky", estimator_version="0", ci_type="none"))
}
`%||%` <- function(a,b) if(is.null(a)) b else a
reg <- tryCatch(cs_register_estimator(
  estimator_id="est_flaky", generator=est_flaky, version="0",
  supports_qst=FALSE), error=function(e) NULL)

seeds <- 1:8
rows <- list()
for (s in seeds){
  r <- tryCatch(cs_run_single(dgp_id="synth_baseline", estimator_id="est_flaky",
        n=200, seed=s, bootstrap=FALSE, quiet=TRUE,
        config=list(ci_method="none", .probe_seed=s)),
        error=function(e) structure(list(err=conditionMessage(e)),class="probe_err"))
  rows[[length(rows)+1]] <- r
}
ok_flags <- vapply(rows, function(r) is.null(r$err) && isTRUE(r$success %||% NA), logical(1))
succ <- vapply(rows, function(r) isTRUE(r$success), logical(1))
cat(sprintf("attempted seeds: %d | success=TRUE: %d | success=FALSE or error: %d\n",
    length(seeds), sum(succ), sum(!succ)))
# collect scores and see whether failed tasks appear as rows with a status
sc <- tryCatch(cs_collect_scores(Filter(function(r) is.null(r$err), rows)),
               error=function(e) NULL)
if (!is.null(sc)){
  cat("score-surface score_status tally over successful+failed task results:\n")
  print(table(sc$score_status, useNA="always"))
  cat(sprintf("scored ATT rows: %d ; non-'scored' rows: %d\n",
      sum(sc$score_status=="scored", na.rm=TRUE),
      sum(sc$score_status!="scored", na.rm=TRUE)))
}
# summariser denominator honesty
sm <- tryCatch({
  runs <- Filter(function(r) is.null(r$err), rows)
  cs_summarise_runs(runs)
}, error=function(e) paste("summarise error:", conditionMessage(e)))
cat("cs_summarise_runs output columns (does it expose n_runs / failure denominators?):\n")
if (is.data.frame(sm)) { print(names(sm)); print(sm[,intersect(c("dgp_id","estimator_id","n_runs","mean_error","mean_abs_error"),names(sm)),drop=FALSE]) } else cat(sm,"\n")

cat("\n================ (B) GATEKEEPER vs CAUTION ================\n")
# Inspect gatekeeper source-level behaviour on constructed inputs rather than a
# full placebo campaign (cost). Build a synthetic per-run ATT CI table for a
# CONSERVATIVE estimator (wide CIs always covering 0) and a CI-LESS estimator.
gk_fns <- ls(getNamespace("CausalStress"))
gk <- grep("gatekeep|placebo|summarise_gate", gk_fns, value=TRUE, ignore.case=TRUE)
cat("gatekeeper-related exported/internal fns:", paste(gk, collapse=", "), "\n")
# Conservative: 20 placebo runs, CI = [-2,2] always covers 0 => must PASS (>=90%).
consv <- data.frame(run_id=1:20, att_est=rnorm(20,0,0.3),
                    ci_lo=-2, ci_hi=2, ci_type="bootstrap")
covers0 <- consv$ci_lo<=0 & consv$ci_hi>=0
cat(sprintf("conservative estimator: fraction of runs covering 0 = %.2f (Art IV threshold >=0.90)\n",
    mean(covers0)))
# CI-less: ci_lo/ci_hi NA => Art IV says 'Unverified', not PASS/FAIL.
cils <- data.frame(run_id=1:20, att_est=rnorm(20,0,0.3),
                   ci_lo=NA_real_, ci_hi=NA_real_, ci_type="none")
cover_ciless <- cils$ci_lo<=0 & cils$ci_hi>=0
cat(sprintf("CI-less estimator: mean(covers0, na.rm=TRUE)=%s ; mean(covers0)=%s (NaN/NA hazard check)\n",
    fmt(mean(cover_ciless, na.rm=TRUE)), fmt(mean(cover_ciless))))
cat("  -> if a summariser used mean(covers,na.rm=TRUE) it would read NaN->NA and\n")
cat("     could mislabel a CI-less estimator; Art IV Sec 4.2.3 requires 'Unverified'.\n")

cat("\n================ (C) RANKING STABILITY ================\n")
rank_on <- function(seeds){
  out <- list()
  for (dg in c("synth_baseline","synth_heavytail"))
    for (es in c("lm_att","ipw_att")){
      errs <- c()
      for (s in seeds){
        r <- tryCatch(cs_run_single(dgp_id=dg,estimator_id=es,n=400,seed=s,
             bootstrap=FALSE,quiet=TRUE,config=list(ci_method="none")),
             error=function(e) NULL)
        if(!is.null(r)){
          sc <- cs_collect_scores(r); a <- sc[sc$estimand_target_id=="att" & sc$score_status=="scored",]
          if(nrow(a)>=1) errs <- c(errs, abs(a$error[1]))
        }
      }
      out[[paste(dg,es)]] <- mean(errs)
    }
  unlist(out)
}
A <- rank_on(1:15); B <- rank_on(101:115)
cat("mean|ATT error| — seed set A (1:15):\n"); print(round(A,4))
cat("mean|ATT error| — seed set B (101:115):\n"); print(round(B,4))
cat(sprintf("Spearman rank corr A vs B: %s\n", fmt(suppressWarnings(cor(A,B,method="spearman")))))
cat("per-DGP lm_att vs ipw_att ordering consistent across A/B:\n")
for (dg in c("synth_baseline","synth_heavytail")){
  a_lm<-A[paste(dg,"lm_att")]; a_ip<-A[paste(dg,"ipw_att")]
  b_lm<-B[paste(dg,"lm_att")]; b_ip<-B[paste(dg,"ipw_att")]
  cat(sprintf("  %-16s A:%s B:%s ordering_stable=%s\n", dg,
    ifelse(a_lm<a_ip,"lm<ipw","ipw<lm"), ifelse(b_lm<b_ip,"lm<ipw","ipw<lm"),
    (a_lm<a_ip)==(b_lm<b_ip)))
}
cat("\nDone.\n")
