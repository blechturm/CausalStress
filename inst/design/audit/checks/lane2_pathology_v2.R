# Lane 2 (DGP) pathology check — ORCHESTRATOR-AUTHORED correction of the
# credit-terminated agent script `lane2_pathology_checks.R`.
# Change vs original: call generators with include_truth = TRUE. The original
# used include_truth = FALSE, which aborts on generators (e.g. heavytail_v160)
# that call cs_check_dgp_synthetic() unconditionally on a NULL true_qst.
# Data columns (df$p, w, y0, y1, structural_te, X*) are identical either way.
# Adds Section H: an include_truth = FALSE robustness probe across all 12 IDs.
# Oracle cache -> tempdir(); explicit seed. EMPIRICAL evidence for the DGP lane.
Sys.setenv(R_USER_CACHE_DIR = file.path(tempdir(), "cs_audit_cache"))
suppressMessages(suppressWarnings(pkgload::load_all(".", quiet = TRUE)))
SEED <- 20260724; N <- 5000
fmt <- function(x, d = 4) formatC(x, digits = d, format = "f")
kurt <- function(x){x<-x[is.finite(x)];m<-mean(x);s<-sd(x);mean((x-m)^4)/s^4-3}

ids <- c("synth_baseline","synth_heavytail","synth_qte1",
  "synth_nonlinear_heteroskedastic","synth_overlap_stressed","synth_tilt_mild",
  "synth_hd_sparse_plm","synth_placebo_tau0","synth_placebo_nonlinear",
  "synth_placebo_heavytail","synth_placebo_tilted","synth_placebo_kangschafer")

cat("== SECTION A: overlap / selection strength / naive bias (n=",N," seed=",SEED,") ==\n",sep="")
dgps <- list()
for (id in ids) {
  row <- cs_get_dgp(id, quiet = TRUE); gen <- row$generator[[1]]
  d <- gen(n = N, seed = SEED, include_truth = TRUE); dgps[[id]] <- d
  p <- d$df$p; w <- d$df$w; ctrl <- w==0
  odds <- p[ctrl]/(1-p[ctrl]); ess <- sum(odds)^2/sum(odds^2)
  naive <- mean(d$df$y[w==1]) - mean(d$df$y[w==0])
  cat(sprintf("%-32s v%-5s %-11s | p[min,med,max]=%s,%s,%s frac(p<.01)=%s frac(p>.99)=%s\n",
    id,row$version,row$status,fmt(min(p)),fmt(median(p)),fmt(max(p)),
    fmt(mean(p<.01)),fmt(mean(p>.99))))
  cat(sprintf("   sd(logit p)=%s treated_sh=%s ESS_ctrl/n_ctrl=%s true_att=%s naive_bias=%s\n",
    fmt(sd(qlogis(pmin(pmax(p,1e-12),1-1e-12)))),fmt(mean(w)),
    fmt(ess/sum(ctrl)),fmt(d$true_att),fmt(naive-d$true_att)))
}

cat("\n== SECTION B: placebo integrity (pathwise Y1==Y0, zero truth) ==\n")
for (id in grep("placebo",ids,value=TRUE)) {
  d <- dgps[[id]]
  cat(sprintf("%-28s identical(y0,y1)=%s max|tau|=%s true_att=%s max|true_qst|=%s\n",
    id, identical(d$df$y0,d$df$y1), fmt(max(abs(d$df$structural_te))),
    fmt(d$true_att), fmt(if(!is.null(d$true_qst)) max(abs(d$true_qst$value)) else NA)))
}

cat("\n== SECTION C: noise tails (eps0_hat = y0 - mu0(X); baseline mu0=1+X1+0.5X2) ==\n")
nr <- function(id,eps) cat(sprintf("%-24s sd=%s excess_kurt=%s max|eps|=%s frac(|eps|>2.5)=%s\n",
  id,fmt(sd(eps)),fmt(kurt(eps),1),fmt(max(abs(eps)),1),fmt(mean(abs(eps)>2.5))))
db<-dgps[["synth_baseline"]]$df; nr("baseline", db$y0-(1+db$X1+0.5*db$X2))
dh<-dgps[["synth_heavytail"]]$df; nr("heavytail",dh$y0-(1+dh$X1+0.5*dh$X2))
dq<-dgps[["synth_qte1"]]$df; nr("qte1",dq$y0-(1+dq$X1+0.5*dq$X2))
cat(sprintf("qte1 sign-flip tau==+/-1 by X1>0: %s\n", all(dq$structural_te==ifelse(dq$X1>0,1,-1))))

cat("\n== SECTION D: multi-axis coupling (nonlinear_heteroskedastic) ==\n")
dn<-dgps[["synth_nonlinear_heteroskedastic"]]$df
mu0n<-1+0.5*dn$X1^3+1.5*dn$X2^2-1.0*dn$X4; epsn<-dn$y0-mu0n; sig<-0.1+exp(0.5*dn$X2)
cat(sprintf("cor(sigma(X),p)=%s cor(mu0(X),logit p)=%s sd(eps|trt)/sd(eps|ctrl)=%s\n",
  fmt(cor(sig,dn$p)),fmt(cor(mu0n,qlogis(dn$p))),
  fmt(sd(epsn[dn$w==1])/sd(epsn[dn$w==0]))))

cat("\n== SECTION E: shared-noise audit: sd(y1-y0-tau) for non-placebo DGPs ==\n")
cat("   (0 => eps1 pathwise-identical to eps0 i.e. CRN/shared noise in the DATA path)\n")
for (id in setdiff(ids,grep("placebo",ids,value=TRUE))) {
  dd<-dgps[[id]]$df
  cat(sprintf("  %-32s sd(y1-y0-tau)=%s\n",id,fmt(sd(dd$y1-dd$y0-dd$structural_te))))
}

cat("\n== SECTION F: kang-schafer misspecification (truth=0; OLS/IPW-in-X ATT = bias) ==\n")
ks<-dgps[["synth_placebo_kangschafer"]]$df
cat(sprintf("kangschafer OLS-in-X ATT bias=%s naive=%s | placebo_tau0 OLS ATT bias=%s\n",
  fmt(coef(lm(y~w+X1+X2+X3+X4,ks))["w"]),
  fmt(mean(ks$y[ks$w==1])-mean(ks$y[ks$w==0])),
  fmt(coef(lm(y~w+X1+X2+X3+X4+X5,dgps[["synth_placebo_tau0"]]$df))["w"])))

cat("\n== SECTION G: selection ladder sd(logit p) baseline->tilt->overlap ==\n")
for (id in c("synth_tilt_mild","synth_baseline","synth_placebo_tilted","synth_overlap_stressed")){
  pp<-dgps[[id]]$df$p
  cat(sprintf("  %-24s sd(logit p)=%s\n",id,fmt(sd(qlogis(pmin(pmax(pp,1e-12),1-1e-12))))))
}

cat("\n== SECTION H: include_truth=FALSE robustness probe across all 12 IDs ==\n")
cat("   (does data-only generation succeed, or does the self-check abort?)\n")
for (id in ids){
  row<-cs_get_dgp(id,quiet=TRUE); gen<-row$generator[[1]]
  ok<-tryCatch({gen(n=50,seed=1L,include_truth=FALSE); "OK"},
    error=function(e) paste0("ABORT: ",conditionMessage(e)))
  cat(sprintf("  %-32s v%-5s include_truth=FALSE -> %s\n",id,row$version,substr(ok,1,60)))
}
cat("\nDone. SEED=",SEED,"\n")
