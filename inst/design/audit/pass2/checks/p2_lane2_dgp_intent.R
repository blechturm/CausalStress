# PASS 2 - Lane 2 (DGP) intent-first evidence.
# Extends inst/design/audit/checks/lane2_pathology_v2.R with MULTI-SEED
# robustness for the intent-sensitive re-adjudications so no single-seed
# artifact can mislead. Generators are called with include_truth = TRUE
# (mandate; include_truth = FALSE aborts heavytail/tilt_mild/placebos via the
# NULL true_qst self-check). Oracle cache -> tempdir(); explicit seeds.
#
# Establishes, empirically:
#  S1  12-DGP pathology signature at benchmark n (fresh-seed re-confirmation).
#  S2  heavytail L2 break: mean-of-outcomes does NOT converge (across-seed
#      spread of naive mean-ATT is flat/erratic in n), yet the STRUCTURAL ATT
#      truth (signal-only) is stable and QST (quantiles) exists  -> SOUND-by-intent.
#  S3  hd_sparse constant +1 shift: Y1 == Y0 + 1 exactly every seed
#      (sd(y1-y0-tau)=0), true_att=1  -> declared shared-noise design (SOUND).
#  S4  tilt_mild vs baseline (item e): milder positivity than baseline
#      (consistent with sidecar mild<moderate) but >= baseline naive/confounding.
#  S5  nonlinear_heteroskedastic (item e): sigma(X), mu0(X) and selection all
#      couple through X2 -> intended combined stressor, entangled attribution.
#  S6  Registry 1.4.0 staleness: realized parameter fingerprints vs documented.
#  S7  placebo integrity (sharp null Y1==Y0, zero truth) across seeds.

Sys.setenv(R_USER_CACHE_DIR = file.path(tempdir(), "cs_audit_cache_p2"))
suppressMessages(suppressWarnings(pkgload::load_all(".", quiet = TRUE)))

fmt  <- function(x, d = 4) formatC(x, digits = d, format = "f")
kurt <- function(x){x<-x[is.finite(x)];m<-mean(x);s<-sd(x);mean((x-m)^4)/s^4-3}
gen_of <- function(id) cs_get_dgp(id, quiet = TRUE)$generator[[1]]
sd_logit <- function(p) sd(qlogis(pmin(pmax(p,1e-12),1-1e-12)))
ess_ctrl <- function(p, w){o<-p[w==0]/(1-p[w==0]); sum(o)^2/sum(o^2)/sum(w==0)}
naive_att <- function(d) mean(d$df$y[d$df$w==1]) - mean(d$df$y[d$df$w==0])

ids <- c("synth_baseline","synth_heavytail","synth_qte1",
  "synth_nonlinear_heteroskedastic","synth_overlap_stressed","synth_tilt_mild",
  "synth_hd_sparse_plm","synth_placebo_tau0","synth_placebo_nonlinear",
  "synth_placebo_heavytail","synth_placebo_tilted","synth_placebo_kangschafer")

cat("################ p2_lane2_dgp_intent ################\n")

## S1 -------------------------------------------------------------------------
SEED1 <- 20260726L; N1 <- 5000L
cat(sprintf("\n== S1: 12-DGP signature (fresh seed=%d, n=%d) ==\n", SEED1, N1))
for (id in ids) {
  d <- gen_of(id)(n = N1, seed = SEED1, include_truth = TRUE)
  p <- d$df$p; w <- d$df$w
  shared <- if (id %in% grep("placebo", ids, value = TRUE) || id == "synth_hd_sparse_plm")
    sd(d$df$y1 - d$df$y0 - d$df$structural_te) else sd(d$df$y1 - d$df$y0 - d$df$structural_te)
  cat(sprintf("%-32s sdlogit=%6s fr<.01=%5s fr>.99=%5s ESSc=%5s tATT=%7s nbias=%8s sd(y1-y0-tau)=%s\n",
    id, fmt(sd_logit(p),3), fmt(mean(p<.01),3), fmt(mean(p>.99),3),
    fmt(ess_ctrl(p,w),3), fmt(d$true_att,3), fmt(naive_att(d)-d$true_att,3),
    fmt(shared,3)))
}

## S2 : heavytail L2 break -- non-convergence of the mean ---------------------
cat("\n== S2: heavytail mean-of-outcomes non-convergence (40 seeds) ==\n")
cat("   across-seed sd of naive mean-ATT should SHRINK ~1/sqrt(n) for baseline,\n")
cat("   but stay large/erratic for heavytail (Cauchy => no finite mean).\n")
seeds2 <- 1:40
for (n in c(1000L, 5000L, 20000L)) {
  bl <- vapply(seeds2, function(s) naive_att(gen_of("synth_baseline")(n=n,seed=s,include_truth=TRUE)), numeric(1))
  ht <- vapply(seeds2, function(s) naive_att(gen_of("synth_heavytail")(n=n,seed=s,include_truth=TRUE)), numeric(1))
  cat(sprintf("  n=%6d  baseline sd(naiveATT)=%7s [range %7s]   heavytail sd(naiveATT)=%9s [range %9s]\n",
    n, fmt(sd(bl),4), fmt(diff(range(bl)),3), fmt(sd(ht),3), fmt(diff(range(ht)),2)))
}
# structural truth stays well-posed while mean is ill-posed
dht <- gen_of("synth_heavytail")(n=20000L, seed=7L, include_truth=TRUE)
cat(sprintf("  heavytail: true_att(structural,signal-only)=%s  finite=%s\n",
  fmt(dht$true_att), is.finite(dht$true_att)))
cat(sprintf("  heavytail: QST quantiles exist (any NA in true_qst$value)=%s  eps excess_kurt=%s max|eps|=%s\n",
  anyNA(dht$true_qst$value),
  fmt(kurt(dht$df$y0-(1+dht$df$X1+0.5*dht$df$X2)),1),
  fmt(max(abs(dht$df$y0-(1+dht$df$X1+0.5*dht$df$X2))),1)))

## S3 : hd_sparse constant +1 shift (shared noise) ----------------------------
cat("\n== S3: hd_sparse constant +1 shift == declared design (5 seeds) ==\n")
for (s in 1:5) {
  d <- gen_of("synth_hd_sparse_plm")(n=3000L, seed=s, include_truth=TRUE)
  cat(sprintf("  seed=%d  sd(y1-y0-tau)=%s  identical(y1, y0+1)=%s  true_att=%s\n",
    s, fmt(sd(d$df$y1-d$df$y0-d$df$structural_te)),
    isTRUE(all.equal(d$df$y1, d$df$y0 + 1)), fmt(d$true_att,3)))
}

## S4 : tilt_mild vs baseline (item e) ----------------------------------------
cat("\n== S4: tilt_mild vs baseline over 30 seeds (item e) ==\n")
seeds4 <- 1:30
grab <- function(id) {
  t(vapply(seeds4, function(s){
    d <- gen_of(id)(n=4000L, seed=s, include_truth=TRUE)
    c(sdlogit=sd_logit(d$df$p), essc=ess_ctrl(d$df$p,d$df$w),
      nbias=naive_att(d)-d$true_att)
  }, numeric(3)))
}
bl <- grab("synth_baseline"); tm <- grab("synth_tilt_mild")
cat(sprintf("  baseline : sd(logit p)=%s  ESSc=%s  |naive bias|=%s\n",
  fmt(mean(bl[,"sdlogit"]),3), fmt(mean(bl[,"essc"]),3), fmt(mean(abs(bl[,"nbias"])),3)))
cat(sprintf("  tilt_mild: sd(logit p)=%s  ESSc=%s  |naive bias|=%s\n",
  fmt(mean(tm[,"sdlogit"]),3), fmt(mean(tm[,"essc"]),3), fmt(mean(abs(tm[,"nbias"])),3)))
cat(sprintf("  => tilt_mild positivity milder than baseline: %s (sidecar mild<moderate)\n",
  mean(tm[,"sdlogit"]) < mean(bl[,"sdlogit"])))
cat(sprintf("  => tilt_mild |naive bias| >= baseline: %s (covariate-shift/confounding axis)\n",
  mean(abs(tm[,"nbias"])) >= mean(abs(bl[,"nbias"]))))

## S5 : nonlinear_heteroskedastic coupling (item e) ---------------------------
cat("\n== S5: nonlinear_heteroskedastic axis coupling over 10 seeds (item e) ==\n")
cc <- t(vapply(1:10, function(s){
  d <- gen_of("synth_nonlinear_heteroskedastic")(n=5000L, seed=s, include_truth=TRUE)$df
  mu0 <- 1 + 0.5*d$X1^3 + 1.5*d$X2^2 - 1.0*d$X4
  sig <- 0.1 + 1.0*exp(0.5*d$X2)
  c(cor_sig_p = cor(sig, d$p), cor_mu0_logitp = cor(mu0, qlogis(d$p)))
}, numeric(2)))
cat(sprintf("  mean cor(sigma(X), p)=%s   mean cor(mu0(X), logit p)=%s\n",
  fmt(mean(cc[,"cor_sig_p"]),3), fmt(mean(cc[,"cor_mu0_logitp"]),3)))
cat("  (nonzero both => heteroskedasticity + outcome curvature + selection all via X2:\n")
cat("   intended combined stressor, but single-axis attribution is not separable.)\n")

## S6 : Registry 1.4.0 staleness (VALUE fingerprints) -------------------------
cat("\n== S6: Registry 1.4.0 documented vs realized (sidecars/code authoritative) ==\n")
ov <- gen_of("synth_overlap_stressed")(n=20000L, seed=1L, include_truth=TRUE)$df
cat(sprintf("  overlap_stressed  sd(logit p)=%s  (Reg 3.0X1+3.0X2 -> ~4.24; code 9.0 -> ~12.7)\n",
  fmt(sd_logit(ov$p),2)))
pt <- gen_of("synth_placebo_tilted")(n=20000L, seed=1L, include_truth=TRUE)$df
cat(sprintf("  placebo_tilted    sd(logit p)=%s  (Reg 1.0X1+1.2X2 -> ~1.56; code 0.6/0.8 -> ~1.0)\n",
  fmt(sd_logit(pt$p),2)))
hd <- gen_of("synth_hd_sparse_plm")(n=200L, seed=1L, include_truth=TRUE)$df
cat(sprintf("  hd_sparse_plm     #covariates=%d  (Reg p=50; code p=100)\n",
  sum(grepl("^X[0-9]+$", names(hd)))))
nh <- gen_of("synth_nonlinear_heteroskedastic")(n=200L, seed=1L, include_truth=TRUE)$df
cat(sprintf("  nonlinear_het     has X5=%s  (Reg & code both R^4; mu0 Reg=sin+0.5X2^2, code=0.5X1^3+1.5X2^2)\n",
  "X5" %in% names(nh)))

## S7 : placebo integrity across seeds ----------------------------------------
cat("\n== S7: placebo sharp-null integrity (3 seeds) ==\n")
for (id in grep("placebo", ids, value = TRUE)) {
  ok <- vapply(1:3, function(s){
    d <- gen_of(id)(n=2000L, seed=s, include_truth=TRUE)
    identical(d$df$y0,d$df$y1) && d$true_att==0 &&
      max(abs(d$true_qst$value))==0 && max(abs(d$df$structural_te))==0
  }, logical(1))
  cat(sprintf("  %-28s sharp-null holds all seeds = %s\n", id, all(ok)))
}

cat("\nDone. seeds fixed; oracle cache in tempdir.\n")
