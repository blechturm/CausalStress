# ---------------------------------------------------------------------------
# Lane 2 (DGP) Phase 2 check: pathology verification, placebo integrity,
# axis attribution, and noise-independence audit for all 12 registry DGP IDs.
#
# Run:  "C:\Program Files\R\R-4.5.2\bin\x64\Rscript.exe" \
#         inst/design/audit/checks/lane2_pathology_checks.R \
#         > inst/design/audit/checks/lane2_pathology_checks_output.txt 2>&1
#
# Write boundary: only writes to stdout and tempdir() (oracle cache is
# redirected into tempdir via R_USER_CACHE_DIR so no user-cache writes occur).
# Seeds: explicit everywhere (SEED = 20260724).
# ---------------------------------------------------------------------------

# Redirect any package cache writes (oracle QST regeneration) into tempdir()
Sys.setenv(R_USER_CACHE_DIR = file.path(tempdir(), "cs_audit_cache"))

suppressMessages(suppressWarnings(pkgload::load_all(".", quiet = TRUE)))

SEED <- 20260724
N    <- 5000

fmt <- function(x, d = 4) formatC(x, digits = d, format = "f")

kurtosis_excess <- function(x) {
  x <- x[is.finite(x)]
  m <- mean(x); s <- sd(x)
  mean((x - m)^4) / s^4 - 3
}

overlap_stats <- function(p, w) {
  ctrl <- w == 0
  odds <- p[ctrl] / (1 - p[ctrl])       # ATT weights on controls
  ess  <- sum(odds)^2 / sum(odds^2)
  c(
    p_min      = min(p),
    p_q01      = unname(quantile(p, 0.01)),
    p_med      = unname(median(p)),
    p_q99      = unname(quantile(p, 0.99)),
    p_max      = max(p),
    frac_lt001 = mean(p < 0.01),
    frac_gt099 = mean(p > 0.99),
    sd_logit_p = sd(qlogis(pmin(pmax(p, 1e-12), 1 - 1e-12))),
    treated_sh = mean(w),
    ess_ctrl   = ess,
    ess_ratio  = ess / sum(ctrl)
  )
}

ids <- c(
  "synth_baseline", "synth_heavytail", "synth_qte1",
  "synth_nonlinear_heteroskedastic", "synth_overlap_stressed",
  "synth_tilt_mild", "synth_hd_sparse_plm",
  "synth_placebo_tau0", "synth_placebo_nonlinear",
  "synth_placebo_heavytail", "synth_placebo_tilted",
  "synth_placebo_kangschafer"
)

cat("=========================================================\n")
cat("SECTION A: registry resolution + per-DGP overlap/noise stats\n")
cat("n =", N, " seed =", SEED, " (registry-resolved default versions)\n")
cat("=========================================================\n\n")

dgps <- list()
for (id in ids) {
  row <- cs_get_dgp(id, quiet = TRUE)
  gen <- row$generator[[1]]
  d   <- gen(n = N, seed = SEED, include_truth = FALSE)
  dgps[[id]] <- list(row = row, d = d)
  os <- overlap_stats(d$df$p, d$df$w)
  cat(sprintf("%-33s v%-6s status=%-12s\n", id, row$version, row$status))
  cat(sprintf(
    "  p: min=%s q01=%s med=%s q99=%s max=%s | frac(p<.01)=%s frac(p>.99)=%s\n",
    fmt(os["p_min"]), fmt(os["p_q01"]), fmt(os["p_med"]),
    fmt(os["p_q99"]), fmt(os["p_max"]),
    fmt(os["frac_lt001"]), fmt(os["frac_gt099"])
  ))
  cat(sprintf(
    "  sd(logit p)=%s treated_share=%s ESS_ctrl=%s (ratio=%s) true_att=%s\n",
    fmt(os["sd_logit_p"]), fmt(os["treated_sh"]),
    fmt(os["ess_ctrl"], 1), fmt(os["ess_ratio"]), fmt(d$true_att)
  ))
  naive <- mean(d$df$y[d$df$w == 1]) - mean(d$df$y[d$df$w == 0])
  cat(sprintf("  naive DiM=%s  naive bias=%s\n\n", fmt(naive), fmt(naive - d$true_att)))
}

cat("=========================================================\n")
cat("SECTION B: placebo integrity (pathwise sharp null + zero truth)\n")
cat("(true_qst regenerated with oracle cache redirected to tempdir)\n")
cat("=========================================================\n\n")

placebos <- grep("placebo", ids, value = TRUE)
for (id in placebos) {
  row <- cs_get_dgp(id, quiet = TRUE)
  gen <- row$generator[[1]]
  d <- gen(n = N, seed = SEED, include_truth = TRUE)
  qst_max <- if (!is.null(d$true_qst)) max(abs(d$true_qst$value)) else NA_real_
  cat(sprintf(
    "%-28s identical(y0,y1)=%s  max|tau|=%s  true_att=%s  max|true_qst|=%s\n",
    id,
    identical(d$df$y0, d$df$y1),
    fmt(max(abs(d$df$structural_te))),
    fmt(d$true_att),
    fmt(qst_max)
  ))
}
cat("\n")

cat("=========================================================\n")
cat("SECTION C: noise-tail verification (heavytail / qte1 vs baseline)\n")
cat("eps0_hat = y0 - documented mu0(X); registry formulas\n")
cat("=========================================================\n\n")

noise_report <- function(id, eps) {
  cat(sprintf(
    "%-28s sd=%s mad=%s sd/mad=%s excess_kurtosis=%s max|eps|=%s frac(|eps|>2.5)=%s\n",
    id, fmt(sd(eps)), fmt(mad(eps)), fmt(sd(eps) / mad(eps)),
    fmt(kurtosis_excess(eps), 1), fmt(max(abs(eps)), 1), fmt(mean(abs(eps) > 2.5))
  ))
}
db <- dgps[["synth_baseline"]]$d$df
noise_report("synth_baseline", db$y0 - (1 + db$X1 + 0.5 * db$X2))
dh <- dgps[["synth_heavytail"]]$d$df
noise_report("synth_heavytail", dh$y0 - (1 + dh$X1 + 0.5 * dh$X2))
dq <- dgps[["synth_qte1"]]$d$df
noise_report("synth_qte1", dq$y0 - (1 + dq$X1 + 0.5 * dq$X2))
dq_sign_ok <- all(dq$structural_te == ifelse(dq$X1 > 0, 1, -1))
cat(sprintf("synth_qte1 sign-flip tau check (tau == +/-1 by X1>0): %s\n\n", dq_sign_ok))

cat("=========================================================\n")
cat("SECTION D: heteroskedasticity + axis coupling (nonlinear_heteroskedastic)\n")
cat("sigma(X)=0.1+exp(0.5*X2); p=plogis(0.5*X1-0.5*X2)  [v1.6.0 formulas]\n")
cat("=========================================================\n\n")

dn <- dgps[["synth_nonlinear_heteroskedastic"]]$d$df
mu0_n  <- 1 + 0.5 * dn$X1^3 + 1.5 * dn$X2^2 - 1.0 * dn$X4
eps_n  <- dn$y0 - mu0_n
sig_n  <- 0.1 + exp(0.5 * dn$X2)
qs <- cut(dn$X2, quantile(dn$X2, c(0, .25, .5, .75, 1)), include.lowest = TRUE,
          labels = c("X2 Q1", "X2 Q2", "X2 Q3", "X2 Q4"))
agg <- tapply(eps_n, qs, sd)
cat("empirical sd(eps0) by X2 quartile:", paste(names(agg), fmt(agg), collapse = " | "), "\n")
cat(sprintf("cor(sigma(X), p(X)) = %s   [axis coupling: noise scale vs selection]\n",
            fmt(cor(sig_n, dn$p))))
cat(sprintf("sd(eps0 | treated) = %s   sd(eps0 | control) = %s   ratio = %s\n",
            fmt(sd(eps_n[dn$w == 1])), fmt(sd(eps_n[dn$w == 0])),
            fmt(sd(eps_n[dn$w == 1]) / sd(eps_n[dn$w == 0]))))
cat(sprintf("cor(mu0(X), qlogis(p)) = %s   [axis coupling: curvature vs selection]\n\n",
            fmt(cor(mu0_n, qlogis(dn$p)))))

cat("=========================================================\n")
cat("SECTION E: high-dim sparse PLM structure (v-resolved)\n")
cat("=========================================================\n\n")

dhd <- dgps[["synth_hd_sparse_plm"]]$d$df
xcols <- grep("^X", names(dhd), value = TRUE)
cat(sprintf("n = %d covariates = %d\n", nrow(dhd), length(xcols)))
cat(sprintf("cor(X1,X2)=%s (declared 0.95)  cor(X1,X10)=%s (0.95^9=%s)\n",
            fmt(cor(dhd$X1, dhd$X2)), fmt(cor(dhd$X1, dhd$X10)), fmt(0.95^9)))
cat(sprintf("p range: [%s, %s]  sd(logit p)=%s\n",
            fmt(min(dhd$p)), fmt(max(dhd$p)),
            fmt(sd(qlogis(dhd$p)))))
cat(sprintf("pathwise ITE sd for NON-placebo DGP: sd(y1-y0-tau) = %s (baseline: %s)\n",
            fmt(sd(dhd$y1 - dhd$y0 - dhd$structural_te)),
            fmt(sd(db$y1 - db$y0 - db$structural_te))))
cat("  -> 0 means eps1 is pathwise identical to eps0 (shared noise), which\n")
cat("     contradicts Registry 1.4.0 s1.2 independence for non-placebo DGPs.\n\n")

cat("shared-noise audit across all non-placebo DGPs: sd(y1 - y0 - tau):\n")
for (id in setdiff(ids, placebos)) {
  dd <- dgps[[id]]$d$df
  cat(sprintf("  %-33s %s\n", id, fmt(sd(dd$y1 - dd$y0 - dd$structural_te))))
}
cat("\n")

cat("=========================================================\n")
cat("SECTION F: Kang-Schafer misspecification magnitude (truth = 0)\n")
cat("OLS-in-X and IPW-in-X ATT estimates ARE the misspecification bias\n")
cat("=========================================================\n\n")

ks <- dgps[["synth_placebo_kangschafer"]]$d$df
f_ols <- lm(y ~ w + X1 + X2 + X3 + X4, data = ks)
ps_fit <- suppressWarnings(glm(w ~ X1 + X2 + X3 + X4, data = ks, family = binomial()))
ph <- pmin(pmax(fitted(ps_fit), 1e-6), 1 - 1e-6)
odds <- ph / (1 - ph)
ipw_att <- mean(ks$y[ks$w == 1]) -
  sum(ks$y[ks$w == 0] * odds[ks$w == 0]) / sum(odds[ks$w == 0])
cat(sprintf("kangschafer: OLS-in-X ATT bias = %s | IPW-in-X ATT bias = %s | naive = %s\n",
            fmt(coef(f_ols)["w"]), fmt(ipw_att),
            fmt(mean(ks$y[ks$w == 1]) - mean(ks$y[ks$w == 0]))))

pt0 <- dgps[["synth_placebo_tau0"]]$d$df
f0 <- lm(y ~ w + X1 + X2 + X3 + X4 + X5, data = pt0)
cat(sprintf("placebo_tau0 (contrast): OLS ATT bias = %s\n\n", fmt(coef(f0)["w"])))

cat("=========================================================\n")
cat("SECTION G: tilt axis separation (selection geometry vs baseline)\n")
cat("=========================================================\n\n")

cosang <- function(a, b) sum(a * b) / sqrt(sum(a^2) * sum(b^2))
base_beta <- c(X1 = 0.5, X2 = -0.5, X4 = 0)
tilt_beta <- c(X1 = 0.45, X2 = -0.3, X4 = -0.25)
ptl_beta  <- c(X1 = 0.6, X2 = 0.8, X4 = 0)
ovl_beta  <- c(X1 = 9, X2 = 9, X4 = 0)
cat(sprintf("|beta|: baseline=%s tilt_mild=%s placebo_tilted(v1.4)=%s overlap(v1.6)=%s\n",
            fmt(sqrt(sum(base_beta^2))), fmt(sqrt(sum(tilt_beta^2))),
            fmt(sqrt(sum(ptl_beta^2))), fmt(sqrt(sum(ovl_beta^2)))))
cat(sprintf("cos angle to baseline: tilt_mild=%s placebo_tilted=%s overlap_stressed=%s\n",
            fmt(cosang(base_beta, tilt_beta)), fmt(cosang(base_beta, ptl_beta)),
            fmt(cosang(base_beta, ovl_beta))))
cat("  -> overlap_stressed and placebo_tilted select along a direction ORTHOGONAL-to-\n")
cat("     NEGATIVELY-aligned with the baseline selection direction (X2 sign flipped),\n")
cat("     so severity ladders baseline -> tilt -> overlap change direction, not just dose.\n\n")

cat("sd(logit p) ladder (selection strength, same seed):\n")
for (id in c("synth_tilt_mild", "synth_baseline", "synth_placebo_tilted",
             "synth_overlap_stressed")) {
  pp <- dgps[[id]]$d$df$p
  cat(sprintf("  %-25s sd(logit p) = %s\n", id,
              fmt(sd(qlogis(pmin(pmax(pp, 1e-12), 1 - 1e-12))))))
}
cat("  -> tilt_mild has WEAKER selection than the baseline sanity check.\n\n")

cat("Done. SEED =", SEED, "\n")
