# Phase 1 Claim Reconstruction — Lane ESTIMANDS (docs-only)

## 1. Scientific claim
CausalStress identifies where estimators succeed, degrade, and fail under
controlled *synthetic* stress **without misleading comparisons across estimands,
populations, failures, or DGPs** (phase0-triage "Claim under review"). It enforces
a versioned Two-Tier Truth Contract (Const. Art. I §1.1) via *typed,
no-cross-scoring* over a governed estimand registry (§1.7; contracts). Tiers rank:
reproducible ≠ scientifically valid ≠ externally representative (triage).

## 2. Assumptions
Selection-on-observables and SUTVA (every narrative §3); overlap varies (moderate
default; severe `overlap_stressed`; latent-space only `kangschafer`). Frozen RNG +
immutable `(dgp_id,version)` (Art. II §2.1, VII); truth is the **noise-free
structural τ(X)**, never the sample mean of realized y1−y0 (§1.3 Prohibition, §1.5).

## 3. Per-estimand
| Estimand | Truth tier | Conditioning pop. | FS vs superpop | Eval sample |
|---|---|---|---|---|
| ATT | Structural (§1.3) | treated | finite-sample mean of τ(X) over treated | full generated run |
| ATE | Structural (§1.5) | all | finite-sample over declared scoring pop = full run by default; superpop only if DGP declares (§1.5) | full generated run (contracts, Wave 1 pins ATE to full run sample) |
| QST | Distributional (§1.4) | treated | superpopulation oracle, MC N=10⁶ | canonical grid u∈{0.01..0.99} (§1.4, §3.2.A) |
| CATE | Structural (§1.6) | held-out eval | finite-sample unit-level τ(Xᵢ) | second governed held-out draw (§2.2); Wave 1 = `target_not_implemented` (§1.7) |

## 4. Estimand appropriateness per DGP (NEW)
Scalar-mean ATT/ATE are **well-posed** (population target finite) on: `baseline`,
`qte1`, `nonlinear_heteroskedastic`, `overlap_stressed`, `tilt_mild`,
`hd_sparse_plm`, and all Gaussian placebos. `qte1` is well-posed but
*scientifically blind*: mean-ATT masks a sign flip; QST is the required "X-ray"
(qte1.Rmd §2,§7).

**Heavytail (the point):** `synth_heavytail` / `synth_placebo_heavytail` noise
`0.8·N(0,0.5)+0.2·Cauchy(0,1)` has **undefined mean** (heavytail.Rmd §4 "Moment
note"). The narratives make mean-based *estimation* **deliberately ill-posed**:
"any L2/MSE estimator is not well-posed… variance explosion" (§2,§8);
placebo_heavytail.Rmd §2 names **OLS, IPW, ATE** — "the sample mean will never
converge. Only Robust Estimators (Median/Quantile treatment effects) are
theoretically valid." **QST is the intended robust target.**

**DIVERGENCE (flag):** (a) The *ATT/ATE truth still exists* — it is the finite
structural mean of τ(X), "well-defined even when E[Y] is not" (heavytail.Rmd §5).
So "ill-posed" is precise only about mean-based *estimator convergence*, not the
target — the triage shorthand blurs this. (b) Governing text signals the robust
estimand **only in prose** (narratives; Registry §2.2 "Penalize L2/MSE"). The
**machine-readable** `stress_profile.target` is **`"both"`** for both heavytail
DGPs (sidecars) — it does *not* single out QST or flag ATT/ATE ill-posed. The
Constitution registry (§1.7) is DGP-agnostic; the Families-spec machinery that
*would* label ill-posed regimes (`moment_regime.mean_exists/att_estimable`, Art.
VI/VIII) is unshipped/deferred (README) and absent from the point DGP. So nothing
structurally prevents a user naively ranking a no-mean ATT.
