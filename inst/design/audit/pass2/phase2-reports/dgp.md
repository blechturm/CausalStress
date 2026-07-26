# PASS 2 · Phase 2 · Lane DGP — Intent-First Scientific Review

**Reviewer:** isolated DGP lane (full repo). **Git HEAD context:** v0.2.0 branch.
**Evidence:** `inst/design/audit/pass2/checks/p2_lane2_dgp_intent.R`
(+ `_output.txt`, seeds fixed), building on the salvaged
`inst/design/audit/checks/lane2_pathology_v2.R` (+ `_output.txt`).
All 12 generators read at source; every `inst/dgp_meta/<id>.yml` + `<id>.Rmd`
read before classifying. Labels: EMPIRICAL / DEFINITIONAL / VALUE.

---

## 1. Reconstructed claim

CausalStress supplies a 12-DGP synthetic substrate (7 signal-bearing + 5 sharp-null
placebos) that *locates where estimators succeed, degrade, and fail under one or
more controlled stress axes, without misleading comparisons across estimands or
DGPs.* Each DGP declares a `stress_profile` sidecar + a narrative (`§2` intent),
carries immutable oracle truth (structural ATT on the deterministic signal `τ(X)`,
Const. §1.3; QST on the realized distribution via 10⁶ oracle, Const. §1.4), and a
1–5★ difficulty. Placebos (Const. Art. IV / Registry §3) gate hallucination via a
sharp null. The intent baseline against which behavior is judged is the
sidecar tag + narrative §2, **not** naive expectation. Per triage, sidecars +
narratives are authoritative; the frozen Registry 1.4.0 is not.

---

## 2. What is scientifically sound (with evidence)

Adjudication rule (triage §"Intent-first"): a behavior fulfilling a DGP's declared
intent is SOUND even when it looks pathological.

**2.1 heavytail — SOUND-by-intent (re-adjudication, pass-1 got this wrong).**
Noise is `0.8·N(0,0.5) + 0.2·Cauchy(0,1)` (`R/dgp-synth-heavytail.R` l.55-65;
narrative §4 l.139-147 "the population mean of realized outcomes is undefined, and
any L2/MSE estimator is not well-posed"). The mean-based ATT instability pass-1
flagged as a defect **is the declared "L2 break" demonstration**. p2 check **S2**
proves the mechanism directly: the across-seed spread of the naive mean-ATT does
**not** shrink with n for heavytail (sd = 9.13 → 1.88 → 9.62 at n = 1k/5k/20k;
range up to 53), whereas baseline shrinks ~1/√n (0.105 → 0.035 → 0.024) — the
sample mean does not converge. Crucially, the **truths remain well-posed**: the
structural ATT (signal-only `τ(X)=1+0.5X₁`) is finite (1.119) and the QST curve has
no NA (quantiles exist for Cauchy). So only mean-based *estimators* are ill-posed;
the DGP, its ATT truth, and its QST truth are sound. Grounded by Const. §1.3 l.49
(truth "must never be calculated as the sample mean of realized differences … in
heavy-tailed settings") and narrative §5 l.155-161. EMPIRICAL (S2), DEFINITIONAL.

**2.2 hd_sparse_plm — SOUND-by-intent (re-adjudication).** `τ(X)≡1`,
`Y1 = Y0 + 1` with a single ε (`R/dgp-synth-hd-sparse-plm.R` l.50-51; narrative §4
l.130-138 shows one ε; Registry §2 l.237-239 "Y₁ = Y₀ + 1"). p2 **S3**:
`sd(y1−y0−τ)=0`, `identical(y1, y0+1)=TRUE`, `true_att=1` on every seed. This
constant +1 shift is the **declared design**; it is immaterial to the scored
truths — structural ATT(=1) and marginal QST(+1 at every τ) are identical under
shared or independent ε; only the barred CATE-level paired difference would differ
(Const. §1.3 l.47-49 bars sample-mean-of-differences truth). This is SOUND, with at
most a documentation-consistency note (see F2), **not** a scientific defect.
EMPIRICAL (S3).

**2.3 Placebo integrity (gatekeeper).** All 5 placebos enforce the sharp null at
source (`y1 <- y0`): p2 **S7** confirms `identical(y0,y1)`, `true_att=0`,
`max|true_qst|=0`, `max|structural_te|=0` on every seed. The Kang–Schafer trap is a
genuine misspecification stressor (OLS-in-X ATT bias ≈ −7.0 against a truth of 0;
lane2 §F), not a spurious null. EMPIRICAL (S1, S7; lane2 §B/§F).

**2.4 Difficulty separation is real and graded.** p2 **S1** / lane2 §G give a monotone
overlap ladder by `sd(logit p)`: tilt_mild 0.60 < baseline 0.71 < placebo_tilted
0.99 < hd_sparse 0.95 < kangschafer 1.15 ≪ overlap_stressed 12.2 (ESS_ctrl/n falls
0.71 → 0.63 → … → 0.002; overlap frac(p<.01)=0.36). Each signal DGP hits its
declared axis: qte1 τ=±1 sign-flip is exact with a near-cancelling mean ATT (~0.2)
while half the units are harmed (lane2 §C); overlap positivity is severe;
nonlinear_het couples cubic curvature + heteroskedastic σ. EMPIRICAL (S1, S5; lane2 §C/§D/§G).

**2.5 Stable/experimental split is well-calibrated.** Only `synth_baseline` and
`synth_heavytail` are `stable`; the other 10 are `experimental`. These 10 are
exactly the DGPs carrying recent v1.4–v1.6 hardening churn (overlap 3→9, nonlinear
sin→cubic, hd_sparse p 50→100/ρ 0.5→0.95, placebo_tilted 1.0/1.2→0.6/0.8) — the
same designs the frozen Registry has not caught up to (F1). Marking them
"experimental / pending human validation" is honest and defensible. DEFINITIONAL
(sidecars) + EMPIRICAL (S6).

**2.6 Estimand well-posedness per DGP holds where it matters.** For every scored
(DGP, estimand): the structural ATT target is a deterministic function of `τ(X)`
(Const. §1.3), so it exists even on heavytail; QST is defined on quantiles, which
exist under all 12 noise laws (p2 S2 confirms no-NA QST on the no-mean DGP). The
*population target always exists*; the exposure gap is signaling, not existence (F4).

---

## 3. Findings (5; documentation / metadata / scope — no scientific defect found)

Tiers: **T2** = could mislead a documentation-reading user; **T3** = low / cosmetic /
scope. Action classes: `doc-fix`, `metadata-fix`, `scope-note`.

### F1 — Registry 1.4.0 is stale vs the authoritative sidecars/code · T2 · doc-fix
- **Claim/clause:** Registry §2/§3 "Canonical Parameters" purport to be the DGP
  definitions. `inst/design/CAUSAL_STRESS_DGP_REGISTRY_1.4.0.md` (dated 2025-11-29):
  overlap §2.6 l.140 `plogis(3X₁+3X₂)`; nonlinear_het §2.4 l.114/118/120
  (`μ₀=1+0.8sin X₁+0.5X₂²−0.3X₄`, `σ=0.3+0.2|X₃|`); hd_sparse §2 l.211/216-219/244
  (`p=50`, `Σ=0.5^{|i−j|}`, `γ=(0.5,−0.5,0.25,−0.25,0.1)`); placebo_tilted §3.2
  l.284 `plogis(1.0X₁+1.2X₂)`.
- **Type/evidence:** VALUE/EMPIRICAL. p2 **S6**: overlap realized `sd(logit p)=12.38`
  (code `9X₁+9X₂`; Registry's 3.0 → ~4.24), placebo_tilted `1.00` (code 0.6/0.8;
  Registry's 1.0/1.2 → ~1.56), hd_sparse `#covariates=100` (Registry 50). Source:
  `R/dgp-synth-overlap-stressed.R` l.202, `-hd-sparse-plm.R` l.187-204,
  `-nonlinear-heteroskedastic.R` l.279-291, `-placebo-tilted.R` l.99.
- **Bearing v0.3.0:** a user reading Registry 1.4.0 gets four wrong DGP specs.
  Bump the Registry to ≥1.6 or stamp it "superseded — sidecars/narratives
  authoritative." Sidecars/narratives themselves are correct.

### F2 — Registry ε-independence clause not reconciled with constant-shift DGPs · T3 · doc-fix
- **Claim/clause:** Registry §1.2 l.48 states globally "ε₀, ε₁ are independent
  draws"; the exception list at l.49 names **only** placebos, yet the same document's
  hd_sparse block (l.237-239) declares `Y₁=Y₀+1` (shared ε).
- **Type/evidence:** EMPIRICAL. p2 **S3** (shared noise every seed).
- **Adjudication (mandated):** **SOUND-by-intent, NOT a defect.** Const. §1.4 l.61
  already grants "unless explicitly specified," which authorizes the declared shift;
  and it is immaterial to the scored ATT(=1)/QST(+1) truths. Only the Registry's own
  §1.2 wording lags its §2 hd_sparse block.
- **Bearing v0.3.0:** add "constant-shift synthetic DGPs (`hd_sparse_plm`)" to the
  Registry §1.2 independence-exception list. Pure documentation consistency.

### F3 — "One pathology axis per DGP" is imprecise for the coupled DGPs · T3 · doc-note
- **Claim/clause:** The suite is framed as isolating one axis per DGP (Phase-1 §1;
  Families spec l.309 "to isolate tail robustness"). Two DGPs are deliberately
  multi-axis: `nonlinear_heteroskedastic` (narrative §2 l.101-103 "nonlinear
  confounding **+** severe heteroskedasticity") and `qte1` (sign-flip heterogeneity
  **+** Student-t(4) tails).
- **Type/evidence:** EMPIRICAL. p2 **S5**: `cor(σ(X),p)=−0.658`, `cor(μ₀,logit p)=0.366`
  — σ(X), the cubic/quadratic μ₀, and selection **all** route through X₂
  (`R/dgp-synth-nonlinear-heteroskedastic.R` l.279-291). The heteroskedasticity is
  entangled with treatment assignment; this σ–selection coupling is not stated in
  the narrative.
- **Adjudication:** the *combined* stressor is intended (name + tags + narrative
  §2/§8) → SOUND. The note is that an estimator's failure on this DGP is **not
  separably attributable** to one axis, and the σ–selection entanglement is
  undocumented.
- **Bearing v0.3.0:** label these two as combined/multi-axis in the sidecar/narrative;
  optionally note the σ–selection coupling so users don't read a single-cause story.

### F4 — heavytail-family metadata does not signal mean-ATT ill-posedness · T3 (borderline T2) · metadata-fix
- **Claim/clause:** This is the DGP-lane form of the triage cross-cutting question
  ("does the tool prevent a user from naively ranking a mean-based ATT on a no-mean
  DGP, or tell them to use QST?"). `inst/dgp_meta/synth_heavytail.yml` sets
  `target: "both"` (inviting mean-ATT ranking) and `noise: "heavy"` — the **same**
  tag `synth_qte1.yml` carries, though qte1's Student-t(4) has finite mean and
  variance while heavytail's Cauchy mixture has neither. From the machine-readable
  sidecar alone a user cannot tell which DGP breaks L2.
- **Type/evidence:** EMPIRICAL. p2 **S2**: heavytail mean-ATT does not converge
  (excess kurtosis 1.8e4, max|ε|=1.6e4) while its structural ATT and QST stay
  well-posed. The narrative (§2 l.108-110, §8) *does* say "prefer robust/quantile
  targets"; the sidecar does not. Const. §1.3 l.49.
- **Bearing v0.3.0:** add a machine-readable well-posedness/preferred-estimand flag
  (e.g., `moment_order`, or `att_mean_illposed: true`) distinguishing infinite-
  variance from finite heavy tails, so ranking surfaces can steer to QST. Whether the
  *ranking engine* emits a warning is Lane-1/Lane-3 scope; the DGP-metadata gap is here.

### F5 — Entire suite is selection-on-observables (missing pathology class) · T3 · scope-note
- **Claim/clause:** The headline claim ("identifies where estimators succeed,
  degrade, and fail") is broad; every DGP asserts unconfoundedness + SUTVA
  (narratives §3, e.g. heavytail §3 l.113-114 "Unconfoundedness holds by
  construction"; every experimental DGP §3 "Selection on observables holds / SUTVA
  holds"). No DGP encodes **unmeasured confounding, measurement error, or
  interference/dependent units.**
- **Type/evidence:** DEFINITIONAL (all 12 narratives §3).
- **Adjudication:** SCOPE, not defect — the suite never claims to cover hidden-bias
  or SUTVA-violation failures. But it bounds the claim: the benchmark cannot locate
  failures driven by those mechanisms.
- **Bearing v0.3.0:** document the covered-vs-uncovered failure taxonomy; a
  hidden-bias / interference family is a candidate for a future line.

---

## 4. Items examined and cleared (no material finding)

- **tilt_mild "weaker than baseline" (item e) → SOUND, not mislabeled.** p2 **S4**
  (30 seeds): tilt_mild `sd(logit p)=0.597 < baseline 0.709` — this is *consistent*
  with the sidecar (`overlap: mild` **<** baseline `moderate`) and the Registry
  matrix label "Covariate Shift" (l.16), not a contradiction. Its ★2 (vs baseline
  ★1) is defensible: `|naive bias|=0.287 ≥ baseline 0.228` and the propensity loads
  on X₄ (in `p` but **not** in μ₀/τ; `R/dgp-synth-tilt-mild.R` l.41), a covariate-
  shift axis a naive analyst can miss. Narrative §7/§8 honestly says "mild … most
  methods pass." Cleared.
- **nonlinear_het "smooth" label for a cubic μ₀ → correct.** The schema enum is
  {linear, smooth, discontinuous} (`inst/dgp_meta/schema.yml`); a cubic is smooth
  (non-linear but C^∞). `effect: constant` (τ=1), `noise: heteroskedastic` both
  accurate. Cleared. (The coupling nuance is F3, not a label error.)
- **Stable/experimental defensibility (item b) → sound** (see §2.5).
- **qte1 `noise: "heavy"` for Student-t(4):** loose but within the coarse enum (t₄ is
  heavier-tailed than Gaussian); Registry §2.3 correctly says "Student-t (df=4)."
  The material signaling gap (t₄ finite-variance vs Cauchy no-variance both tagged
  "heavy") is captured in F4; no separate finding.
- **Placebo sharp nulls / Kang–Schafer trap → sound** (see §2.3).
- **Estimand existence per DGP → sound** (see §2.6).

## 5. Generic synthetic-benchmark limits (stated once for the packet)

These bound *every* lane and are not per-DGP findings: (i) truth is model-internal —
oracle correctness does not establish external representativeness (triage tier
"reproducible ≠ externally representative"); (ii) all covariate laws are Gaussian /
uniform / Bernoulli — no skew, discreteness, or real-covariate structure; (iii)
difficulty ★ are author-assigned, not empirically calibrated against the full
estimator set; (iv) results are finite-n at benchmark sizes. These are inherent to a
synthetic stress suite, acknowledged, and not defects.
