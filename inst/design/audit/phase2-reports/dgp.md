# Phase 2 — Lane 2: Simulation & DGP Design

**Authorship note:** completed by the orchestrator after the isolated Phase-2
agent was credit-terminated (it left the seeded check script
`lane2_pathology_checks.R`, which aborts on the `include_truth=FALSE` generator
bug in F6; corrected as `lane2_pathology_v2.R`, run and captured). Isolation
compromised at Phase 2 — recorded limitation. All empirical claims cite
`checks/lane2_pathology_v2_output.txt`.

## 1. Reconstructed claim (carried, one revision)

The synthetic suite is the truth-bearing substrate: each DGP emits both potential
outcomes so two-tier truth is exactly known, each entry isolates a named pathology
axis, placebos catch hallucination, and families would extend points to breakdown
curves. **Revision:** Phase-2 evidence shows the "one named axis per DGP"
assumption is *partly* false — some stress DGPs couple several axes (F1), and one
naming/severity label is inverted (F2).

## 2. What is scientifically sound

- **Placebo integrity is exact.** `lane2_pathology_v2` §B: all five placebos have
  `identical(y0,y1)=TRUE`, `max|τ|=0`, `true_att=0`, `max|true_qst|=0`. The sharp
  null is pathwise (Const. §4.1; Registry line 269). **EMPIRICAL: sound.**
- **Difficulty genuinely separates.** §A naive-bias ladder: baseline 0.13 →
  heavytail 0.72 → hd_sparse 3.67 → overlap 1.71 → kangschafer −19.7. Stress DGPs
  are materially harder than the sanity baseline — not cosmetic. **EMPIRICAL: sound.**
- **Headline pathologies are real.** overlap_stressed: 36% of p<0.01, control
  ESS/n = 0.0097, sd(logit p)=12.5 (severe positivity failure); heavytail: excess
  kurtosis 3774, max|ε|≈995 (Cauchy-mixture tails); kangschafer: OLS-in-X ATT bias
  −7.0 on a truth-0 DGP (severe misspecification); qte1 sign-flip τ=±1 by X1
  exact. **EMPIRICAL: sound** — these are meaningful mechanisms, not
  contract-compliant noise.

## 3. Findings

**F1 — `nonlinear_heteroskedastic` couples ≥3 axes; failure is not attributable to one mechanism.**
Type **EMPIRICAL** (`lane2_pathology_v2` §D): cor(σ(X),p)=−0.65, cor(μ₀(X),logit p)
=0.36 — nonlinearity, heteroskedasticity, and selection all move together through
X. Contradicts the "one named axis per DGP" premise (Registry Quick Reference
"Challenge" column). Tier: **scientifically valid** (attribution of estimator
failure). v0.3.0: this is the core argument *for* parameterized families — they
vary one dial at a time to produce interpretable breakdown curves. Action:
**documentation only** (relabel as compound-stress) now; **DGP scientific
validation** later.

**F2 — `synth_tilt_mild` selection is *weaker* than the sanity baseline.**
Type **EMPIRICAL** (§A, §G): sd(logit p) = 0.607 (tilt_mild) < 0.715 (baseline).
The named "tilt" severity sits below the baseline sanity check, so a
baseline→tilt→overlap "ladder" is non-monotone. Tier: **valid** (interpretability
/ mislabeled severity). v0.3.0: reinforces families (a governed dial would make
severity monotone by construction). Action: **DGP scientific validation** (or
documentation of the true ordering).

**F3 — `hd_sparse_plm` violates the Registry's noise-independence rule and has a degenerate QST.**
Type **EMPIRICAL + DEFINITIONAL**. Registry 1.4.0 §4 (line 48): "ε₀, ε₁ are
**independent** draws" for all non-placebo DGPs. `lane2_pathology_v2` §E:
`hd_sparse_plm` has sd(y1−y0−τ)=**0** — ε₁≡ε₀ (shared within-unit noise), a
constant +1 shift. Consequence: its QST truth is flat +1 at all τ (Registry line
257) — QST is **degenerate** on this DGP and exercises no quantile heterogeneity.
Tier: **valid** (a governing-clause contradiction, and a non-informative QST
target). Action: **DGP scientific validation** (new version restoring independent
ε₁, or an explicit Registry carve-out documenting the constant-shift choice).

**F4 — Ten of twelve DGP IDs are self-labeled scientifically unvalidated.**
Type **DEFINITIONAL** (`R/cs-dgp-registry.R` status column: only `synth_baseline`,
`synth_heavytail` are `stable`; the other ten are `experimental` /
"Pending human validation"; README scientific-status disclaimer concurs). The
breadth of the intended claim rests on DGPs whose scientific interpretation the
project itself has not signed off. Tier: **valid** (for the 10) and **externally
representative**. v0.3.0: argues foundational **DGP scientific validation** should
precede or accompany families/CATE. Action: **DGP scientific validation**.

**F5 — The suite spans stress *within* correct identification only.**
Type **DEFINITIONAL**: every DGP selects on observables (unconfoundedness holds
given X); none injects unmeasured confounding, measurement error, missing data, or
clustered/dependent units. The instrument measures estimator behavior *assuming
identification holds*; it cannot speak to robustness under identification failure.
Tier: primarily **externally representative** (and **valid** if a user over-reads
"where estimators fail" as including identification failure). Action:
**documentation only** (scope statement) + **future spec ticket** (candidate new
DGP families). This is a scope boundary, not a defect.

**F6 — `include_truth=FALSE` aborts on 5/12 generators (consistency defect).**
Type **EMPIRICAL** (`lane2_pathology_v2` §H): `heavytail_v160`, `qte1_v160`,
`tilt_mild_v160`, and the four v1.3.0/v1.4.0 placebos abort ("`true_qst` must be a
tibble") because they call `cs_check_dgp_synthetic()` on a NULL `true_qst`;
`baseline_v160`, `nonlinear_heteroskedastic`, `overlap_stressed`, `hd_sparse` do
not. Tier: **neither** — a data-only-generation robustness inconsistency,
tangential to scientific validity (production always uses `include_truth=TRUE`).
Action: **future spec ticket** (or documentation that `include_truth=FALSE` is
unsupported).

## 4. No material finding

- **Placebo suite** (exact sharp null) and **difficulty separation** hold up
  cleanly (see §2).
- **Immutable versioning / frozen `(dgp_id,version)`** is a governance strength,
  not challenged here.
- **overlap_stressed, heavytail, kangschafer, qte1** each produce their headline
  pathology as declared — no material gap between claim and behavior for these.
