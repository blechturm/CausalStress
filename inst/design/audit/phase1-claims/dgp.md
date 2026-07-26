# Phase 1 Claim Reconstruction — DGP Lane

## 1. The claim

CausalStress presents itself as a governed scientific instrument, not merely a library (Constitution, Preamble): it locates where causal estimators succeed, degrade, and fail by scoring them against unambiguous, versioned, reproducible truth — an "integrity-first laboratory for governed stress mechanisms and estimator failure boundaries" (horizon.md, ACIC 2026 entry). The synthetic DGP suite is the truth-bearing substrate: each synthetic DGP emits both potential outcomes and the structural effect vector, so two-tier truth (structural / distributional) is exactly known rather than estimated (Constitution Art. I; DGP Registry 1.4.0 §1.1); each registry entry isolates one named pathology axis (Registry Quick Reference, "Challenge" column); the sharp-null placebo suite makes hallucinated effects detectable (Art. IV §4.1); and parameterized families are designed to turn point comparisons into continuous breakdown ("kill-plot") curves (Families Spec v3.2 §§1.1–1.2).

## 2. Assumptions required

- Truth is estimator-independent and unambiguous: τ(X) deterministic in X only; QST via 10^6 oracle MC or analytic equivalent (Const. §§1.3–1.4; Families Spec Art. III/IV).
- Frozen substrate: immutable `(dgp_id, version)`, locked RNG, same-substrate bitwise identity (Const. Arts. II, VII).
- No truth leakage: airlock strips `y0`,`y1`,`p`,`structural_te`; holds only for honest estimators (Const. §3.1; contracts.md "Security Scope").
- No cross-scoring: outputs scored only against matching target truth; mismatches are explicit non-comparable rows (Const. §1.7; contracts.md).
- Failure attribution presumes each DGP's declared axis is the operative difficulty; placebo pathwise identity gives zero-variance null (Registry §3; Const. §4.1).
- Findings are conditional on DGPs resembling reality (horizon.md coverage-map guardrail); denominators must be survivorship-honest (Families Spec §4.3; horizon.md runner-integrity item 6).

## 3. Estimand populations and targets

| Estimand | Truth tier | Level | Conditioning population | Finite-sample vs. superpopulation |
|---|---|---|---|---|
| ATT | Structural (noise-free τ(X)) | population-scalar | treated units of the realized draw | finite-sample: (1/N_treated)Σ_{W=1}τ(X_i) (Const. §1.3, §1.7) |
| ATE | Structural | population-scalar | all units, full generated run sample (`mean(meta$structural_te)`) | finite-sample default; superpopulation only if explicitly declared and distinguished (Const. §1.5; v0_2_0_spec "ATE truth and scoring"; contracts.md Wave 1 pin) |
| QST | Distributional (signal+noise) | curve, grid u∈{0.01,…,0.99} | treated (QTT-style: Q_u(Y_1∣W=1)−Q_u(Y_0∣W=1)) | superpopulation-flavored: oracle over the population law of X∣W=1, not the realized sample (Const. §1.4; Registry §1.1; treated-vs-population hazard flagged in horizon.md quantile-axis entry) |
| CATE | Structural, conditional-mean τ(X_i) — never realized Y_1−Y_0 | unit-level | held-out evaluation sample, a second governed finite draw keyed by `unit_id` | finite-sample; registered but unimplemented in v0.2.0 (`target_not_implemented`) (Const. §§1.6–1.7, §2.2; roadmap.md) |

Real-data DGPs: external ATT truth only (or NA); other estimands non-comparable, never regenerated (Const. §1.3 "Real Data", §1.7).
