# Phase 1 Reconstruction — Lane: ESTIMANDS

**Reviewer:** isolated, design-docs only. **Date:** 2026-07-24.

## 1. The scientific claim

CausalStress is designed to support claims of the form: *estimator E, given only sanitized observational data, attains measured error/coverage/robustness against exactly known, versioned, immutable ground truth for a specific typed estimand target, under named stress conditions* — reproducibly across time, machines, and R versions (Constitution Preamble; Art. II). Typed no-cross-scoring (Art. I §1.7) guarantees each output is compared only to its own target's truth; the placebo gatekeeper (Art. IV) supports the companion claim that an estimator does or does not hallucinate effects under an exact sharp null.

## 2. Required assumptions

- Two-tier truth: structural τ(X) is deterministic in X only — never W, p(X), or realized noise (Art. I §1.3, §1.5).
- Synthetic truth is exactly computable (analytic or oracle MC N=10^6, precision <1e-5; §1.4); noise independent across units.
- Immutability + RNG stationarity: frozen (dgp_id, version); same-substrate bitwise identity, cross-substrate tolerance (Art. II §2.1–2.2; Art. VII).
- Airlock honesty: estimators never see y0/y1/p/structural_te; non-adversarial estimator code (Art. III §3.1; contracts.md "Security Scope").
- Placebo sharp null via pathwise identity Y1 ≡ Y0 (Art. IV §4.1).
- Real-data truth is external ("Stabilized Experimental Estimate"), never regenerated (§1.3 "Real Data"); binary, static treatment only (horizon.md exclusions).
- Estimators declare (estimand, target_population); unscoreable requests become explicit non-comparable records (§1.7; contracts.md "Estimator Contract").

## 3. Per-estimand population/target

- **ATT** — Structural tier; population-scalar; conditioning population = treated units; **finite-sample** average of τ(X_i) over realized treated units in the generated run sample; evaluated in-sample on that same run sample (Const. §1.3, §1.7 table; contracts.md Wave 1: scoreable, population id "treated").
- **ATE** — Structural tier; population-scalar; all units; **finite-sample** over the declared scoring population, defaulting to the full generated run sample ("matching the finite-sample convention of §1.3"); superpopulation ATE only if a DGP explicitly declares oracle support and the descriptor distinguishes it. Wave 1 pins scoring to the full generated run sample, in-sample (Const. §1.5; contracts.md "Wave 1 target support").
- **QST** — Distributional tier (signal+noise); curve on fixed grid u∈{0.01…0.99}; conditioning = treated (W=1); truth is **superpopulation-level** (oracle MC N=10^6 or analytic), while the estimator's curve is computed on the run sample — a deliberate tier asymmetry (Const. §1.4, §1.7; contracts.md Runner Output Contract).
- **CATE** — Structural tier; unit-level conditional-mean τ(X_i), never realized Y1−Y0; evaluated on a **held-out** evaluation sample — a second governed, seeded, truth-bearing draw keyed by runner-issued unit_id (Const. §1.6, §2.2 "Held-out Evaluation Samples"; RFC-1 synthesis §1.7). Finite-sample per-unit truth (meta$structural_te). Unimplemented in v0.2.0: resolves to `target_not_implemented` (§1.7 staged rule; roadmap.md).
