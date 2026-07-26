# RFC Seed: Estimand Registry — Typed Estimands, ATE + CATE, and the Scoring Join

**Status:** Seed (thesis stage; awaiting adversarial response per `rfc_cycle.md`)
**Date:** 2026-06-16
**RFC:** RFC-1 of the v0.2.0 estimand/UX/families program
**Authors:** Max Thomasberger (maintainer), drafted with assistant
**Sources:** `CAUSAL_STRESS_CONSTITUTION.md` v1.8.2 (Art. I, III, IV); `contracts.md`; `horizon.md` "Estimand expansion" entry (parked 2026-06-12); `research/prior_art_killplot_heavytail_benchmarking.md`; v0.1.9 audit finding C4; archived `CAUSALSTRESS_DESIGN_v0.3.0.md` (deferred `target_level="unit"` sketch).

## Process note

This RFC starts at the **seed** stage. The audit findings (esp. C4), the prior-art research note, and the horizon "Estimand expansion" entry serve as accumulated input. Per `rfc_cycle.md`, the next stage is an adversarial **response** by a different author. No implementation begins until a synthesis is accepted and the implied Constitution amendment is ratified.

This RFC is the **linchpin** of a three-RFC program: **RFC-2 (UX Convergence + API Stability)** will freeze the public surface *over the estimand contract defined here*, and **RFC-3 (Families Reconciliation)** consumes the per-estimand validity model defined here. Decisions made here therefore propagate; that is intentional and is why this RFC goes first.

---

## 1. Problem statement

CausalStress benchmarks exactly two estimands — structural ATT (Art. I §1.3) and distributional QST (Art. I §1.4). Three problems follow:

1. **Field relevance.** The field's center of gravity is **CATE** (unit-level τ(X)) — confirmed in `research/prior_art_killplot_heavytail_benchmarking.md` (the CATE-benchmarking literature: Curth & van der Schaar 2021; CATENets; the EMCS studies). An instrument that cannot score CATE reads as partial to its target audience. ATE (the unconditional mean) is also a standard target the package cannot currently score.
2. **The estimand-mismatch defect (audit C4).** `est_tmle_att` natively targets ATE; the package had only ATT truth to score against, so it silently scored an ATE estimate as ATT. The one-line fix landed, but the *defect class* — an estimator's output scored against the wrong estimand's truth — remains structurally possible because there is no first-class notion of "which estimand did this estimator target."
3. **No shared estimand abstraction.** The estimator registry encodes capability as a single boolean `supports_qst` plus an `oracle` flag. There is no typed estimand identity, so estimand selection does not enter run identity (fingerprints), the gatekeeper hardcodes ATT/QST in three blocks, and downstream collectors/plots hardcode `att`/`qst` columns.

## 2. Goals / non-goals

**Goals.**
- Define a typed, governed **estimand abstraction** (`estimand_target`) covering ATT, QST, **ATE**, **CATE**.
- Make estimator output **scored against its declared estimand's truth** (the "scoring join"), eliminating the C4 defect class structurally.
- Generalize the gatekeeper (Art. IV) to be **per-estimand pluggable**, with a CATE placebo gate.
- Define CATE truth + metric (PEHE) and an honest position on heavy-tail metric validity.
- Specify the **constitutional amendment** (Art. I, Art. IV) and the **fingerprint/identity** change implied by estimand selection.

**Non-goals (explicitly deferred).**
- The public-API convergence and freeze (RFC-2).
- DGP families / λ-sweeps / kill-plots (RFC-3) — but this RFC must define the per-regime validity hook they consume.
- QTE of `Y1−Y0` (distribution of individual effects; not identified without rank invariance — incompatible with the unambiguous-truth principle).
- LATE/IV, mediation, survival estimands (different identification machinery; a later package era).
- GenGC bootstrap-CI validity (RFC-4, cross-repo).

## 3. Core proposal — the estimand abstraction

### 3.1 The estimand set (v0.2.0)

| Estimand | target_level | Definition | Truth source | Cost |
| --- | --- | --- | --- | --- |
| **ATT** (existing) | population (treated) | E[Y¹−Y⁰ ∣ W=1] | `mean(structural_te[w==1])` | already shipped |
| **ATE** (new) | population (all) | E[Y¹−Y⁰] | `mean(structural_te)` | **near-free** (second scalar) |
| **QST** (existing) | distributional (treated) | Qᵤ(Y¹∣W=1) − Qᵤ(Y⁰∣W=1) on canonical grid | oracle MC | already shipped |
| **CATE** (new) | unit | τ(Xᵢ) = E[Y¹−Y⁰ ∣ Xᵢ] | `structural_te` per unit (the noise-free structural effect) | **truth free**; metric + schema are the work |

Key observation: **all four reuse `structural_te`.** ATT/ATE are its averages (over treated / all); CATE is the per-unit vector itself; QST is the only one needing the oracle. CATE truth is therefore the *noise-free structural* effect, consistent with Art. I §1.3 (truth on the structural component).

### 3.2 Estimand identity (`estimand_target`)

Introduce a typed identifier carried through the run lifecycle:

```
estimand_target := { id: "att"|"ate"|"qst"|"cate", target_level: "population"|"unit", conditioning: "treated"|"all" }
```

A run *requests* one or more estimand targets; an estimator *declares* which it produces; the runner *scores each produced target against that target's truth* and marks any unmatched request **non-comparable** (never silently cross-scored). This is the **scoring join**.

### 3.3 Estimator capability matrix (replaces `supports_qst`)

The estimator registry's `supports_qst` boolean + `oracle` flag generalize to a capability declaration: the set of `estimand_target`s each estimator produces. Several existing estimators already compute CATE internally (`grf` causal forest, `bart`) and currently *collapse* it to ATT — exposing CATE largely **un-collapses what exists**.

### 3.4 Metrics

- **ATE:** bias, RMSE, coverage, CI width (same as ATT).
- **CATE:** primary metric **PEHE** = √(mean((τ̂(Xᵢ) − structural_teᵢ)²)); plus per-unit calibration. *(Heavy-tail caveat — see Open Question Q5.)*
- Per-unit predictions are large (n × seeds × estimators); **default: store the scored metric only; raw τ̂(X) opt-in** (proposed; see Q6).

### 3.5 Gatekeeper generalization (Art. IV)

Replace the three hardcoded blocks with a **per-estimand gatekeeper interface**. CATE gate: on a placebo (τ≡0), any estimated heterogeneity is hallucination — an estimator passes if τ̂(Xᵢ) ≈ 0 within tolerance (or its per-unit CI includes 0) across the placebo suite. The sharp-null placebo suite is **more discriminating for CATE than for ATT** and is a headline asset of this amendment.

### 3.6 Fingerprint / identity

`estimand_target` enters run identity: a CATE run and an ATT run on the same `(dgp, est, n, seed)` are **different runs**. This implies a schema-3 → **schema-4 fingerprint bump** (additive field), with deterministic legacy comparison preserved (the established pattern). *(See Q7 — whether to bump or treat absent target as "att" for back-compat.)*

## 4. Proposed constitutional amendment

**Article I.** Keep the **two truth tiers** (structural, distributional) but (a) enumerate the estimands within them, (b) add the **population/unit axis**, and (c) state that ATE and CATE are derived from the same noise-free structural component `structural_te` already mandated. *Rationale:* this is additive — it does not change the meaning of ATT or QST — which bears on the version-bump question (Q8).

**Article IV.** Reword §4.2 scope from "exclusively ATT and QST" to "the estimands defined in Article I," and define the per-estimand pass criteria (ATT CI-includes-0 ≥90%; QST 10/10 rule unchanged; **CATE placebo-heterogeneity gate** added).

## 5. Downstream consumers (forward references)

- **RFC-2 (UX freeze):** the converged collectors/plots/runners must host `estimand_target`; `cs_collect_cate()` and an aggregated CATE plot are part of the surface RFC-2 will freeze. **RFC-2 cannot freeze until this contract is accepted.**
- **RFC-3 (families):** the per-estimand **validity hook** (which estimands/metrics are well-posed under a given DGP/λ regime — e.g. PEHE is unstable under infinite-variance noise) is defined here and consumed by the families' moment-regime model. This is the bridge between the estimand registry and the kill-plot.

## 6. Open questions / decision options (for the response stage)

- **Q1 — CATE truth = structural (noise-free) τ(X), confirmed?** PEHE is conventionally scored against the conditional mean effect = structural τ(X). Confirm this is the truth (vs the realized individual effect Y¹−Y⁰, which we reject). Any benchmark-convention mismatch a reviewer can cite?
- **Q2 — `estimand_target` shape.** A flat enum (`"att"|"ate"|"qst"|"cate"`) vs the structured `{id, target_level, conditioning}` triple. The triple is more extensible (LATE later) but heavier. Which?
- **Q3 — Multi-estimand runs.** Should one run request *multiple* estimands (e.g. ATT + CATE from a causal forest in one fit) and emit several scored rows, or is one run = one estimand? Multi is efficient (one fit) but complicates identity and storage.
- **Q4 — Capability matrix vs keep-it-simple.** Full matrix (estimator × estimand × eligibility-fn) vs a minimal `produces = c(...)` vector now and matrix later. Risk of under- vs over-engineering the registry.
- **Q5 — PEHE under heavy tails (couples to RFC-3).** PEHE squares errors → unstable/infinite under infinite-variance estimator error. Do we (a) define a robust CATE metric now (median-PEHE / calibration), (b) defer it to RFC-3's moment-regime model, or (c) ship PEHE with a validity flag? This is the CATE analog of the ATT-under-Cauchy kill-plot result.
- **Q6 — Storage of per-unit τ̂(X).** Summary-metric-only by default with raw opt-in (proposed) vs always store raw (auditability) vs never (size). Batch-schema bump required either way.
- **Q7 — Fingerprint back-compat.** Bump schema-3 → schema-4 with `estimand_target`, OR treat an absent target as `"att"` so existing pins resume unchanged. Trade reproducibility-clarity vs migration cost.
- **Q8 — Constitution version bump.** Is adding estimands "additive" (minor, e.g. 1.9.0) because it doesn't change ATT/QST semantics, or a "semantic change to an Article" (major, 2.0.0) per the Constitution's own amendment rule? This is a genuine governance ambiguity.
- **Q9 — Gatekeeper for ATE.** ATT has a placebo gate; does ATE need its own, or does the ATT gate suffice (since placebo ATE = ATT = 0)? And does CATE's gate subsume the scalar gates (a CATE estimator that passes the per-unit-zero gate trivially passes ATT)?
- **Q10 — Scope creep risk.** Is four estimands the right v0.2.0 cut, or should CATE alone ship first (ATE is trivial; CATE is the hard, field-relevant one) to de-risk?

## 7. Rejected alternatives (preliminary — reviewer should attack)

- **Add CATE without the typed abstraction** (just a `cate` column). Rejected: reproduces the C4 defect class for the next estimand and gives families no validity hook.
- **Make ATT a special case of CATE** (always compute unit-level, average for ATT). Rejected for v0.2.0: forces every estimator into unit-level output; many (IPW, lm) are natively scalar.
- **Distribution of individual effects (quantiles of Y¹−Y⁰).** Rejected: not identified without rank invariance — violates the unambiguous-truth principle.

## 8. Acceptance criteria for this RFC

The synthesis is acceptable when it: (a) fixes the estimand set and `estimand_target` shape; (b) resolves the scoring-join and capability-matrix design; (c) states the CATE metric position (incl. heavy-tail validity); (d) gives ratifiable Article I + IV amendment text and a version-bump decision; (e) decides the fingerprint/storage schema changes; and (f) confirms the RFC-2/RFC-3 interfaces (the surface to be frozen; the validity hook). Implementation does not begin until the synthesis is accepted **and** the Constitution amendment is ratified.
