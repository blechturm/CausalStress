# Phase 0 — Triage: Adversarial Scientific Design Review

**Date:** 2026-07-24
**Orchestrator:** Claude (governed review; findings routed via action classes, never applied)
**Git HEAD:** `9c16cd2d6a8915aef808bb3c865b8224778fa227` ("Replace qs persistence with atomic RDS")
**Worktree status at start (pre-existing, NOT produced by this review; left untouched):**

```text
 M inst/design/README.md
 M inst/design/causalstress_v0_2_0_ci_packet/release_closeout.md
 M inst/design/causalstress_v0_2_0_correction_packet/release_closeout.md
```

**Audit-directory discrepancy:** the review brief states `inst/design/audit/` contains
`code-simplicity-audit.md`. It does not. Pre-existing contents are
`governance_spec_packet_proposal.md` and `v0_1_9_deep_code_review_audit.md`; both are
treated as read-only. No pre-existing file in this directory is modified by this review.

## Claim under review

> CausalStress's scientific design supports its intended claim: identifying where causal
> estimators succeed, degrade, and fail under controlled synthetic stress, without
> misleading comparisons across estimands, populations, failures, or DGPs.

Tier separation maintained throughout:
`Reproducible + contract-compliant ≠ scientifically valid benchmark ≠ externally representative benchmark`.
Contract compliance is established elsewhere and out of scope; this review adjudicates
tiers two and three only.

## Map of the scientific surface

- **Estimands & truth** (Constitution v2.0.1 Art. I): ATT — structural, treated,
  finite-sample (§1.3); ATE — structural, all units, finite-sample default with declared
  scoring population (§1.5); QST — distributional (signal+noise), treated, canonical grid
  u∈{0.01..0.99}, oracle N=10^6 immutable (§1.4); CATE — unit-level structural τ(X_i),
  held-out evaluation, **registered but not implemented** (`target_not_implemented`)
  (§1.6, §1.7). Typed scoring = `requested ∩ produced ∩ truth-available`; cross-scoring
  prohibited (§1.7). Truth code: `R/cs-truth.R`, `R/cs-oracle-truth.R`; scoring:
  `R/cs-contracts.R` (`cs_build_score_surface`), `R/cs-runner.R`.
- **DGPs** (Art. VII; `R/dgp-synth-*.R`; registry `R/cs-dgp-registry.R`): 12 IDs,
  24 immutable versions, all `type="synthetic"`. Stable: `synth_baseline`,
  `synth_heavytail` (v1.6.0). Ten IDs `experimental` — "Pending human validation".
  Mechanisms present: heavy tails, sharp-null placebos (4), nonlinearity/
  heteroskedasticity, overlap stress, tilted propensities, Kang–Schafer
  misspecification, high-dim sparse PLM. YAML sidecars declare noise/effect profiles.
- **Scoring/aggregation/failure accounting** (`contracts.md` Runner Output Contract):
  long score surface (schema 4) with `score_status ∈ {scored, non_comparable,
  estimator_error, runner_error}` and machine-readable `non_comparable_reason`;
  `success=FALSE` result rows; batch error reconciliation
  `length(results)+nrow(errors)==nrow(tasks)`; low-bootstrap CI policy (<90% replicates
  → CI NA + success=FALSE). Summaries: `R/cs-summary.R`, `R/cs-collect.R`,
  `R/cs-tidy*.R`, gatekeeper `R/cs-gatekeeper.R`.
- **Gatekeeper** (Art. IV): sharp-null pathwise Y1≡Y0 placebo suite; ATT 95% CI covers 0
  in ≥90% of runs; QST "10/10 rule" (>9 of 99 pointwise exclusions = null-rejection run;
  ≤10% run rate); CI-less estimators labeled "Unverified"; ATE/CATE gate components
  deferred to a recalibration RFC (§4.2.5).
- **Proposed CATE design** (RFC-1 synthesis + Constitution §1.6/§2.2/§3.1): held-out
  second governed draw, unit_id-keyed predictions, PEHE + robust companion,
  heterogeneity-detection gate; staged to a future wave; v0.3.0 ordering vs.
  parameterized families is an open maintainer decision (roadmap.md "Planning gate").

## Triage of the review questions

| # | Question | Type | Lane |
|---|---|---|---|
| 1 | ATT/ATE/QST/CATE defined and scored correctly | DEFINITIONAL (+EMPIRICAL shadow: oracle coverage, truth recomputation) | 1 — estimands |
| 2 | DGPs scientifically meaningful vs. merely contract-compliant | DEFINITIONAL (+EMPIRICAL shadow: do stress dials actually produce the claimed pathology) | 2 — dgp |
| 3 | Span of failure mechanisms | DEFINITIONAL (coverage argument); gaps list feeds VALUE decision | 2 — dgp |
| 4 | Fair comparisons across estimands/populations/DGPs | DEFINITIONAL (no-cross-scoring coherence) + EMPIRICAL (score-surface behavior) | 1 (definitions) & 3 (aggregation) |
| 5 | Metrics, uncertainty quantification, aggregation | EMPIRICAL | 3 — statistics |
| 6 | Gatekeeper: robustness vs. punishing caution | EMPIRICAL (conservative-estimator probe) | 3 — statistics |
| 7 | Failure/missingness handling; survivorship bias | EMPIRICAL (failure-injection probe) | 3 — statistics |
| 8 | Which conclusions are externally meaningful | DEFINITIONAL; consolidated in synthesis scope statement (generic synthetic-benchmark limits stated once, in the packet) | all → synthesis |
| 9 | Families vs. CATE next | **VALUE — never adjudicated by agents**; lanes may contribute decision inputs only | routed to maintainer |

## Lane assignments

- **Lane 1 — estimands:** truth definitions, conditioning populations, no-cross-scoring,
  QST interpretation, finite-sample vs. superpopulation, proposed CATE design.
- **Lane 2 — dgp:** stress-mechanism meaningfulness, stable/experimental defensibility,
  missing pathologies, whether parameterized families are needed for breakdown curves.
- **Lane 3 — statistics:** metrics, MC design, CI coverage/calibration, aggregation,
  failure denominators, missingness, ranking stability, gatekeeper behavior.

## Process & environment notes for all reviewers

- Evidence standard: every claim labeled EMPIRICAL / DEFINITIONAL / VALUE; empirical
  claims need a runnable check (run if ≲5 min, else filed with cost estimate);
  unchecked empirical claims are CONJECTURE and support no verdict. Checks outrank
  opinions. Protection claims require a cited clause.
- Write boundary: only `inst/design/audit/` and `tempdir()`. No code/test/spec edits,
  no staging, no commits.
- Runnable environment: `"C:\Program Files\R\R-4.5.2\bin\x64\Rscript.exe"` with
  `pkgload::load_all(".", quiet=TRUE)`; dependencies live in the user library
  (`win-library/4.5`). R 4.6.0 lacks the dependency library — do not use it. Set
  `NOT_CRAN=true` where tests are invoked; always set explicit seeds.
- Isolation: reviewer agents run in fresh contexts. Phase 1 = docs only
  (`inst/design/`), no code, no artifacts, no cross-visibility. Phase 2 = full repo +
  own Phase 1 + this triage. Phase 3 = other lanes' Phase 2 reports. Isolation is
  enforced by instruction and fresh contexts, not by filesystem sandbox; recorded as a
  review limitation.
