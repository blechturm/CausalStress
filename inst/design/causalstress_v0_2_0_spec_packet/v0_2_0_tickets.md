# CausalStress v0.2.0 Tickets

**Status:** ACTIVE
**Packet:** `causalstress_v0_2_0_spec_packet`

## Batch 0 - Contract Baseline

### CS-1200 - Update contract index for typed estimand outputs

- **Batch:** 0
- **Source:** v0.2.0 spec; Constitution v2.0.0; RFC-1
- **Motivation:** Wave 1 changes the estimator output contract. The level-2
  `contracts.md` index must not continue to document only `list(att, qst, meta)`.
- **Files:** `inst/design/contracts.md`
- **Constitutional check:** Articles I and III; contract index remains below the
  Constitution and records code-facing obligations only.
- **Test obligation:** Design review confirms `contracts.md` names typed
  `outputs`/`meta`, the legacy shim, Wave 1 target support, non-comparable
  reasons, and fit/score identity at contract level.
- **Review gate:** Batch 0 contract-baseline review.
- **Disposition:** complete_after_review

## Batch 1 - Typed Scoring Core

### CS-1201 - Implement canonical estimand targets and non-comparable reasons

- **Batch:** 1
- **Source:** RFC-1 estimand target descriptor; v0.2.0 spec
- **Motivation:** Scoring must be keyed by governed targets, not hardcoded ATT/QST
  fields or bare strings.
- **Files:** `R/`, `tests/testthat/`
- **Constitutional check:** Article I target vocabulary and no-cross-scoring rule.
- **Test obligation:** ATT, ATE, QST, and CATE descriptors expose canonical
  fields; compact ids derive deterministically; unknown or misspelled
  non-comparable reasons are rejected.
- **Review gate:** Batch 1 typed-target review.
- **Disposition:** complete_after_review

### CS-1202 - Normalize typed outputs and implement the three-way scoring join

- **Batch:** 1
- **Source:** RFC-1 output contract; v0.2.0 spec
- **Motivation:** Existing estimators must continue to run while new code consumes
  `list(outputs, meta)`, and scoring must be driven by
  `requested` intersect `estimator-produced` intersect `DGP-truth-available`.
- **Files:** `R/cs-contracts.R`, `R/cs-runner.R`, estimator wrappers,
  `tests/testthat/`
- **Constitutional check:** Article III estimator contract.
- **Test obligation:** Legacy `list(att, qst, meta)` outputs normalize to typed
  ATT/QST outputs; scored ATT/QST values are numerically identical to the
  pre-Wave-1 regression corpus; ATE outputs are never scored as ATT and ATT
  outputs are never scored as ATE; the join emits the correct non-comparable
  reason for each missing requested/produced/truth branch.
- **Review gate:** Batch 1 output-contract review.
- **Disposition:** complete_after_review

### CS-1203 - Implement scorer-only structural truth and Wave 1 ATE producer

- **Batch:** 1
- **Source:** Constitution Article I/III; v0.2.0 spec
- **Motivation:** ATE uses `meta$structural_te` as truth, but ordinary estimators
  must not see truth columns through the typed runner path.
- **Files:** `R/cs-runner.R`, `R/cs-truth.R`, `R/est-oracle-att.R`,
  `R/cs-estimator-registry.R`, `tests/testthat/`
- **Constitutional check:** Article I ATE truth and Article III airlock.
- **Test obligation:** `oracle_att` emits typed ATT and ATE outputs; synthetic ATE
  score equals `mean(meta$structural_te)` over the full generated run sample; a
  spy estimator proves ordinary typed-path estimators never receive `y0`, `y1`,
  `p`, or `structural_te`; real-DGP ATE/CATE requests without external truth emit
  `truth_unavailable`, never regenerated truth.
- **Review gate:** Batch 1 truth-channel and ATE review.
- **Disposition:** complete_after_review

## Batch 2 - Schema 4 and Output Surfaces

### CS-1204 - Implement fit/score identity and schema-4 persistence

- **Batch:** 2
- **Source:** RFC-1 fit/score identity; Constitution Articles V and VI
- **Motivation:** One fit may produce multiple score records. The old
  DGP-estimator-seed result grain is not enough for typed scoring.
- **Files:** `R/cs-fingerprint.R`, `R/cs-pins.R`, `R/cs-staging.R`,
  `R/cs-runner.R`, campaign/resume/consolidation helpers, `tests/testthat/`
- **Constitutional check:** Articles V and VI atomic grains; Article II truth
  identity where relevant.
- **Test obligation:** Schema-4 score records include fit identity, target,
  metric, truth version, and populated `scoring_population_id`; nullable Wave 2
  fields (`seed_eval`, `n_eval`, `eval_derivation`, `unit_id_digest`,
  `prediction_digest`, `transductive`) exist; one fit can produce multiple score
  records without overwrite; schema 1-3 resume attempts fail closed with a
  classed migration/fresh-run error.
- **Review gate:** Batch 2 schema/persistence review.
- **Disposition:** implementation_complete_awaiting_review

### CS-1205 - Implement typed collection, tidy, science payload, and audit surfaces

- **Batch:** 2
- **Source:** v0.2.0 spec collection-surface design; RFC-1 row/tidy/science gap
- **Motivation:** ATT/QST-shaped collection outputs cannot be independent scoring
  authorities under typed scoring.
- **Files:** `R/cs-collect.R`, `R/cs-tidy-run.R`, `R/cs-result-to-row.R`,
  `R/cs-accessors.R`, `R/cs-summary*.R`, audit/science payload helpers,
  `tests/testthat/`
- **Constitutional check:** Article I no-cross-scoring and Articles V/VI score
  identity.
- **Test obligation:** Canonical typed score surface is long-form and includes
  scored rows, non-comparable rows with machine-readable reasons, and
  `estimator_error`/`runner_error` statuses; QST renders as multi-row point
  coordinates; ATT/QST helpers are compatibility projections or explicitly
  marked legacy; science payload and audit outputs preserve typed target ids,
  score statuses, and non-comparable reasons.
- **Review gate:** Batch 2 collection/audit review.
- **Disposition:** implementation_complete_awaiting_review

## Batch 3 - Staging, Gatekeeper, and Integration

### CS-1206 - Implement deterministic CATE staging

- **Batch:** 3
- **Source:** RFC-1 Rev 2 `target_not_implemented`; Constitution Article I
- **Motivation:** CATE is ratified but intentionally not implemented in Wave 1.
  The interim behavior must be deterministic.
- **Files:** runner/planner target handling, score-row helpers, `tests/testthat/`
- **Constitutional check:** Article I staged implementation and no-cross-scoring.
- **Test obligation:** CATE-only tasks hard-reject before estimator execution with
  a classed `target_not_implemented` error; mixed-target tasks emit a
  `target_not_implemented` CATE score row and continue scoring implemented
  targets; no held-out CATE sample is generated in Wave 1.
- **Review gate:** Batch 3 CATE-staging review.
- **Disposition:** open

### CS-1207 - Preserve gatekeeper policy and add ATE structural slot only

- **Batch:** 3
- **Source:** Constitution Article IV; horizon gatekeeper recalibration deferral
- **Motivation:** Wave 1 may add ATE gate structure, but must not decide deferred
  calibration, difficulty tiers, or registry consequences.
- **Files:** `R/cs-gatekeeper.R`, estimator registry/gate metadata as needed,
  `tests/testthat/`
- **Constitutional check:** Article IV per-estimand components; deferred
  gatekeeper policy remains deferred.
- **Test obligation:** ATT/QST gatekeeper verdicts and registry consequences are
  unchanged; ATE slot exists only as structure; ATE/CATE do not receive new
  `Non-Robust` policy or calibrated thresholds in Wave 1.
- **Review gate:** Batch 3 gatekeeper-scope review.
- **Disposition:** open

### CS-1208 - Run Wave 1 acceptance regression suite

- **Batch:** 3
- **Source:** v0.2.0 spec acceptance criteria
- **Motivation:** The breaking contract must be proven through public/default
  paths and regression comparisons before release-gate work.
- **Files:** `tests/testthat/`, test fixtures/regression corpus as needed
- **Constitutional check:** Articles I, III, V, and VI.
- **Test obligation:** Focused tests cover output normalization, no-cross-scoring,
  non-comparable reasons, ATE truth, real-DGP truth unavailability, both CATE
  staging branches, artifact identity uniqueness, typed collection/audit output,
  scorer-only airlock, and legacy ATT/QST numerical compatibility.
- **Review gate:** Batch 3 acceptance-regression review.
- **Disposition:** open

## Batch 4 - Release Gate

### CS-1209 - Run v0.2.0 Wave 1 release gate and closeout

- **Batch:** 4
- **Source:** `inst/design/release_gate.md`, active v0.2.0 spec acceptance
  criteria
- **Motivation:** The packet must close with reproducible evidence and no
  unresolved constitutional or review findings.
- **Files:** `inst/design/README.md`,
  `inst/design/causalstress_v0_2_0_spec_packet/release_closeout.md`,
  release surfaces as needed
- **Constitutional check:** No known constitutional violation remains open except
  by constitutional amendment or explicit deferral outside the packet.
- **Test obligation:** Run or explicitly defer-with-rationale every gate in
  `inst/design/release_gate.md`, including R CMD check, full tests, validation,
  acceptance criteria, review routing, planning-state check, and constitutional
  violation check.
- **Review gate:** Final release-gate review before tagging or merging.
- **Disposition:** open
