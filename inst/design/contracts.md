# CausalStress Operational Contracts

**Status:** Active contract index
**Authority:** Below `CAUSAL_STRESS_CONSTITUTION.md`; above active packets where
the Constitution is silent.

This document maps the code-facing contracts that agents must preserve or update
through an active spec packet. If this document conflicts with the Constitution,
the Constitution wins.

## DGP Contract

Normative source: Constitution Articles II, III, and VII.

Synthetic DGPs must return a list containing at least:

- `df`: data frame with observed outcome `y`, treatment `w`, internal truth
  columns before the runner airlock, and one or more synthetic covariates named
  exactly `X1`, ..., `Xk` using uppercase `X` with consecutive one-based integer
  suffixes.
- `true_att`: scalar truth for ATT where applicable.
- `true_qst`: truth table with canonical `tau` and `value` columns for truth
  tables.
- `meta`: metadata including DGP id/version/status where available.

Runner-facing DGP resolution must go through the registry. Version and status
selection, deprecation/invalidated warnings, and preservation of historical DGPs
are Constitution-governed.

## Estimator Contract

Normative source: Constitution Article III.

Estimators receive sanitized runner data by default. The runner must remove
`y0`, `y1`, `p`, and `structural_te` before estimator execution unless an
explicit oracle-access mechanism permits a narrower grant. Column-scoped oracle
access may grant `p` for true-propensity oracle estimators or `structural_te` for
internal structural benchmark estimators. `y0`/`y1` are never exposed through the
ordinary runner airlock in the v2.x line. Estimator descriptors declare
`oracle_columns` and `oracle_default_columns`; the registry `oracle` flag is
provenance/eligibility metadata, not a blanket raw-data grant.

The v2.0.0 estimator output contract is:

`function(df, tau, config) -> list(outputs, meta)`

where `outputs` is a named collection of typed estimand outputs keyed by
`estimand_target_id`, and `meta` carries estimator id, version/provenance,
capabilities, CI diagnostics, warnings, and errors.

Wave 1 target support:

- `att`: scalar point estimate and optional interval fields; scoreable where
  produced and truth exists.
- `ate`: scalar point estimate and optional interval fields; scoreable where
  produced and truth exists. Wave 1 pins ATE scoring to the full generated run
  sample.
- `qst`: curve matching the runner-provided tau grid; scoreable where produced
  and truth exists.
- `cate`: registered target only in Wave 1. CATE-only tasks hard-reject before
  estimator execution; mixed-target tasks emit a `target_not_implemented`
  non-comparable score row for CATE and continue scoring implemented targets.

Legacy `list(att, qst, meta)` outputs remain accepted during Wave 1 as a
compatibility shim. The runner normalizes `att` and `qst` into typed outputs
before scoring. Missing legacy fields mean "not produced", not estimator
failure.

Typed scoring is a three-way join:

`requested` intersect `estimator-produced` intersect `DGP-truth-available`

Outputs must be scored only against their matching target truth. ATE outputs
must not be scored as ATT, and ATT outputs must not be scored as ATE.

The non-comparable reason vocabulary includes at minimum:
`estimator_not_produced`, `truth_unavailable`, `metric_invalid_for_regime`,
`ci_unavailable`, `gate_unimplemented`, `not_requested`, and
`target_not_implemented`.

### Security Scope

The airlock is a contract guard against accidental truth leakage to honest
estimators. CausalStress does not claim adversarial sandboxing for malicious
estimator code. Documentation and tests must not imply that the package can
prevent a deliberately hostile estimator from inspecting process state or
otherwise bypassing ordinary R calling conventions.

## Runner Output Contract

Runner outputs must expose:

- success/failure semantics (`success`, `error`) for each task.
- DGP and estimator identifiers and versions.
- seed, sample size, tau identifiers where relevant.
- CI provenance including method, type, level, validity, fail code, and bootstrap
  success counts.
- granted oracle columns, when any.
- configuration and task fingerprints under the active schema.

Wave 1 typed scoring additionally exposes a canonical long score surface. It has
one row per scalar score or QST point coordinate and includes:

- run identity: `dgp_id`, `dgp_version`, `estimator_id`, `estimator_version`,
  `n`, and `seed`.
- artifact identity: `fit_fingerprint`, curve/scalar-level
  `score_fingerprint`, physical-row `score_row_fingerprint`, and
  `schema_version`.
- target identity: `estimand_target_id`, target descriptor fields, and
  `metric_id`.
- point coordinate: `tau`/`tau_index` for QST rows and `NA` for scalar ATT/ATE
  rows.
- values: `estimate`, `truth`, `error`, optional `ci_lo`, and optional `ci_hi`.
- status: `score_status` in `scored`, `non_comparable`, `estimator_error`, or
  `runner_error`.
- non-comparability: `non_comparable_reason`, populated only when
  `score_status = "non_comparable"`.

Non-comparable requested targets are represented in this surface as rows with
`NA` value fields and a machine-readable `non_comparable_reason`. ATT/QST
user-facing helpers may remain as compatibility projections over the typed
surface, but they are not independent scoring authorities.

ATE truth is computed on the scorer side from runner-owned DGP truth state.
Ordinary estimators must not receive `structural_te` through the typed path.

Failures attributable to a single task's estimator execution should become a
`success = FALSE` result row with provenance. Batch-level failures that escape
`cs_run_single()` must become structured `batch$errors` rows, and batch
artifacts must reconcile task count as:

`length(results) + nrow(errors) == nrow(tasks)`.

When bootstrap CIs are requested and the estimator reports fewer than 90% of
requested bootstrap replicates as successful for any reported inferential
dimension, the runner must emit a classed CI warning, set CI fields for the
affected dimension to `NA`, and mark the task result `success = FALSE`.

## Fingerprints and Pins

Normative sources: active packet plus historical specs for legacy artifacts.

Known schemas:

- legacy v0.1.7: historical fingerprint format; migration/read support only.
- schema 2: shipped v0.1.9 format; missing DGP version and affected by config
  normalization defects identified in the audit.
- schema 3: shipped v0.1.10 format for DGP version, canonical config, CI
  intent/source, task fingerprint, and resume comparison rules.
- schema 4: active v0.2.0 Wave 1 design change for fit/score identity and typed
  score records.

Schema 4 separates:

- fit artifact identity: data/model/config identity for one model fit.
- score record identity: fit identity plus scored estimand target, metric, truth
  version, and `scoring_population_id`.
- score row identity: score-record identity plus a canonical row coordinate.
  QST point rows use `tau_id`, scored ATT/ATE scalar rows use `scalar`, and
  target-level non-comparable/error rows without a point coordinate use
  `record_status`. `tau_index` controls ordering and is not identity-bearing.

All rows for one QST curve share a `score_fingerprint` and have distinct
`score_row_fingerprint` values. `meta$score_fingerprints` is the stable unique
set of score-record identities in first-occurrence order, while
`meta$score_row_fingerprints` contains one identity per score-surface row.
Pins, typed collectors, row projections, and science/audit accessors preserve
both levels.

`scoring_population_id` is populated in Wave 1 score records. ATT/QST/ATE use
their declared Wave 1 population ids. Schema 4 reserves nullable Wave 2 CATE
fields: `seed_eval`, `n_eval`, `eval_derivation`, `unit_id_digest`,
`prediction_digest`, and `transductive`.

Pin naming and metadata must be documented in the active packet before changes
ship. Historical specs are authoritative for interpreting artifacts from their
era, but not for new implementation work.

## Batch Artifact Contract

Normative sources: `archive/DESIGN_V0.1.9_BATCHING.md` as historical artifact
reference and the active v0.1.10 packet for new work.

Batch artifacts must include:

- schema version.
- batch id and task count.
- result list.
- error table with `task_fingerprint` and `error_class`.
- plan/task fingerprints sufficient to validate consolidation.
- provenance for worker node, backend, git hash where available, and timestamp.

Workers must write staged artifacts only. Consolidation into pins is a controller
responsibility.

## RNG Contract

Normative source: Constitution Article II.

Benchmark execution uses the Constitution's mandated RNG kind. Package load and
validation paths must not leak RNG side effects into the user's session.

Same-substrate bitwise identity is required for DGP data and truth outputs under
the declared R/platform/numeric-library/thread substrate. Cross-substrate
reproducibility is tolerance-level unless a version-specific regression corpus
proves bitwise identity. The release gate records the validation substrate.

## Validation Contract

Release validation must cover three surfaces:

1. Registry metadata and sidecar validation: semver/status invariants, rationale
   requirements, and per-version sidecar noise/effect/version/status checks.
2. Version-aware executable validation: every registered `(dgp_id, version)`
   generator is executed and checked, not first-match-per-id.
3. Public DGP certification: `cs_validate_dgp()` rejects structurally invalid
   synthetic DGPs, including missing potential outcomes and non-canonical
   covariate names.

All three surfaces use the same internal covariate-name predicate. For the
synthetic generation frame, the authoritative operational non-covariate fields
are `y`, `w`, `p`, `y0`, `y1`, and `structural_te`; every remaining column must
form exactly `X1...Xk`, with `k >= 1`. Contract failures abort with class
`causalstress_dgp_error`, and validation restores the caller's RNG state even
when validation fails.

## Thread Contract

Constitution Article V governs computational safety. Release lines are serial by
default unless the active release specification authorizes parallel execution
under the Article VI atomic-persistence protocol. Parallel paths must emit
provenance and cap worker-local threads without permanently mutating process
environment. Article VI governs atomic persistence, including staged writes,
worker pin-isolation, and serial consolidation.
