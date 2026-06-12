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

- `df`: data frame with observed outcome `y`, treatment `w`, covariates, and
  internal truth columns before the runner airlock.
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
explicit oracle-access mechanism permits a narrower grant. The v0.1.10 packet
must resolve audit M1: either implement column-scoped config-based oracle access
or amend the Constitution to authorize the registry-flag mechanism.

Estimator outputs must be structured lists with:

- `att`: ATT point estimate and optional interval fields.
- `qst`: optional QST table matching the requested tau grid exactly.
- `meta`: estimator id, version/provenance, CI diagnostics, warnings, and errors.

An estimator id ending in `_att` must not report an ATE into ATT scoring unless
the active packet explicitly relabels or excludes that estimator.

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
- configuration and task fingerprints under the active schema.

Failures attributable to a single task's estimator execution should become a
`success = FALSE` result row with provenance. Batch-level failures that escape
`cs_run_single()` must become structured `batch$errors` rows, and batch
artifacts must reconcile task count as:

`length(results) + nrow(errors) == nrow(tasks)`.

## Fingerprints and Pins

Normative sources: active packet plus historical specs for legacy artifacts.

Known schemas:

- legacy v0.1.7: historical fingerprint format; migration/read support only.
- schema 2: current v0.1.9 format; missing DGP version and affected by config
  normalization defects identified in the audit.
- schema 3: planned v0.1.10 design change for DGP version, canonical config,
  CI intent/source, task fingerprint, and resume comparison rules.

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

The v0.1.10 packet must define which entry points set RNG state, which restore
state, and how validation-on-load preserves both `.Random.seed` absence and
byte-identical existing seed state.

## Validation Contract

Release validation must cover three surfaces:

1. Registry metadata and sidecar validation: semver/status invariants, rationale
   requirements, and per-version sidecar noise/effect/version/status checks.
2. Version-aware executable validation: every registered `(dgp_id, version)`
   generator is executed and checked, not first-match-per-id.
3. Public DGP certification: `cs_validate_dgp()` rejects structurally invalid
   synthetic DGPs, including missing potential outcomes.

## Thread Contract

Constitution Article V governs computational safety: v0.1.x is serial by
default, and experimental parallel paths must require explicit opt-in, emit
provenance, and cap worker-local threads without permanently mutating process
environment. Article VI governs atomic persistence, including staged writes,
worker pin-isolation, and serial consolidation.
