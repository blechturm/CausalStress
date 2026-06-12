# CausalStress v0.1.10 Tickets

**Status:** ACTIVE DRAFT

## Batch 0 - Governance Bootstrap

### CS-1100 - Install authority index and governance scaffold

- **Source:** governance proposal
- **Files:** `inst/design/README.md`, `contracts.md`, `rfc_cycle.md`,
  `release_gate.md`, `roadmap.md`, `horizon.md`, `templates/`
- **Constitutional check:** Constitution remains apex authority.
- **Test obligation:** Design review only.
- **Review gate:** Batch 0 governance scaffold review.
- **Disposition:** complete after Claude review

## Batch 1 - Mechanical High-Yield Fixes

### CS-1101 - Resolve C4 TMLE estimand disposition

- **Source:** audit C4
- **Files:** `R/est-tmle.R`, estimator registry, tests
- **Constitutional check:** Estimator target must match output/scoring contract.
- **Test obligation:** Heterogeneous-effect repro distinguishes ATE from ATT.
- **Maintainer disposition:** keep the existing `tmle_att` id and extract
  `fit$estimates$ATT`.
- **Review gate:** Batch 1 Codex review verifies the implementation follows the
  recorded C4 disposition.
- **Disposition:** implementation complete; awaiting review.

### CS-1102 - Preserve batch task errors and reconcile task counts

- **Source:** audit C1
- **Files:** `R/cs-run-batch.R`, `R/cs-consolidate.R`, tests
- **Constitutional check:** Provenance and error reporting must be complete.
- **Test obligation:** Estimator errors become result rows; escaping task errors
  become `batch$errors`; task counts reconcile.
- **Review gate:** Batch 1 runner/batch contract review.
- **Disposition:** implementation complete; awaiting review.

### CS-1103 - Fix package dependencies

- **Source:** audit C5
- **Files:** `DESCRIPTION`, `NAMESPACE` if needed
- **Constitutional check:** none
- **Test obligation:** `R CMD check` reaches package checks without missing
  namespace failures.
- **Review gate:** Batch 1 packaging review.
- **Disposition:** implementation complete; awaiting review.

### CS-1104 - Make DGP validation version-aware and non-vacuous

- **Source:** audit M6, M19
- **Files:** validation helpers and tests
- **Constitutional check:** DGP certification must reject unconstitutional DGPs.
- **Test obligation:** every registered `(dgp_id, version)` is validated; missing
  potential outcomes fail `cs_validate_dgp()`.
- **Review gate:** Batch 1 validation review.
- **Disposition:** implementation complete; awaiting review.

## Batch 2 - Schema 3, RNG Isolation, and Oracle Truth Cache Identity

### CS-1105 - RFC schema-3 fingerprint and RNG isolation design

- **Source:** audit C2, C3, M3, M7, M8, M12
- **Files:** `inst/design/rfc/`
- **Constitutional check:** Articles II and VII.
- **Test obligation:** RFC synthesis accepted before implementation.
- **Review gate:** RFC final review and maintainer acceptance.
- **Disposition:** open

### CS-1106 - Implement schema-3 resume/fingerprint model

- **Source:** audit C2, M3, M7, M12
- **Files:** fingerprint, pins, runner, campaign, tests
- **Constitutional check:** provenance and legacy-read compatibility.
- **Test obligation:** default-config resume skips; DGP version and canonical
  config are fingerprinted.
- **Review gate:** Batch 2 fingerprint/resume review.
- **Disposition:** open; blocked by CS-1105.

### CS-1107 - Isolate RNG side effects

- **Source:** audit C3
- **Files:** RNG helpers, load validation, campaign planning, tests
- **Constitutional check:** Article II.
- **Test obligation:** package load preserves absent and existing `.Random.seed`
  states; campaign plans are stable under the mandated RNG policy.
- **Review gate:** Batch 2 RNG isolation review.
- **Disposition:** open; blocked by CS-1105.

### CS-1117 - Version oracle truth cache identity and atomic writes

- **Source:** audit M8
- **Files:** `R/cs-oracle-truth.R`, fingerprint/cache helpers, tests
- **Constitutional check:** Articles II, VI, and VII; truth values must be
  reproducible, cache identity must include oracle algorithm identity, and cache
  writes must be atomic.
- **Test obligation:** oracle truth cache key includes DGP version and oracle
  algorithm parameters/fingerprint; stale cache entries invalidate; cache writes
  use checked temp+rename semantics.
- **Review gate:** Batch 2 truth-cache identity review.
- **Disposition:** open; blocked by CS-1105.

## Batch 3 - Constitutionally Gated Fixes

### CS-1108 - Decide and implement oracle access mechanism

- **Source:** audit M1
- **Files:** Constitution amendment or runner/airlock implementation, tests
- **Constitutional check:** Article III.
- **Test obligation:** propensity-only oracle cannot access counterfactual truth.
- **Review gate:** Batch 3 constitutional decision review.
- **Disposition:** open; requires RFC/amendment decision.

### CS-1109 - Decide and implement bitwise scope for BLAS-sensitive DGPs

- **Source:** audit M9
- **Files:** Constitution amendment or DGP implementation, tests/docs
- **Constitutional check:** Article II.
- **Test obligation:** either pure-R deterministic path or documented amended
  scope with validation evidence.
- **Review gate:** Batch 3 constitutional decision review.
- **Disposition:** open; requires RFC/amendment decision.

## Batch 4 - Governance Conformance

### CS-1110 - Fix gatekeeper unverified semantics

- **Source:** audit M2
- **Files:** gatekeeper and tests
- **Constitutional check:** Article IV.
- **Test obligation:** CI-less ATT/QST paths are Unverified, never PASS/NA.
- **Review gate:** Batch 4 gatekeeper review.
- **Disposition:** open

### CS-1111 - Restore parallel governance on campaign paths

- **Source:** audit M4, M5
- **Files:** campaign runner, batch runner, scale helpers, tests
- **Constitutional check:** Articles V, VI, and VII.
- **Test obligation:** parallel requires opt-in, emits provenance, restores env,
  and does not silence DGP governance warnings.
- **Review gate:** Batch 4 parallel governance review.
- **Disposition:** open

### CS-1112 - Enhance registry and sidecar validation

- **Source:** audit M10, M11
- **Files:** registry validation, sidecars, tests
- **Constitutional check:** Article VII.
- **Test obligation:** strict validation checks sidecar version/status and
  deprecation warning includes date when present.
- **Review gate:** Batch 4 registry validation review.
- **Disposition:** open

### CS-1113 - Fix CI/bootstrap and optional-package failure semantics

- **Source:** audit M13, M15
- **Files:** runner, bootstrap helpers, tests
- **Constitutional check:** runner output contract.
- **Test obligation:** low bootstrap success is explicit failure/warning as
  specified; missing packages produce `success = FALSE` rows.
- **Review gate:** Batch 4 CI/failure semantics review.
- **Disposition:** open

### CS-1114 - Fix campaign forwarding and batch tau handling

- **Source:** audit M16
- **Files:** campaign, one-seed, batch runner, planner, tests
- **Constitutional check:** estimator requested tau contract.
- **Test obligation:** documented `...` and strategy tau reach `cs_run_single()`.
- **Review gate:** Batch 4 campaign forwarding review.
- **Disposition:** open

### CS-1115 - Close batching schema and consolidation gaps

- **Source:** audit M18
- **Files:** batch runner, consolidator, tidy/accessors, tests
- **Constitutional check:** Article VII provenance.
- **Test obligation:** error schema includes task fingerprint/class; consolidator
  validates fingerprints/schema; tidy rows propagate required fields.
- **Review gate:** Batch 4 batching schema review.
- **Disposition:** open

## Batch 5 - Cleanup

### CS-1116 - Route minors and design-document corrections

- **Source:** audit minors including downgraded M17, D1-D5, ex-M14 residual
  tau-NA item; D4 registry-spec stale entries deferred to a registry-spec
  version bump
- **Files:** design docs, cleanup tests as needed
- **Constitutional check:** depends on item
- **Test obligation:** no known stale non-registry design doc remains active;
  residual code minors are ticketed/deferred/rejected in closeout; consider a
  `.gitattributes` line-ending policy for Markdown/YAML governance files to
  avoid LF/CRLF churn.
- **Review gate:** Batch 5 cleanup and audit-routing review.
- **Disposition:** open

## Batch 6 - Release Gate

### CS-1118 - Run v0.1.10 release gate and closeout

- **Source:** `inst/design/release_gate.md`, active packet acceptance criteria
- **Files:** `inst/design/README.md`,
  `inst/design/causalstress_v0_1_10_spec_packet/release_closeout.md`, release
  surfaces as needed
- **Constitutional check:** no known constitutional violation remains open
  except by constitutional amendment.
- **Test obligation:** run or explicitly defer-with-rationale every gate in
  `inst/design/release_gate.md`, including R CMD check, validation suite, full
  tests, acceptance criteria, audit routing, README planning-state check, and
  constitutional-violation check.
- **Review gate:** final release-gate review before tagging or merging.
- **Disposition:** open
