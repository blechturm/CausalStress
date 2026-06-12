# CausalStress v0.1.10 Spec

**Status:** ACTIVE DRAFT
**Date opened:** 2026-06-11
**Authority:** Active packet under `inst/design/README.md`

## Objective

Convert the v0.1.9 audit Rev 2 into a governed repair cycle, bootstrap the
spec-packet process, and close the highest-risk correctness and governance gaps
before further feature work.

## Scope

- Bootstrap governance artifacts: authority index, contract index, release gate,
  RFC cycle, templates, active packet.
- Route v0.1.9 audit Rev 2 findings.
- Fix or explicitly defer the 3 critical and 19 major findings from the audit.
- Add adversarial contract tests for the public/default paths that let the
  defects survive.

## Non-Scope

- New DGPs.
- New estimators.
- Sensitivity analysis.
- Python spoke work.
- Broad documentation polish outside the governance migration and audit fixes.

## Required Product Decision

C4 disposition must be recorded before Batch 1 implementation begins:

1. extract `fit$estimates$ATT` under the existing `tmle_att` id;
2. relabel the estimator to `tmle_ate` and exclude it from ATT scoring; or
3. drop the estimator.

This is a maintainer product decision, not an RFC and not coupled to M1/M9.

## Design

The release is organized into seven batches (Batch 0-Batch 6):

1. Batch 0: governance bootstrap.
2. Batch 1: mechanical high-yield fixes and contract tests.
3. Batch 2: schema-3 fingerprint/RNG/oracle-truth-cache design change,
   preceded by an RFC.
4. Batch 3: constitutionally gated M1/M9 decisions and implementation.
5. Batch 4: governance-conformance repairs.
6. Batch 5: cleanup, minor findings, design-doc corrections, and archive work.
7. Batch 6: release gate and closeout.

## Constitutional Compliance

- Article II: RNG isolation and bitwise scope are touched by C3, M3, M9, and
  the schema/RNG RFC.
- Article III: airlock/oracle access and estimator target semantics are touched
  by M1 and C4.
- Article IV: gatekeeper behavior is touched by M2.
- Article V: experimental parallel mode, thread caps, and serial-by-default
  execution are touched by M4 and M5.
- Article VI: atomic persistence, worker pin isolation, consolidation, and
  truth-cache writes are touched by M4, M8, and M18.
- Article VII: provenance, fingerprints, version/status warnings, and audit
  routing are touched by C2, M7, M8, M10, M11, M12, and M18.

Any ticket that would violate the Constitution is invalid until the Constitution
is amended through its own process.

## Acceptance Criteria

- `inst/design/README.md`, `contracts.md`, `rfc_cycle.md`, `release_gate.md`,
  `horizon.md`, `roadmap.md`, and `templates/` exist and are self-contained.
- The v0.1.9 audit Rev 2 findings are routed in `v0_1_10_tickets.md` or this
  spec with ticket/defer/reject rationale.
- C4 disposition is recorded before Batch 1 implementation starts.
- Contract tests cover these public/default paths directly in the packet:
  default-config resume skips instead of erroring; batch error semantics produce
  result rows for estimator/task execution failures and `batch$errors` rows for
  failures escaping `cs_run_single()`, with task-count reconciliation; CI-less
  ATT/QST gatekeeper paths are Unverified, never PASS/NA; missing estimator
  packages produce `success = FALSE` rows; `cs_validate_dgp()` rejects synthetic
  DGPs missing `y0`/`y1`; package load preserves both absent and existing RNG
  state.
- The release gate in `../release_gate.md` passes before closeout.
- The release-gate ticket reads `../release_gate.md`, records all gate evidence
  in `release_closeout.md`, and stops for review before tagging or merging.

## Audit Findings Consumed

Source: `../audit/v0_1_9_deep_code_review_audit.md` Rev 2.

| Finding | Disposition | Ticket |
| --- | --- | --- |
| C1 | Ticketed | CS-1102 |
| C2 | Ticketed | CS-1105, CS-1106 |
| C3 | Ticketed | CS-1105, CS-1107 |
| C4 | Ticketed | CS-1101 |
| C5 | Ticketed | CS-1103 |
| M1 | Ticketed | CS-1108 |
| M2 | Ticketed | CS-1110 |
| M3 | Ticketed | CS-1105, CS-1106 |
| M4 | Ticketed | CS-1111 |
| M5 | Ticketed | CS-1111 |
| M6 | Ticketed | CS-1104 |
| M7 | Ticketed | CS-1105, CS-1106 |
| M8 | Ticketed | CS-1105, CS-1117 |
| M9 | Ticketed | CS-1109 |
| M10 | Ticketed | CS-1112 |
| M11 | Ticketed | CS-1112 |
| M12 | Ticketed | CS-1105, CS-1106 |
| M13 | Ticketed | CS-1113 |
| M14 | Rejected as stated by audit Rev 2; residual tau-NA reporting routed as minor cleanup | CS-1116 |
| M15 | Ticketed | CS-1113 |
| M16 | Ticketed | CS-1114 |
| M17 | Ticketed as downgraded minor cleanup | CS-1116 |
| M18 | Ticketed | CS-1115 |
| M19 | Ticketed | CS-1104 |
| D1 | Ticketed as design cleanup | CS-1116 |
| D2 | Ticketed as design cleanup | CS-1116 |
| D3 | Ticketed as design cleanup | CS-1116 |
| D4 | Split: non-registry design drift ticketed; registry-spec stale entries deferred to a registry-spec version bump | CS-1116, closeout deferral |
| D5 | Ticketed as governance cleanup | CS-1100, CS-1116 |
| Minor findings | Batched into cleanup or deferred in closeout | CS-1116 |
| Release gate | Ticketed as final packet gate | CS-1118 |

## Release Gate

The release closes only after the checklist in `../release_gate.md` is satisfied
and evidenced in `release_closeout.md`.
