# CausalStress v0.1.10 Release Closeout

**Status:** OPEN
**Date closed:** TBD

This file is opened with the packet so release-gate evidence has a canonical
destination. It must be completed before v0.1.10 closes.

## Shipped

| Ticket | Evidence |
| --- | --- |
| CS-1100 | Governance scaffold committed in `a05679b`; Batch 0 Claude review reported no blocking findings and declared Batch 0 ready to close after review. |
| CS-1101--CS-1104 | Batch 1 implementation committed in `068c0ff`; Claude review found one blocking dependency issue and non-blocking routing notes, all resolved and re-reviewed before commit. |
| CS-1105 | RFC synthesis `inst/design/rfc/20260612_schema3_rng_oracle_synthesis.md` reviewed with no blocking findings; maintainer accepted CS-1105 as amended on 2026-06-12. |
| CS-1106--CS-1107, CS-1117 | Batch 2 implementation reviewed with no blocking findings; review verified schema-3 fingerprints, versioned pins/resume guards, RNG isolation, oracle truth-cache identity/atomicity, and planner/batch version execution. |
| CS-1108--CS-1109 | Batch 3 RFC accepted as amended on 2026-06-13; Claude review reported no blocking findings and verified column-scoped oracle access, Article II/VII substrate-scoped reproducibility, and release-gate substrate evidence. |

## Migration Notes

- Legacy v0.1.9 result boards with schema-2 pins that lack `dgp_version`
  metadata cannot be proven to match the resolved DGP version under the schema-3
  resume contract. Recompute those runs with `force = TRUE` or use a fresh
  board rather than resuming them.
- v0.1.10 does not claim cross-platform/R-version/BLAS/libm bitwise identity
  unless a version-specific regression corpus proves it. Same-substrate bitwise
  identity remains mandatory, and release-gate evidence must record the
  validation substrate.

## Deferred

| Ticket/Finding | Rationale | Destination |
| --- | --- | --- |
| D4 registry-spec stale entries | Active scientific registry specs stay at the design root and must be corrected by a registry-spec version bump, not silent edits to historical scientific claims. | `roadmap.md` Later Horizons: registry-spec version bump |
| CS-1109 oracle-truth-path same-substrate regression | Batch 3 same-substrate regression covers the BLAS-sensitive data path with `include_truth = FALSE`; direct `include_truth = TRUE` oracle-truth evidence is deferred to avoid adding a costly unit test. | Release-gate evidence or CS-1116 cleanup/integration test decision |

## Rejected

| Ticket/Finding | Rationale |
| --- | --- |
| M14 as originally stated | Rejected by audit Rev 2; residual tau-NA reporting routed as cleanup | 

## Release Gate Evidence

| Gate | Evidence | Result |
| --- | --- | --- |
| Release gate playbook read | TBD | TBD |
| R CMD check | TBD | TBD |
| Validation suite | TBD | TBD |
| Full test suite | TBD | TBD |
| Acceptance criteria | TBD | TBD |
| Audit routing | TBD | TBD |
| README planning state | TBD | TBD |
| Known constitutional violations | TBD | TBD |
| Final release-gate review | TBD | TBD |
