# CausalStress v0.1.10 Batch Plan

**Status:** ACTIVE DRAFT

## Batch 0 - Governance Bootstrap

- **Purpose:** Install the spec-packet governance structure.
- **Tickets:** CS-1100
- **Review checkpoint:** authority index and packet scaffold reviewed before
  implementation batches begin.

## Batch 1 - Mechanical High-Yield Fixes

- **Purpose:** Fix top-priority correctness and packaging defects with focused
  tests.
- **Tickets:** CS-1101, CS-1102, CS-1103, CS-1104
- **Review checkpoint:** confirm C4 disposition, C1 semantics, dependency
  declarations, and validator negative tests.

## Batch 2 - Schema 3, RNG Isolation, and Oracle Truth Cache Identity

- **Purpose:** Coordinate resume/fingerprint, RNG, and oracle truth cache
  identity changes under one design.
- **Tickets:** CS-1105, CS-1106, CS-1107, CS-1117
- **Review checkpoint:** accepted RFC synthesis before implementation; focused
  review after implementation.

## Batch 3 - Constitutionally Gated Fixes

- **Purpose:** Resolve issues requiring constitutional decisions.
- **Tickets:** CS-1108, CS-1109
- **Review checkpoint:** RFC/amendment path complete before code change.

## Batch 4 - Governance Conformance

- **Purpose:** Close remaining major runner, registry, gatekeeper, and batching
  conformance gaps.
- **Tickets:** CS-1110, CS-1111, CS-1112, CS-1113, CS-1114, CS-1115
- **Review checkpoint:** release-gate validation suite can pass after this batch.

## Batch 5 - Cleanup

- **Purpose:** Route minors and design-document corrections.
- **Tickets:** CS-1116
- **Review checkpoint:** closeout confirms every audit finding is ticketed,
  deferred, or rejected.

## Batch 6 - v0.1.10 Release Gate

- **Purpose:** Verify release readiness and record closeout evidence.
- **Tickets:** CS-1118
- **Scope:** read `inst/design/release_gate.md`; run or explicitly defer every
  required gate; verify the design README planning state; complete
  `release_closeout.md`; stop for final review before tagging or merging.
- **Review checkpoint:** all release-gate evidence is recorded, skipped gates
  have rationale, audit routing is complete, and no constitutional violation is
  known open except by constitutional amendment.
