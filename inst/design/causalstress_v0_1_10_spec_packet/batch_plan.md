# CausalStress v0.1.10 Batch Plan

**Status:** Batch 0 complete after Claude review.

## Review Protocol

A batch is the unit of Codex code review. Work one batch at a time.
Batch 0 closed under Claude review before this protocol amendment; starting with
Batch 1, Codex code review is required after every implemented batch before the
next batch starts. Claude or maintainer review may supplement this gate, but it
does not replace the required Codex review.

For implementation or governance batches:

- finish the scoped batch;
- run targeted consistency checks;
- update `v0_1_10_tickets.md`, `tickets.yml`, and this batch plan together;
- stop and ask for Codex code review with an inline prompt;
- do not proceed to the next batch until the Codex review findings are routed.

If a batch starts requiring broad unrelated diffs or work outside the ticket,
stop and ask before continuing.

## Batch 0 - Governance Bootstrap

- **Status:** Complete after Claude review.
- **Purpose:** Install the spec-packet governance structure.
- **Tickets:** CS-1100
- **Scope completed:** authority README, contract index, release gate, RFC cycle,
  roadmap, horizon, archive index, audit records, canonical templates, v0.1.10
  spec packet, ticket files, batch plan, and release closeout scaffold.
- **Review checkpoint:** authority index and packet scaffold reviewed before
  implementation batches begin.
- **Review evidence:** Claude review reported no blocking findings and confirmed
  Batch 0 is ready to close. Non-blocking wording/process fixes were routed in
  the Batch 0 closeout commit.

## Batch 1 - Mechanical High-Yield Fixes

- **Purpose:** Fix top-priority correctness and packaging defects with focused
  tests.
- **Tickets:** CS-1101, CS-1102, CS-1103, CS-1104
- **Review checkpoint:** Codex code review confirms C4 uses
  `fit$estimates$ATT` under the existing `tmle_att` id, C1 semantics,
  dependency declarations, and validator negative tests.

## Batch 2 - Schema 3, RNG Isolation, and Oracle Truth Cache Identity

- **Purpose:** Coordinate resume/fingerprint, RNG, and oracle truth cache
  identity changes under one design.
- **Tickets:** CS-1105, CS-1106, CS-1107, CS-1117
- **Review checkpoint:** accepted RFC synthesis before implementation; focused
  Codex code review after implementation.

## Batch 3 - Constitutionally Gated Fixes

- **Purpose:** Resolve issues requiring constitutional decisions.
- **Tickets:** CS-1108, CS-1109
- **Review checkpoint:** RFC/amendment path complete before code change; Codex
  code review after implementation.

## Batch 4 - Governance Conformance

- **Purpose:** Close remaining major runner, registry, gatekeeper, and batching
  conformance gaps.
- **Tickets:** CS-1110, CS-1111, CS-1112, CS-1113, CS-1114, CS-1115
- **Review checkpoint:** Codex code review confirms the release-gate validation
  suite can pass after this batch.

## Batch 5 - Cleanup

- **Purpose:** Route minors and design-document corrections.
- **Tickets:** CS-1116
- **Review checkpoint:** Codex code review confirms every audit finding is
  ticketed, deferred, or rejected.

## Batch 6 - v0.1.10 Release Gate

- **Purpose:** Verify release readiness and record closeout evidence.
- **Tickets:** CS-1118
- **Scope:** read `inst/design/release_gate.md`; run or explicitly defer every
  required gate; verify the design README planning state; complete
  `release_closeout.md`; stop for final review before tagging or merging.
- **Review checkpoint:** final Codex code review confirms all release-gate
  evidence is recorded, skipped gates have rationale, audit routing is complete,
  and no constitutional violation is known open except by constitutional
  amendment.
