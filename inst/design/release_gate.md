# CausalStress Release Gate

**Status:** Active release checklist and release-gate ticket playbook

A release cannot close until every item below has evidence recorded in the
packet `release_closeout.md`.

## Release-Gate Ticket Requirements

Every final release-gate ticket must include:

- `inst/design/release_gate.md` in its source references.
- A task to read this file before running or updating release gates.
- A task to run or explicitly defer-with-rationale each required check below.
- A task to record exact command/evidence, skipped gates, failures, fixes, and
  final status in the packet `release_closeout.md`.
- A task to verify that `inst/design/README.md` reflects the release planning
  state before closeout.

The release gate is a separate final packet batch. It verifies release readiness;
it does not perform speculative design or broad implementation work. If a gate
uncovers a design issue, route that issue deliberately, fix it under the
appropriate ticket, and rerun the affected gates.

## Required Checks

1. `R CMD check` passes with no ERRORs or WARNINGs.
2. The validation suite passes:
   - registry metadata / sidecar validation.
   - version-aware executable validation of every registered `(dgp_id, version)`.
   - public `cs_validate_dgp()` certification tests, including negative cases.
   The current v0.1.9 validators are not sufficient for this gate until the
   v0.1.10 validation tickets have fixed M6, M10, and M19.
3. The full test suite passes.
4. Packet acceptance criteria are checked off with evidence.
5. Every finding in every open audit is routed:
   - ticketed,
   - deferred with rationale and destination, or
   - rejected with rationale.
6. `inst/design/README.md` reflects the final planning state.
7. The packet has a `release_closeout.md`.
8. No constitutional violation is known open. A constitutional violation can be
   deferred only by constitutional amendment, not by ticket.

## Gate Sequence

1. Read this file and the active packet before starting.
2. Confirm all implementation batches are complete or explicitly deferred.
3. Confirm the diff is release-gate scoped. If broad unrelated changes are
   needed, stop and route them before continuing.
4. Run the required checks and capture evidence.
5. Update `release_closeout.md` with commands, results, skipped gates, and
   dispositions.
6. Update `inst/design/README.md` planning state if the release is closing or a
   next packet is being opened.
7. Stop for review before tagging or merging.

## Evidence Standard

Evidence should name the command, date, result, and any relevant artifact path.
For design-only gates, evidence should name the reviewed file and accepted
decision.
