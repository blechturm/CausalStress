# CausalStress v0.1.10 Batch Plan

**Status:** Batch 2 complete after review.

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

- **Status:** Complete after review.
- **Purpose:** Fix top-priority correctness and packaging defects with focused
  tests.
- **Tickets:** CS-1101, CS-1102, CS-1103, CS-1104
- **Scope completed:** TMLE now extracts `fit$estimates$ATT` under the existing
  `tmle_att` id; batch workers preserve task errors and reconcile planned tasks
  against result/error counts; runtime imports are declared in `DESCRIPTION`;
  DGP validation iterates every registered `(dgp_id, version)` row and rejects
  missing potential outcomes.
- **Checks run:** June 12, 2026, with
  `C:/PROGRA~1/R/R-45~1.2/bin/x64/Rscript.exe` and `.libPaths()` =
  `C:/Users/maxth/Documents/R/win-library/4.5`;
  `C:/Users/maxth/AppData/Local/R/win-library/4.5`;
  `C:/Program Files/R/R-4.5.2/library`. Focused testthat filter
  `est-tmle|v019-worker|v019-e2e|validate-dgp|validate-all` passed; local
  runtime probe confirmed `tmle::tmle()` exposes `ATT$psi` and `ATT$CI`;
  `devtools::check()` with `--no-manual --ignore-vignettes` and build-stage
  vignette rebuilding disabled completed with 0 errors and dependency checks
  OK.
- **Review evidence:** Claude review found one blocking dependency issue
  (`yaml` still Suggests-only) and three non-blocking routing notes; the
  dependency issue and routing notes were fixed, re-reviewed, and cleared for
  commit. Batch 1 committed as `068c0ff`.
- **Review checkpoint:** Codex code review confirms C4 uses
  `fit$estimates$ATT` under the existing `tmle_att` id, C1 semantics,
  dependency declarations, and validator negative tests.

## Batch 2 - Schema 3, RNG Isolation, and Oracle Truth Cache Identity

- **Status:** Complete after review.
- **Purpose:** Coordinate resume/fingerprint, RNG, and oracle truth cache
  identity changes under one design.
- **Tickets:** CS-1105, CS-1106, CS-1107, CS-1117
- **Scope completed:** CS-1105 draft synthesis added at
  `inst/design/rfc/20260612_schema3_rng_oracle_synthesis.md`, covering
  schema-3 fingerprint payload, versioned pin identity, canonical planner
  fingerprints, RNG isolation, and oracle truth cache identity/atomicity.
- **Review evidence:** CS-1105 synthesis was reviewed with no blocking findings;
  non-blocking amendments were routed in the RFC; maintainer accepted CS-1105 as
  amended on 2026-06-12.
- **Implementation completed:** CS-1106 schema-3 fingerprints, versioned pin
  identity, exact-version resume, schema-2/v0.1.7 compatibility handling, and
  canonical planner fingerprints; CS-1107 RNG preservation for load validation,
  validation helpers, and campaign planning; CS-1117 oracle algorithm
  fingerprinted cache identity and atomic temp-write/rename cache writes.
- **Checks run:** design-document consistency review against audit C2, C3, M3,
  M7, M8, M12; `contracts.md`; and the current fingerprint, runner, campaign,
  planner, load-validation, RNG, and oracle truth-cache code paths.
- **Implementation checks run:** June 12, 2026, with
  `C:/Program Files/R/R-4.5.2/bin/x64/Rscript.exe`; focused testthat filter
  `fingerprint|resume|v019-planner|rng-isolation|oracle-truth|pins-integration|pin-management|force-overwrite|usability-permutations|v018-parallel-gating|parallel-protocol|v019-worker|v019-e2e`
  passed. Remaining warnings are pre-existing tidyselect `.data` deprecations
  in the v0.1.9 e2e path. `devtools::check(document = FALSE,
  build_args = '--no-build-vignettes', args = c('--no-manual',
  '--ignore-vignettes'), error_on = 'never')` completed with 0 errors, 4
  warnings, and 4 notes; the remaining warnings/notes are pre-existing
  documentation/package-structure items, and dependency/codoc checks are OK.
- **Review checkpoint:** accepted RFC synthesis before implementation; focused
  Codex code review after implementation.
- **Review evidence:** Claude review reported no blocking findings, verified
  all CS-1105 amendments and Batch 2 code paths, and cleared Batch 2 for
  commit after confirming no `withr::` calls remain in `R/`. Non-blocking
  migration note for legacy v0.1.9 boards was routed to release closeout.

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
