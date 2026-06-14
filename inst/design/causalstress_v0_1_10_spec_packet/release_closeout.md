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
| CS-1110--CS-1115 | Batch 4 implementation committed in `5c96068`; Claude review reported no blocking findings. Review non-blockers were resolved or routed before the Batch 4 commit: QST gatekeeper denominator fixed, dead parallel-backend assignment removed, low-bootstrap contract wording routed to CS-1116, and full-suite future-plan cleanup kept in CS-1116. |

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
| D1--D3 stale v0.3.0 design examples | `CAUSALSTRESS_DESIGN_v0.3.0.md` is archived verbatim and explicitly non-current under `inst/design/README.md`; editing historical examples would obscure the audit trail. | No active implementation instruction remains; current contracts and Constitution govern. |
| D4 registry-spec stale entries | Active scientific registry specs stay at the design root and must be corrected by a registry-spec version bump, not silent edits to historical scientific claims. | `roadmap.md` Later Horizons: registry-spec version bump |
| CS-1109 oracle-truth-path same-substrate regression | Batch 3 same-substrate regression covers the BLAS-sensitive data path with `include_truth = FALSE`; direct `include_truth = TRUE` oracle-truth evidence is deferred to avoid adding a costly unit test. | CS-1118 release-gate evidence decision |
| Residual R CMD check documentation/package-structure warnings | Batch 5 removed the M17 duplicate `cs_tidy_run` alias warning, fixed Batch 4 codoc drift, and fixed the `%||%` Rd-name warning. Remaining warnings/notes are broader package documentation and repository-structure backlog: undocumented exported DGP/estimator aliases, undocumented arguments in existing Rd files, `.claude` inclusion, top-level dev/tmp files, DESCRIPTION title punctuation, and tidy-eval global-variable notes. | CS-1118 release gate must either fix these or record an explicit release-manager deferral. |

## Rejected

| Ticket/Finding | Rationale |
| --- | --- |
| M14 as originally stated | Rejected by audit Rev 2; estimators may request non-canonical tau. Residual tau/truth mismatch is handled as a reporting warning in CS-1116. |
| D5 no authority model | Resolved by the governance index, authority order, active packet structure, RFC process, release gate, and archive model installed under CS-1100. |

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
