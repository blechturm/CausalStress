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

## Deferred

| Ticket/Finding | Rationale | Destination |
| --- | --- | --- |
| D4 registry-spec stale entries | Active scientific registry specs stay at the design root and must be corrected by a registry-spec version bump, not silent edits to historical scientific claims. | `roadmap.md` Later Horizons: registry-spec version bump |

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
