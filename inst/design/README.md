# CausalStress Design Governance Index

**Status:** Active authority index
**Last updated:** 2026-06-13
**Current active packet:** `causalstress_v0_1_10_spec_packet`
**Latest completed packet:** none under the packet system; prior releases are historical records in `archive/`.

This file is the entry point for design and governance work in `inst/design/`.
Every design-document add, move, retirement, or authority change must update this
index in the same change.

## Authority Order

| Level | Role | Binding rule |
| --- | --- | --- |
| 1 | `CAUSAL_STRESS_CONSTITUTION.md` | Supreme authority. A packet, RFC, roadmap, or audit cannot override it. Amend only through the Constitution's own amendment process. |
| 2 | `contracts.md` | Operational contract index for code-facing surfaces. Mutable by spec packet only where the Constitution is silent. |
| 3 | Active spec packet | The only authorization for implementation work in an active release cycle. |
| 4 | Accepted RFC syntheses | Binding within their stated scope until consumed by a packet or superseded by a later accepted synthesis. |
| 5 | `roadmap.md` | Milestone sequence and planning horizon. |
| 6 | Audits and reviews | Findings must be routed before the next release gate: ticketed, deferred with rationale, or rejected with rationale. |
| 7 | Spikes and `horizon.md` | Non-binding research notes and parking lot. |
| 8 | Historical packets and archived designs | Records, not current implementation instructions. They remain normative evidence for reading, validating, or migrating artifacts produced in their era. |

## Reading Order

For non-trivial code or design work, read in this order:

1. `CAUSAL_STRESS_CONSTITUTION.md`
2. `contracts.md`
3. The active packet in `causalstress_v0_1_10_spec_packet/`
4. Relevant accepted RFCs in `rfc/`
5. Relevant audits in `audit/`

Do not use an archived design as current instruction just because it is detailed.
If an archived document conflicts with the Constitution, `contracts.md`, or the
active packet, the current authority wins.

## Current Documents

| Path | Role | Notes |
| --- | --- | --- |
| `CAUSAL_STRESS_CONSTITUTION.md` | Constitution | Apex authority. |
| `CAUSAL_STRESS_CONSTITUTION_EXPLANATION.md` | Commentary | Non-binding explanation. |
| `WHY_A_CONSTITUTION.md` | Commentary | Non-binding rationale. |
| `contracts.md` | Contract index | Code-facing contract map. |
| `roadmap.md` | Roadmap | Active milestone arc. |
| `horizon.md` | Horizon | Deferred work and future directions. |
| `rfc_cycle.md` | Process | RFC workflow for contested or expensive decisions. |
| `release_gate.md` | Process | Release checklist. |
| `templates/` | Process | Canonical packet and closeout templates. |
| `audit/` | Audit input | Findings must be routed before release. |
| `causalstress_v0_1_10_spec_packet/` | Active packet | v0.1.10 governance and defect-repair packet. |
| `rfc/` | RFC records | Accepted syntheses become binding in stated scope. |
| `spikes/` | Exploratory notes | Informative only unless promoted. |
| `archive/` | Historical records | Superseded docs, kept verbatim. |

## Scientific Registry Specs

The following remain at the design root because they are versioned scientific
registry specifications, not ordinary implementation plans:

- `CAUSAL_STRESS_DGP_REGISTRY_1.4.0.md`
- `CAUSAL_STRESS_FAMILIES_SPEC_v3_2_final.md`

Their stale entries identified by audit findings must be corrected through a
registry-spec version bump, not by silently editing old scientific history.

## Archived Documents

The following documents were superseded and moved verbatim to `archive/`:

- `CAUSALSTRESS_DESIGN_v0.3.0.md`
- `CAUSAL_STRESS_MVP_STATUS.md`
- `CAUSAL_STRESS_V0.1.8.Patch_Spec.md`
- `V0.1.8_backlock_tickets.md`
- `DESIGN_V0.1.9_BATCHING.md`
- `CAUSAL_STRESS_METADATA.md`
- `CAUSALSTRESS_DESIGN_SENSITIVITY.md`
- `CAUSAL_STRESS_PYTHON.md`
- `CAUSAL_STRESS_DOCUMENTATION_ARCHITECTURE.md`
- `THREADMODEL.md`
- `CAUSAL_STRESS_ROADMAP.md`

## Pre-CRAN Compatibility Policy

Pre-CRAN artifact formats, fingerprint schemas, and experimental APIs may change
when a spec packet authorizes the change. Anything frozen by the Constitution
does not get a pre-CRAN exception: DGP outputs by `(dgp_id, version)`, truth
tables, and RNG obligations must remain governed by the Constitution.
