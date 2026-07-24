# CausalStress Design Governance Index

**Status:** ACTIVE authority index
**Last updated:** 2026-07-24
**Current active packets:** `causalstress_v0_2_0_correction_packet` (release corrections) and `causalstress_v0_2_0_ci_packet` (CI/tag gate)
**Latest final packet:** `causalstress_v0_2_0_spec_packet` (Wave 1 implementation final; public v0.2.0 tag blocked until the active correction and CI packets close)
**Accepted RFCs (authority level 4):** RFC-1 Estimand Registry — `rfc/20260616_estimand_registry_synthesis.md`, accepted 2026-06-16 and consumed by ratified Constitution v2.0.0. DGP contract terminology clarifications — `rfc/20260722_covariate_naming_synthesis.md` (seed `rfc/20260722_covariate_naming_seed.md`, response `rfc/20260722_covariate_naming_response.md`, independent review `rfc/20260722_covariate_naming_synthesis_review.md`), accepted 2026-07-24 and consumed by ratified Constitution v2.0.1 and correction ticket CS-1221.
**Open RFCs (non-binding):** none.
**Constitution:** **v2.0.1 ratified 2026-07-24** (`CausalStress Scientific Protocol`) — see `CAUSAL_STRESS_CONSTITUTION.md` Amendment History, `rfc/20260722_covariate_naming_synthesis.md`, and `rfc/20260722_covariate_naming_synthesis_review.md`.
**Active release boundary:** v0.2.0 ships Constitution v2.0.1, typed ATT/ATE/QST scoring, corrected schema-4 identities, CI enforcement, and the emergency replacement of archived `qs` runtime persistence with base-R RDS under correction ticket CS-1228. CATE execution, parameterized families, real-data DGP support, a public DGP-extension API, an authoritative feature roster, legacy `.qs` conversion, generalized persistence, and the full documentation release are later or external work requiring separate planning/packets. See `roadmap.md` and `horizon.md`.

This file is the entry point for design and governance work in `inst/design/`.
Every design-document add, move, retirement, or authority change must update this
index in the same change.

## Packet Lifecycle

Packet lifecycle uses only the following states:

| State | Meaning |
| --- | --- |
| `DRAFT` | Proposed authority. Implementation is not authorized. |
| `ACTIVE` | Current implementation or release-gate authority for its stated scope. Distinct active packets may coexist when their scopes do not conflict. |
| `FINAL` | Accepted work and closeout evidence are complete. The packet is a historical authority record and is not reopened for later releases. |
| `SUPERSEDED` | Replaced by a named successor. Retained only to explain lineage. |
| `ARCHIVED` | Removed from active authority and retained as historical context only. |

Ticket dispositions such as `open`, `implementation_complete_awaiting_review`,
and `complete_after_review` are not packet lifecycle states. Historical final
packets are preserved verbatim even if an internal header uses an older word such
as `ACTIVE` or `CLOSED`; the classification in this index controls current
authority.

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
3. The active packet, if one exists
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
| `release_ci_playbook.md` | Process | CI-backed release sequencing, local WSL gate, and tag-handling playbook. |
| `templates/` | Process | Canonical packet and closeout templates. |
| `audit/` | Audit input | Findings must be routed before release. |
| `causalstress_v0_2_0_ci_packet/` | `ACTIVE` packet | v0.2.0 CI and release-gate infrastructure packet. |
| `causalstress_v0_2_0_correction_packet/` | `ACTIVE` packet | Release-blocking constitutional, validation, QST-identity, minimal-documentation, emergency RDS-persistence, and final-publication corrections for v0.2.0. CS-1228 is complete after independent review; fresh final gates remain pending. |
| `causalstress_v0_2_0_spec_packet/` | `FINAL` packet | v0.2.0 Wave 1 typed-scoring implementation packet, closed before public tag pending CI. |
| `causalstress_v0_1_10_spec_packet/` | `FINAL` packet | v0.1.10 governance and defect-repair packet, closed on 2026-06-14. |
| `rfc/` | RFC records | Accepted syntheses become binding in stated scope. |
| `research/` | Research notes | Literature/prior-art assessments; informative only unless promoted via RFC or packet. |
| `research/prior_art_acic_2026_benchmark_audit.md` | Research note | Focused ACIC 2026 prior-art, compatibility, estimand-scope, and incorporation audit; non-binding. |
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
