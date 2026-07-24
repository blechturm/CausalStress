# CausalStress v0.2.0 Release Corrections Closeout

**Status:** OPEN
**Date opened:** 2026-07-24
**Date closed:** TBD
**Packet:** `causalstress_v0_2_0_correction_packet`

## Shipped

| Ticket | Evidence |
| --- | --- |
| CS-1220 | Constitution v2.0.1 was ratified and applied with the accepted uppercase `X1...Xk` clause and three `real-data DGP` prose corrections. Independent Claude review on 2026-07-24 returned **APPROVE** with no blocker, major, or minor findings and confirmed the v2.0.0 history, `type="real"`, historical RFCs, closed Wave 1 spec, and implementation artifacts remain unchanged. Local follow-up confirmed the authoritative R YAML parse and Git's declared LF normalization. |
| CS-1221 | Added one shared canonical synthetic-covariate predicate across internal contract validation, all-version registry execution, and public certification. All 24 registered versions pass; malformed and absent covariate names fail closed; validation restores RNG state. Independent Claude final review returned **APPROVE** with no actionable findings. |
| CS-1222 | Corrected schema-4 identity so QST curve rows share one `score_fingerprint` and carry unique deterministic `score_row_fingerprint` values. Runner metadata, pins, tidy/collector projections, science payloads, and result/batch audit surfaces preserve both levels. Independent Claude final review returned **APPROVE** with no actionable findings. |

## Deferred

| Ticket/Finding | Rationale | Destination |
| --- | --- | --- |

## Rejected

| Ticket/Finding | Rationale |
| --- | --- |

## Release Gate Evidence

| Gate | Evidence | Result |
| --- | --- | --- |
| Constitution v2.0.1 ratification/application | Accepted synthesis applied verbatim; independent Claude review returned **APPROVE** on 2026-07-24. Local checks confirmed `git diff --check`, `*.md`/`*.yml` `eol=lf` attributes, exact amendment/live-clause text, unchanged v2.0.0 history and `type="real"` count, no implementation-path changes, and successful `yaml::read_yaml()` parsing of all eight correction tickets. | Pass |
| Synthetic covariate validation | Focused contract/RNG tests, strict executable validation of all 24 registered versions, malformed-name rejection matrix, full package suite, and independent Claude review. | Pass |
| QST record/row identity | Focused schema-4, pin, projection, science, audit, and historical-resume tests; full package suite; and independent Claude review. | Pass |
| README and roxygen truthfulness | TBD | TBD |
| Focused tests | TBD | TBD |
| Full test suite | TBD | TBD |
| Registry validation | TBD | TBD |
| Lint | TBD | TBD |
| Coverage | TBD | TBD |
| Reproducibility substrate | TBD | TBD |
| R CMD check - Windows | TBD | TBD |
| R CMD check - WSL/Ubuntu | TBD | TBD |
| v0.1.10 archival publication | TBD | TBD |
| Branch/main/tag CI | Coordinated through CS-1214 and CS-1227 | TBD |
| Audit and ticket routing | TBD | TBD |
| Known constitutional violations | TBD | TBD |

## Final Release Decision

The public v0.2.0 tag remains blocked until every ticket in this packet and the
existing CI packet is complete, or explicitly deferred/rejected with a
maintainer-approved rationale that does not violate the Constitution.
