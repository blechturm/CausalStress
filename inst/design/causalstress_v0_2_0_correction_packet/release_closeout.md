# CausalStress v0.2.0 Release Corrections Closeout

**Status:** ACTIVE
**Closeout state:** Open; CS-1225--CS-1227 remain incomplete
**Date opened:** 2026-07-24
**Date closed:** TBD
**Packet:** `causalstress_v0_2_0_correction_packet`

## Shipped

| Ticket | Evidence |
| --- | --- |
| CS-1220 | Constitution v2.0.1 was ratified and applied with the accepted uppercase `X1...Xk` clause and three `real-data DGP` prose corrections. Independent Claude review on 2026-07-24 returned **APPROVE** with no blocker, major, or minor findings and confirmed the v2.0.0 history, `type="real"`, historical RFCs, closed Wave 1 spec, and implementation artifacts remain unchanged. Local follow-up confirmed the authoritative R YAML parse and Git's declared LF normalization. |
| CS-1221 | Added one shared canonical synthetic-covariate predicate across internal contract validation, all-version registry execution, and public certification. All 24 registered versions pass; malformed and absent covariate names fail closed; validation restores RNG state. Independent Claude final review returned **APPROVE** with no actionable findings. |
| CS-1222 | Corrected schema-4 identity so QST curve rows share one `score_fingerprint` and carry unique deterministic `score_row_fingerprint` values. Runner metadata, pins, tidy/collector projections, science payloads, and result/batch audit surfaces preserve both levels. Independent Claude final review returned **APPROVE** with no actionable findings. |
| CS-1223 | Replaced the stale v0.1.x README with the current v0.2.0 scientific boundary, 12-DGP/24-version and 8-estimator inventories, structured runner result, canonical score collection, honest Airlock and experimental-parallel claims, installation, and version-derived citation workflow. Corrected affected roxygen and regenerated README/help/namespace artifacts. Claude returned **APPROVE WITH NON-BLOCKING NOTES**, explicitly adjudicating the nine DGP-version exports as the correct synchronization of authoritative `@export` declarations. |
| CS-1224 | Defined the packet lifecycle vocabulary, normalized both active packets, reclassified the closed Wave 1 packet only in the authority index, narrowed the v0.2.0 roadmap boundary, and routed persistence, real-data/feature-roster/extension risks, families/CATE planning, and the full documentation release. Claude returned **APPROVE WITH NON-BLOCKING NOTES** and confirmed the closed Wave 1 packet remains byte-identical. |

## Deferred

| Ticket/Finding | Rationale | Destination |
| --- | --- | --- |
| Persistence retirement | The archived `qs` dependency is a bounded maintenance and recoverability problem, not part of the v0.2.0 typed-scoring release. | v0.2.1 spec/packet |
| CATE execution and parameterized families | Both need a deeper scientific planning session; families are the more immediate scientific need, while a bounded parallel CATE track may clarify unit-level contracts. | v0.3.0 planning gate; version/scope not yet authorized |
| Real-data DGPs, feature roster, and synthetic-extension naming | No real-data rows or public DGP-registration API exist. A runner-supplied feature roster must precede real-data support; synthetic naming relaxation may require a constitutional amendment. | `horizon.md`; future real-data/extension RFC |
| Full documentation program | Per-DGP reports, pkgdown, canonical workflow, and user-defined estimator/DGP contract vignettes deserve a dedicated release after their target surfaces are settled. | Separately versioned documentation release |
| `DESCRIPTION` release date | The current date predates the correction closeout; changing it before the release gate would immediately make it stale again. | CS-1225 final release-metadata audit |

## Rejected

| Ticket/Finding | Rationale |
| --- | --- |
| Itemize the nine synchronized DGP-version exports in NEWS | The source already declared all versioned generators public and the generated sync changes no scientific behavior. Itemizing it as a feature would overstate a correction to stale generated artifacts; the existing release-hygiene note covers the roxygen correction at the appropriate level. |
| Treat the README installation organization as unverified | Local Git configuration resolves `origin` to `https://github.com/blechturm/CausalStress.git`, exactly matching `pak::pak("blechturm/CausalStress")`. |

## Release Gate Evidence

| Gate | Evidence | Result |
| --- | --- | --- |
| Constitution v2.0.1 ratification/application | Accepted synthesis applied verbatim; independent Claude review returned **APPROVE** on 2026-07-24. Local checks confirmed `git diff --check`, `*.md`/`*.yml` `eol=lf` attributes, exact amendment/live-clause text, unchanged v2.0.0 history and `type="real"` count, no implementation-path changes, and successful `yaml::read_yaml()` parsing of all eight correction tickets. | Pass |
| Synthetic covariate validation | Focused contract/RNG tests, strict executable validation of all 24 registered versions, malformed-name rejection matrix, full package suite, and independent Claude review. | Pass |
| QST record/row identity | Focused schema-4, pin, projection, science, audit, and historical-resume tests; full package suite; and independent Claude review. | Pass |
| README and roxygen truthfulness | `README.Rmd` rendered with all evaluated examples; `devtools::document()` regenerated help/namespace artifacts; focused stale-claim searches passed; final-tree Windows `R CMD check` passed examples, documentation, and the full tests with 0 errors, 0 warnings, and one environment-only time-verification NOTE. Claude independently returned **APPROVE WITH NON-BLOCKING NOTES** and accepted the namespace sync. | Pass |
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
| Audit and ticket routing | Authority index lifecycle definitions, active packet headers/YAML, roadmap, horizon, and both closeouts were reconciled. All CS-1224 deferrals have named destinations; Claude confirmed the closed Wave 1 packet is byte-identical. | Pass |
| Known constitutional violations | TBD | TBD |

## Final Release Decision

The public v0.2.0 tag remains blocked until every ticket in this packet and the
existing CI packet is complete, or explicitly deferred/rejected with a
maintainer-approved rationale that does not violate the Constitution.
