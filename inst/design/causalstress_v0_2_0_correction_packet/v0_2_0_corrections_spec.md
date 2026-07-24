# CausalStress v0.2.0 Release Corrections Spec

**Status:** ACTIVE
**Packet:** `causalstress_v0_2_0_correction_packet`
**Date opened:** 2026-07-24
**Authority:** Active while named in `inst/design/README.md`
**Package version:** remains `0.2.0`

## Objective

Finish the public v0.2.0 release by resolving the narrow constitutional,
schema-4 identity, release-documentation, and governance blockers discovered
after the Wave 1 implementation closeout.

This packet does **not** amend or reopen
`causalstress_v0_2_0_spec_packet/v0_2_0_spec.md`. That closed specification
remains the historical record of the Wave 1 design. This packet consumes its
existing requirements, the accepted DGP-contract RFC, and the release-gate
findings as a separate correction authority.

## Scope

- Apply the maintainer-accepted Constitution v2.0.1 patch authorized by
  `rfc/20260722_covariate_naming_synthesis.md`.
- Enforce canonical uppercase, consecutive, one-based `X1...Xk` names through
  all three synthetic-DGP validation surfaces.
- Correct schema-4 QST identity so a QST curve is one score record with multiple
  coordinate rows, each having its own row identity.
- Repair only release-blocking README and roxygen/man inaccuracies.
- Standardize active/final packet status handling without rewriting the closed
  Wave 1 specification.
- Publish the existing immutable v0.1.10 tag as an archival pre-CI release.
- Rerun the local release gate after corrections and feed the new evidence into
  the existing CI/tag gate (`CS-1214`).
- Complete branch, main, and tag CI before declaring v0.2.0 public.

## Non-Scope

- No DGP scientific logic, parameter, truth, RNG, or version changes.
- No implementation of real-data DGPs.
- No CATE Wave 2 implementation, parameterized DGP families, or gatekeeper
  recalibration.
- No `.qs` retirement or persistence redesign; that remains v0.2.1.
- No pkgdown site, DGP-report suite, or full vignette cycle.
- No public user-defined DGP registration API.
- No broad public-API cleanup beyond correcting false release-facing claims.
- No schema 1-3 migration or resume-policy change.
- No modification of the closed Wave 1 v0.2.0 specification.

## Design

### Historical packet preservation

The closed Wave 1 packet is not edited. Where its document headers still say
`ACTIVE`, the governance index and its accepted closeout identify it as final.
New work, dispositions, and evidence live in this correction packet. This avoids
silently rewriting the historical implementation authority while giving the
remaining work an active packet.

Packet lifecycle values are `DRAFT`, `ACTIVE`, `FINAL`, `SUPERSEDED`, and
`ARCHIVED` in human-facing documents, with lower-case equivalents in YAML.
Ticket dispositions remain a separate vocabulary.

### Constitution v2.0.1 and synthetic covariates

The maintainer applies the exact minimal amendment accepted in
`20260722_covariate_naming_synthesis.md`:

- uppercase, consecutive, one-based `X1...Xk` for synthetic DGPs;
- `real-data DGP` at the three live prose sites;
- unchanged machine discriminator `type = "real"`;
- unchanged historical amendment/RFC wording;
- real-data support remains deferred.

After ratification, one internal covariate-name predicate is shared by:

1. `cs_check_dgp_synthetic()`;
2. the per-version executable pass in `cs_validate_dgp_registry()`; and
3. the public `cs_validate_dgp()` synthetic-certification path.

For the current synthetic generation frame, the operational non-covariate list
is `y`, `w`, `p`, `y0`, `y1`, and `structural_te`. Every remaining column must
form exactly `X1...Xk`, where `k >= 1`. The validator rejects lowercase names,
zero-padded or zero-based names, bare `X`, non-numeric suffixes, and gaps with a
classed `causalstress_dgp_error`. This field list is operational and is not added
to the Constitution.

### QST score-record and score-row identity

The closed Wave 1 spec already requires QST points to share a score-record
identity (`v0_2_0_spec.md`, “Collection, tidy, science payload, and audit
surfaces”). The implementation currently violates that rule by including
`tau_id` in `score_fingerprint`.

Schema 4 is corrected before public release:

- `score_fingerprint` identifies one fit × target × metric × truth version ×
  scoring population. It excludes QST `tau_id`.
- `score_row_fingerprint` identifies one physical row within that score record.
  It hashes `score_fingerprint` plus a canonical row coordinate:
  - QST point rows use `tau_id`;
  - scalar ATT/ATE rows use the canonical coordinate `scalar`;
  - target-level non-comparable/error rows without a point coordinate use
    `record_status`.
- `tau_index` controls ordering and is not part of identity.
- `meta$score_fingerprints` contains the stable unique set of score-record
  fingerprints in first-occurrence order.
- `meta$score_row_fingerprints` contains one fingerprint per score-surface row.
- Pins, row projections, accessors, science payloads, and collectors preserve
  both identity levels without treating legacy ATT/QST projections as scoring
  authorities.

Schema remains version 4 because no schema-4 package release or public artifact
contract exists yet. Any local pre-release schema-4 artifacts are disposable and
must be regenerated; they are not resume-compatible after this correction.
Schemas 1-3 remain historical and unchanged.

### Minimum release-facing documentation

Before v0.2.0, update `README.Rmd`, render `README.md`, correct inaccurate
roxygen, and regenerate affected `.Rd` files. The minimum truthful surface must:

- identify the package as v0.2.0 rather than v0.1.x;
- describe ATT, ATE, and QST as implemented typed targets and CATE as staged;
- describe all currently registered DGPs as synthetic;
- present the current DGP/estimator inventory without inventing APIs;
- remove `cs_register_dgp()` and other nonexistent public-workflow claims;
- describe experimental parallel execution accurately;
- identify `cs_collect_scores()` as the canonical typed score collector and
  mark ATT/QST-specific projections as compatibility surfaces;
- correct installation and citation text; and
- document `cs_run_single()` as returning a structured result list rather than
  a one-row tibble.

The pkgdown site, DGP reports, canonical workflow vignettes, and extension
vignettes remain a dedicated later documentation release.

### Release integration

After implementation review, rerun the governed Windows and WSL/local gates:
focused tests, full tests, registry validation, lint, coverage evidence,
substrate evidence, and `R CMD check`. Record fresh evidence in this packet's
closeout and update the active CI packet where the earlier evidence is
superseded.

The existing annotated `v0.1.10` tag is immutable and points to commit
`d05164a856b3e19101b989021f20dabe0b2a00a8`. Publish that exact tag object as an
archival pre-CI release; never move or recreate it. Its release note must state
that it predates GitHub Actions and relies on its governed local closeout.

Final v0.2.0 release sequencing remains governed by
`release_ci_playbook.md`: branch CI, main/default-branch CI, and tag-triggered CI
are distinct evidence. Both the correction packet and CI packet close only after
the required remote evidence is recorded.

## Constitutional Compliance

- Preamble and Article III: applies the accepted patch through the required
  maintainer ratification action before enforcing uppercase names.
- Article II and VII: changes no released DGP implementation and adds continuous
  validation of the corrected synthetic contract.
- Article V §5.2 and Article VI: restores the governed QST score-record grain and
  preserves unique row identity for atomic/tabular handling.
- Historical DGP versions, truth tables, and RNG guarantees remain unchanged.

## Acceptance Criteria

- Constitution header, date/status, amendment history, §1.3, §1.7, and §3.2 are
  consistent with ratified v2.0.1; `type = "real"` and historical records are
  unchanged.
- One shared predicate validates synthetic covariates across all three required
  surfaces.
- All 24 registered DGP versions pass the predicate; focused negative tests cover
  lowercase, missing/gapped, malformed, and absent covariate names with classed
  errors.
- A scored QST curve has one `score_fingerprint` across all tau rows and one
  unique deterministic `score_row_fingerprint` per tau row.
- Scalar and target-level status rows receive deterministic row fingerprints.
- Metadata, pins, accessors, row projections, collectors, and science payloads
  preserve both identity levels losslessly.
- Focused tests prove schema 4 remains deterministic and schemas 1-3 are not
  admitted as schema-4 resume targets.
- README and generated manual pages make no known false claims about version,
  targets, DGP type, public registration APIs, parallelism, collection, or return
  shapes.
- The closed Wave 1 specification is byte-for-byte untouched by this packet.
- Active packet documents/YAML use the standardized status vocabulary and close
  as `FINAL`/`final` only after their work is complete.
- The exact existing v0.1.10 annotated tag is published without movement and is
  labeled archival/pre-CI.
- Fresh Windows and WSL/local release evidence passes after all corrections.
- Branch, main/default-branch, and v0.2.0 tag CI are green and recorded before
  the release is declared public.
- Every ticket is complete, deferred with a destination, or rejected with a
  rationale in `release_closeout.md`.

## Findings Consumed

| Finding | Disposition | Ticket |
| --- | --- | --- |
| Accepted DGP-contract RFC: apply Constitution v2.0.1 | ticketed | CS-1220 |
| Accepted DGP-contract RFC: missing covariate enforcement | ticketed | CS-1221 |
| Wave 1 spec §Collection: QST points must share score-record identity | ticketed | CS-1222 |
| Release audit: README/roxygen materially stale | ticketed | CS-1223 |
| Governance audit: packet statuses and release boundary drift | ticketed | CS-1224 |
| Local v0.1.10 tag absent from public release history | ticketed | CS-1226 |
| CI packet: remote branch/main/tag evidence pending | coordinated, not duplicated | CS-1214, CS-1227 |

## Open Decisions

None. The maintainer decisions consumed by this packet are recorded in the
accepted RFC and the release-planning discussion. New scientific or public-API
questions must be deferred rather than expanded into v0.2.0.
