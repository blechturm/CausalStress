# CausalStress v0.2.0 Release Correction Tickets

**Status:** ACTIVE
**Packet:** `causalstress_v0_2_0_correction_packet`

## Batch 0 - Constitutional Application

### CS-1220 - Ratify and apply Constitution v2.0.1

- **Batch:** 0
- **Source:** accepted `rfc/20260722_covariate_naming_synthesis.md`
- **Motivation:** The accepted synthesis authorizes a patch correction, but the
  Constitution and governance index still identify v2.0.0 and the validator
  cannot enforce uppercase names before the amendment is applied.
- **Files:** `inst/design/CAUSAL_STRESS_CONSTITUTION.md`,
  `inst/design/README.md`, `NEWS.md`
- **Constitutional check:** Follow the Constitution's patch-amendment process;
  preserve the v2.0.0 history entry and all historical RFC text verbatim.
- **Test obligation:** Review proves the v2.0.1 header/history and three live
  `real-data DGP` sites exactly match the accepted synthesis; `type = "real"`
  remains unchanged; no implementation or DGP artifact changes.
- **Review gate:** Maintainer ratification/application review before CS-1221.
- **Disposition:** complete_after_review

## Batch 1 - Contract Corrections

### CS-1221 - Enforce canonical synthetic covariate names

- **Batch:** 1
- **Source:** accepted DGP-contract RFC; Constitution v2.0.1; Article VII
  continuous enforcement
- **Motivation:** All released DGPs already emit canonical uppercase names, but
  no validation surface enforces the constitutional contract and lowercase
  `x1` currently passes internal validation.
- **Files:** `R/cs-contracts.R`, `R/cs-validate-registry.R`,
  `R/cs-validate-dgp.R`, `inst/design/contracts.md`, `tests/testthat/`
- **Constitutional check:** Implement only after CS-1220; do not alter any DGP
  generator, version, truth, RNG, or the `type = "real"` discriminator.
- **Test obligation:** One shared predicate is exercised by internal validation,
  every registered `(dgp_id, version)`, and public synthetic certification. All
  24 versions pass; lowercase, gap, `X0`/zero-padded, bare/non-numeric `X`, and
  no-covariate probes fail with `causalstress_dgp_error`; RNG state is preserved.
- **Review gate:** Focused constitutional-contract review and all-version
  validation pass.
- **Disposition:** complete_after_review

### CS-1222 - Correct QST score-record and row identity

- **Batch:** 1
- **Source:** closed Wave 1 spec requirement at
  `causalstress_v0_2_0_spec_packet/v0_2_0_spec.md:155`; Constitution Article V
  §5.2
- **Motivation:** Current schema-4 code includes `tau_id` in
  `score_fingerprint`, incorrectly making each QST point a separate score record.
- **Files:** `R/cs-fingerprint.R`, `R/cs-contracts.R`, `R/cs-runner.R`,
  `R/cs-accessors.R`, `R/cs-result-to-row.R`, `R/cs-pins.R`, collectors and
  `R/cs-provenance.R`, audit/science surfaces as required,
  `inst/design/contracts.md`,
  `tests/testthat/`
- **Constitutional check:** One fit × target × metric is one score record; tau is
  a coordinate. Keep schema version 4 because it is not publicly released;
  schemas 1-3 remain historical and fail closed for resume.
- **Test obligation:** QST rows share one deterministic `score_fingerprint` and
  have unique deterministic `score_row_fingerprint` values; scalar/status rows
  have canonical row identities; metadata and every projection preserve both;
  current per-row score-fingerprint uniqueness assertions are replaced with the
  governed curve/row assertions.
- **Review gate:** Schema-4 identity and compatibility-surface review before
  release documentation is frozen.
- **Disposition:** complete_after_review

## Batch 2 - Release Surfaces and Governance

### CS-1223 - Repair release-blocking README and roxygen claims

- **Batch:** 2
- **Source:** release audit and maintainer documentation-scope decision
- **Motivation:** README and generated help still describe v0.1.x, advertise a
  nonexistent `cs_register_dgp()`, misstate parallel/typed-scoring status, and
  document `cs_run_single()` with the wrong return shape.
- **Files:** `README.Rmd`, `README.md`, affected `R/*.R` roxygen sources,
  generated `man/*.Rd`, `NEWS.md`
- **Constitutional check:** Documentation must not overclaim scientific status,
  real-data support, implemented estimands, or security/concurrency guarantees.
- **Test obligation:** Render/check README; regenerate documentation; focused
  search finds no stale v0.1.x release claim or nonexistent public DGP
  registration workflow; examples parse/run where practical; `R CMD check`
  reports no documentation errors or warnings.
- **Review gate:** Release-facing truthfulness review. Full pkgdown/vignette work
  is explicitly deferred.
- **Disposition:** open

### CS-1224 - Normalize release governance and route deferrals

- **Batch:** 2
- **Source:** maintainer packet-status decision; accepted DGP-contract RFC open
  risks; release-boundary review
- **Motivation:** The governance index must distinguish the finished Wave 1
  history from active correction/CI work, use consistent status meanings, and
  stop presenting CATE/families/docs as unfinished work inside the public
  v0.2.0 release boundary.
- **Files:** `inst/design/README.md`, `inst/design/horizon.md`,
  `inst/design/roadmap.md`, active correction/CI packet status and closeout files
- **Constitutional check:** Do not edit the closed Wave 1 v0.2.0 spec. Preserve
  deferred CATE and real-data obligations without claiming implementation.
- **Test obligation:** Index names all active/final packets unambiguously;
  `DRAFT/ACTIVE/FINAL/SUPERSEDED/ARCHIVED` meanings are used consistently;
  future synthetic extension names, feature-roster design, real-data support,
  parameterized families, CATE planning, and the full documentation release are
  routed to horizon/roadmap with no v0.2.0 release claim.
- **Review gate:** Governance consistency review before local release gate.
- **Disposition:** open

## Batch 3 - Release Gate and Publication

### CS-1225 - Run the post-correction local release gate

- **Batch:** 3
- **Source:** `inst/design/release_gate.md`,
  `inst/design/release_ci_playbook.md`, CS-1214
- **Motivation:** The June local evidence predates the constitutional,
  validation, QST identity, and documentation corrections and cannot close the
  final tag gate unchanged.
- **Files:** tests/tools as required,
  `inst/design/causalstress_v0_2_0_correction_packet/release_closeout.md`,
  `inst/design/causalstress_v0_2_0_ci_packet/release_closeout.md`
- **Constitutional check:** Read the release gate before execution; refuse
  closeout if any known constitutional violation remains open.
- **Test obligation:** Fresh focused tests, full tests, strict registry
  validation, lint, coverage evidence, substrate evidence, Windows and WSL/local
  `R CMD check`, acceptance-criteria audit, and clean worktree review are
  recorded with commands, substrate, commit SHA, and results.
- **Review gate:** Independent final local-gate review; evidence feeds CS-1214.
- **Disposition:** blocked_by_CS-1221_CS-1222_CS-1223_CS-1224

### CS-1226 - Publish the immutable v0.1.10 archival release

- **Batch:** 3
- **Source:** maintainer release-lineage decision; v0.1.10 governed closeout
- **Motivation:** Campaign provenance names package v0.1.10, but the existing
  immutable annotated tag has not been published in the public release history.
- **Files:** no repository content change expected; Git tag/release metadata and
  closeout evidence only
- **Constitutional check:** Push the existing tag object unchanged; never move,
  delete, or recreate it.
- **Test obligation:** Verify annotated tag `v0.1.10` resolves to commit
  `d05164a856b3e19101b989021f20dabe0b2a00a8`; publish it with an archival/pre-CI
  note referencing its governed closeout; record the public URL and immutable
  object/commit IDs.
- **Review gate:** Maintainer approval immediately before external publication.
- **Disposition:** open

### CS-1227 - Complete remote CI and publish v0.2.0

- **Batch:** 3
- **Source:** CS-1214; `inst/design/release_ci_playbook.md`; correction packet
- **Motivation:** v0.2.0 is not public until all correction tickets close and
  branch, main/default-branch, and tag-triggered CI evidence is green.
- **Files:** active packet closeouts, `inst/design/README.md`, release metadata;
  no unreviewed implementation changes
- **Constitutional check:** Both active packets must report no open release
  blocker; the v0.2.0 tag must point to the reviewed commit and never be moved.
- **Test obligation:** Record green branch CI, main/default-branch CI where used,
  and tag-triggered v0.2.0 CI URLs/status; confirm package version 0.2.0,
  Constitution v2.0.1, fresh local gate, immutable tag target, complete ticket
  routing, and final packet statuses.
- **Review gate:** Maintainer final release decision after CS-1214, CS-1225, and
  CS-1226; close correction and CI packets as `FINAL` only after tag CI is green.
- **Disposition:** blocked_by_CS-1214_CS-1225_CS-1226

## Release-Gate Ticket Requirement

CS-1225 and CS-1227 consume `inst/design/release_gate.md` and
`inst/design/release_ci_playbook.md`. All gate evidence must be recorded in
`release_closeout.md`; a local pass alone cannot authorize the public tag.
