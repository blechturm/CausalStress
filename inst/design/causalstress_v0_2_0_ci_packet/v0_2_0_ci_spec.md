# CausalStress v0.2.0 CI Packet Spec

**Status:** FINAL
**Packet:** `causalstress_v0_2_0_ci_packet`
**Date opened:** 2026-06-18
**Authority:** Historical final authority for the v0.2.0 CI release gate
**Package version:** remains `0.2.0`; this packet gates the public v0.2.0 tag and does not authorize a semantic package-version bump.

## Objective

Implement the roadmap Phase 1 continuous-enforcement infrastructure that was
identified as a tag blocker during the v0.2.0 Wave 1 release-gate review:

1. Add GitHub Actions CI for package check, full tests, validation, coverage,
   linting, and reproducibility-substrate evidence.
2. Make the CI jobs compatible with the already-implemented v0.2.0 Wave 1 typed
   scoring surface.
3. Consume `inst/design/release_ci_playbook.md` so local WSL, branch, main, and
   tag CI are treated as separate evidence.
4. Close the public v0.2.0 tag blocker by recording green CI evidence in a CI
   packet closeout.

## Scope

- Add GitHub Actions workflows under `.github/workflows/`.
- Add and follow `inst/design/release_ci_playbook.md`.
- Run `R CMD check` on the declared CI matrix: Ubuntu release, Ubuntu devel,
  Windows release, and macOS release.
- Run the full testthat suite with `NOT_CRAN=true`.
- Run the registry / DGP validation suite used by the release gate.
- Add coverage reporting with `covr` and preserve coverage evidence as a CI
  artifact. External services such as Codecov are optional and not required for
  this packet unless configured safely.
- Add a lint job with either a clean `lintr` result or an explicit repository
  baseline that makes remaining lint debt visible without blocking unrelated
  release-gate work.
- Add a reproducibility-substrate job that records R version, platform,
  `.libPaths()`, `RNGkind()`, `extSoftVersion()`, relevant thread environment,
  and a same-substrate include-truth bitwise probe.
- Adjust v0.2.0 golden-value tests if needed so exact checks are reserved for
  same-substrate identity/passthrough assertions and cross-substrate DGP/model
  values use documented tolerances.
- Run a local WSL/Ubuntu rehearsal before pushing CI-sensitive changes, because
  this development machine has WSL access. Record the commands and results in
  closeout. WSL evidence is useful but does not replace remote CI.
- Update release-gate and packet closeout evidence with CI run URLs, commit SHA,
  job matrix, branch/main/tag CI status, and final status.

## Non-Scope

- No package version bump beyond `0.2.0`.
- No scientific DGP logic changes.
- No CATE Wave 2 implementation.
- No gatekeeper recalibration.
- No pkgdown deployment or vignette authoring; documentation deployment remains
  in the later documentation cycle.
- No mandatory dependency on a third-party coverage service token.
- No claim that a local WSL pass is sufficient to validate the release tag.

## Design

### Workflow layout

The implementation may use one workflow file or several. The required logical
jobs are:

- `check`: `R CMD check` via `r-lib/actions`, matrixed over Ubuntu release,
  Ubuntu devel, Windows release, and macOS release.
- `test-validation`: full `testthat` plus the explicit validation suite under
  `NOT_CRAN=true`.
- `coverage`: `covr::package_coverage()` with artifact upload for coverage
  results.
- `lint`: `lintr::lint_package()` or a reviewed lint-baseline mode if existing
  style debt is too large for immediate cleanup.
- `substrate`: print substrate evidence and run the include-truth bitwise probe.

The workflows should trigger on pull requests and pushes to the active branch
and default branch. If branch names are hardcoded, they must include `v0.2.0`;
prefer generic `pull_request` plus `push` rules that remain useful after merge.

### CI evidence separation

The CI packet adopts the release order in `release_ci_playbook.md`: local
Windows evidence, local WSL/Ubuntu evidence when applicable, branch CI,
main/default-branch CI, and tag-triggered CI are all distinct. A green branch
run does not prove the tag run. A public v0.2.0 tag is not release-valid until
the tag-triggered run is green.

### Local WSL rehearsal

Because this development machine has Ubuntu available through WSL, any CI packet
implementation touching workflows, executable R code, tests, coverage, lint, or
release-gate logic must run a local WSL rehearsal before push unless the
maintainer records why it was skipped. The minimum rehearsal is defined in
`release_ci_playbook.md` and must include full tests, validation, and package
check or the closest WSL-available equivalents.

### Dependency posture

CI should install package dependencies using the standard R action stack. Optional
scientific estimators should not make package check brittle merely because a
Suggests package is unavailable on one platform. The CI design must make any
choice explicit:

- either install the optional estimator dependencies needed for the full suite,
- or let tests skip honestly through existing `skip_if_not_installed()` guards,
- or record a deliberate platform-specific skip with rationale.

`_R_CHECK_FORCE_SUGGESTS_` must not silently turn optional estimator availability
into a false package failure unless the packet explicitly chooses that stricter
policy.

### Golden-value tolerance policy

Exact/tight golden-value checks are valid for schema-preservation identities
where the typed row is compared against an already-computed in-process legacy
value. Absolute generated-data or fitted-model reference values are
cross-substrate-sensitive and must use a documented tolerance or be explicitly
limited to a declared reference substrate. This packet consumes the Batch 3/4
review note routed to `horizon.md`.

### Closeout

The final CI packet closeout must record:

- CI workflow file names.
- Commit SHA.
- Local Windows and WSL commands/results.
- GitHub branch, main/default-branch, and tag run URLs or an explicit statement
  that a later tag-triggered closeout step remains pending.
- Matrix dimensions and job statuses.
- R CMD check status.
- Full test / validation status.
- Coverage and lint status.
- Reproducibility-substrate artifact or log evidence.
- Whether the public v0.2.0 tag is unblocked.

## Constitutional Compliance

- Article II: CI records reproducibility substrate and exercises the
  same-substrate include-truth bitwise probe.
- Article III: CI runs the typed estimator-output contract tests and airlock
  tests from Wave 1.
- Article V and VI: CI runs schema-4 fit/score identity and atomic-persistence
  tests.
- Article VII: CI provides the continuous enforcement mechanism required for
  DGP immutability compliance.

## Acceptance Criteria

- `.github/workflows/` contains reviewed CI workflow(s) covering check, tests,
  validation, coverage, lint, and substrate evidence.
- `inst/design/release_ci_playbook.md` exists, is indexed in
  `inst/design/README.md`, and is cited by the final CI release-gate ticket.
- Local WSL/Ubuntu rehearsal evidence is recorded or explicitly skipped with a
  maintainer-approved rationale.
- `R CMD check` passes in CI with no errors or warnings on the required matrix,
  with any notes classified and routed.
- Full tests pass in CI under `NOT_CRAN=true` or documented optional-dependency
  skips.
- The validation suite passes in CI and records registry row counts and
  `all_valid=TRUE`.
- Coverage evidence is produced and archived or uploaded.
- Lint evidence is produced; any baseline or exclusions are documented in the
  packet closeout.
- The substrate job records R/platform/library/RNG/thread evidence and
  `include_truth_bitwise=TRUE`.
- Golden-value tests are safe under the CI matrix: cross-substrate reference
  values use tolerances or are reference-substrate-gated.
- The packet closeout records branch, main/default-branch, and tag CI
  URLs/status and explicitly unblocks or refuses the public v0.2.0 tag.

## Audit / Review Findings Consumed

| Finding | Disposition | Ticket |
| --- | --- | --- |
| v0.2.0 Batch 4 review M2: public tag blocked by absent CI | ticketed | CS-1210--CS-1214 |
| v0.2.0 Batch 3/4 review note: golden-value cross-substrate tolerances | ticketed | CS-1213 |
| ledgr release CI lessons: branch/main/tag evidence separation, WSL rehearsal, coverage as separate gate | adapted | CS-1210--CS-1214 |

## Open Decisions

| Decision | Owner | Required before |
| --- | --- | --- |
| Whether coverage is artifact-only or uploaded to an external service | maintainer | CS-1212 implementation |
| Whether lint is strict green immediately or starts with a reviewed baseline | maintainer | CS-1212 implementation |
