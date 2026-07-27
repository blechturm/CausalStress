# CausalStress v0.2.1 Tickets

**Status:** ACTIVE — accepted after independent review
**Packet:** `causalstress_v0_2_1_spec_packet`
**Authority commit:** `0b20f12`
**Source specification:** `v0_2_1_spec.md`
**Accepted:** 2026-07-26
**Independent review:** APPROVE WITH NON-BLOCKING NOTES; F-1–F-3 incorporated

## Disposition Rules

- The `open` entries below are active initial dispositions. Work proceeds one
  batch at a time under the review protocol.
- CS-1232 through CS-1236 are independently reviewable. Any one may be deferred
  to v0.2.2 only by an explicit reviewed disposition with no partial
  implementation left in the release tree.
- An unexpected failure of the CS-1232 schema-4 identity lock or the CS-1236
  governed-vocabulary invariant is a stop-and-investigate condition, not a
  routine deferral.
- Every implemented batch stops for independent review before the next batch.

## Batch 0 — Governance and Runner Characterization

### CS-1230 — Reconcile the v0.2.1 governance and release boundary

- **Batch:** 0
- **Source:** accepted v0.2.1 specification; `roadmap.md`; scientific pass-2
  maintainer adjudication
- **Motivation:** The governance index now activates v0.2.1, but the roadmap
  still describes v0.2.0 as current and Registry 1.4.0 as a live future repair.
  Current planning surfaces must agree before implementation begins.
- **Files:** `inst/design/README.md`, `inst/design/roadmap.md`, and only directly
  affected current governance indexes; do not edit
  `CAUSAL_STRESS_DGP_REGISTRY_1.4.0.md` or historical packets.
- **Constitutional check:** Authority ordering and history preservation only;
  no scientific registry, DGP, estimand, or constitutional content changes.
- **Test obligation:** Governance review proves v0.2.0 is historical, v0.2.1 is
  the active documentation/maintenance release, Registry 1.4.0 is labelled
  historical/superseded, its successor is deferred to the families packet, and
  no archived authority was rewritten.
- **Review gate:** Batch 0 governance and characterization review.
- **Disposition:** complete_after_review

### CS-1231 — Characterize the duplicated per-seed runner contracts

- **Batch:** 0
- **Source:** simplicity audit F1; accepted v0.2.1 specification
- **Motivation:** WP-02 and future parameterized families will exercise the
  runner heavily. A behavior net is required before any later convergence of
  the duplicated seed-execution paths.
- **Files:** `tests/testthat/` and test fixtures only. The production diff under
  this ticket must be empty.
- **Constitutional check:** Preserve Article II RNG isolation, Article V
  concurrency and identities, Article VI worker isolation/atomicity, and the
  current batch/persistence contracts.
- **Test obligation:** Through supported public paths, characterize serial and
  experimental-parallel execution; board writes versus staging/gather;
  ordinary `...` and custom `tau` forwarding; thread/backend provenance;
  documented warning and progress semantics including cached work;
  deterministic output order; `skip_existing`; `force`; current-schema resume;
  schema-migration, fingerprint-mismatch, and bootstrap-CI rejection; and grid
  versus planned-batch behavior as distinct contracts. Assert observable
  results, documented order, classed failures, no-overwrite/resume behavior,
  and score/fingerprint identities—not exact progress wording, internal helper
  call order, or undocumented timing.
- **Review gate:** Batch 0 governance and characterization review; verify an
  empty production diff and route rather than fix any discovered defect.
- **Disposition:** complete_after_review

## Batch 0 Implementation Evidence

- **CS-1230:** `roadmap.md` now records v0.2.0 and its emergency RDS correction
  as completed, makes v0.2.1 the current program, reproduces the accepted six-
  batch boundary, keeps F1/F3 and future science out of scope, and labels DGP
  Registry 1.4.0 historical/superseded without editing it or any final packet.
- **CS-1231:** `test-runner-characterization.R` adds five public-path contracts:
  serial result/tau/order/identity equivalence; experimental-parallel warning,
  stage/gather, thread-cap, backend, and identity equivalence; cached no-
  overwrite and forced-recompute behavior; path-specific DGP warning counts with
  progress enabled; and distinct grid versus planned-batch return/artifact
  contracts. No `R/` file changed.
- **Focused test:** the final unsandboxed focused run passed 44 expectations.
  The first managed-sandbox attempt had two `EPERM` harness errors because
  `pins::board_temp()` could not traverse the user-profile path; no package
  assertion failed, and the same tree passed with normal filesystem access.
- **Affected tests:** force-overwrite, parallel protocol/reproducibility,
  resume, campaign configuration/forwarding/bootstrap, runner seeds, v0.1.8
  parallel gating/thread caps, and v0.1.9 runner/worker suites all passed.
- **Full suite:** passed in 223.3 seconds. The 56 warnings came from existing
  experimental-DGP, optional-estimator CI, and governed RNG-warning tests; the
  Batch 0 file emitted no warning.
- **Registry:** strict and executable validation passed 24/24 with every row
  valid.
- **Static checks:** the new test file has no lints; `git diff --check` is clean;
  the production `R/` diff and historical registry/final-packet diffs are empty.

Independent review returned **APPROVE WITH NON-BLOCKING NOTES**. Before commit,
the test-only estimator registration gained teardown, the two characterized
runner asymmetries were explicitly routed to the future F1/F3 decision, and the
current design index cross-referenced the registry successor to the families
work. The warning-message substring remains a documented characterization
limitation because the warning has no dedicated class.

CS-1230 and CS-1231 are `complete_after_review`. This acceptance does not
authorize Batch 1.

## Batch 1 — Independently Deferrable Bounded Maintenance

### CS-1232 — Retire historical fingerprint fixture builders

- **Batch:** 1
- **Source:** simplicity audit F2; accepted v0.2.1 specification
- **Motivation:** Two private production helpers manufacture only rejected
  historical test artifacts and falsely resemble supported migration writers.
- **Files:** `R/cs-fingerprint.R`,
  `tests/testthat/test-v018-fingerprint-schema.R`, and directly related tests.
- **Constitutional check:** Article V schema-4 identity and Article VI
  fail-closed resume behavior remain unchanged; legacy readers/discovery remain.
- **Test obligation:** Confirm no maintainer-declared unpublished `:::` use;
  replace fixture hashes with an unmistakable non-hash sentinel; prove missing
  and schemas 1–3 fail with the exact governed migration classes before the
  sentinel can be compared; freeze representative schema-4 fingerprints and
  prove none moves. Any schema-4 lock failure stops the batch for investigation.
- **Review gate:** Batch 1 bounded-maintenance review; this ticket may be
  explicitly deferred to v0.2.2 only with no partial diff.
- **Disposition:** complete_after_review

### CS-1233 — Remove the obsolete thread setter

- **Batch:** 1
- **Source:** simplicity audit F4; accepted v0.2.1 specification
- **Motivation:** `cs_enforce_threads()` is unused package machinery beside the
  actual scoped environment-cap boundary.
- **Files:** `R/cs-scale-helpers.R` and focused thread/runner tests.
- **Constitutional check:** Article V concurrency and Article VI worker
  isolation; caller environment must still be restored.
- **Test obligation:** Confirm no supported or maintainer-declared unpublished
  `:::` use; remove `cs_enforce_threads()`; retain
  `cs_thread_caps_env()` plus scoped restoration; prove all caps apply and the
  caller environment is restored on success and error.
- **Review gate:** Batch 1 bounded-maintenance review; independently deferrable
  to v0.2.2 with reviewed rationale and no partial diff.
- **Disposition:** complete_after_review

### CS-1234 — Remove the unused estimator-result adapter

- **Batch:** 1
- **Source:** simplicity audit F5; accepted v0.2.1 specification
- **Motivation:** `cs_extract_estimator_result()` is a test-only forwarding
  abstraction over the canonical output normalization boundary.
- **Files:** `R/cs-helpers.R`, `tests/testthat/test-helpers-extraction.R`, and
  canonical estimator-output normalization tests.
- **Constitutional check:** Article III estimator output contract and Airlock;
  typed and supported legacy output shapes remain unchanged.
- **Test obligation:** Confirm no supported or maintainer-declared unpublished
  `:::` use; move every unique legacy fixture to
  `cs_normalize_estimator_outputs()` tests; preserve `list(att, qst, meta)` and
  typed-output behavior; remove the adapter and adapter-only tests without a
  replacement alias or new helper layer.
- **Review gate:** Batch 1 bounded-maintenance review; independently deferrable
  to v0.2.2 with reviewed rationale and no partial diff.
- **Disposition:** complete_after_review

### CS-1235 — Collapse the duplicated `cs_run_grid()` tau branch

- **Batch:** 1
- **Source:** simplicity audit F6; accepted v0.2.1 specification
- **Motivation:** The exported wrapper duplicates an entire call solely to
  distinguish omitted/`NULL` tau from a supplied value.
- **Files:** `R/cs-runner-grid.R`, `tests/testthat/test-runner-grid.R`, tau and
  identity regression tests.
- **Constitutional check:** Articles I and V; public signature, target behavior,
  validation, results, and every fit/score identity remain unchanged.
- **Test obligation:** Characterize omitted, explicit `NULL`, canonical default,
  and custom `tau` first. If observationally equivalent as specified, resolve
  one effective value and make one explicit `cs_run_seeds()` call. Preserve
  warnings, row order, score coordinates, fingerprints, and public defaults. If
  equivalence fails, stop and retain the branch for separate design review.
- **Review gate:** Batch 1 bounded-maintenance review; independently deferrable
  to v0.2.2 with reviewed rationale and no partial diff.
- **Disposition:** complete_after_review

### CS-1236 — Make governed vocabularies single-source

- **Batch:** 1
- **Source:** simplicity audit F7; Constitution Article I §1.7; accepted v0.2.1
  specification
- **Motivation:** Estimand IDs currently have two authoritative-looking
  representations, while two one-use vocabulary getters add navigation without
  protecting another boundary.
- **Files:** `R/cs-contracts.R`,
  `tests/testthat/test-v020-typed-scoring-core.R`, and directly related contract
  tests.
- **Constitutional check:** Preserve every exact governed estimand ID,
  descriptor field, non-comparable reason, error class, and public message.
- **Test obligation:** Make descriptor names the source of target IDs; assert
  descriptor names equal embedded `estimand_target_id` values; keep the exact
  non-comparable vocabulary adjacent to its validator as a literal or one named
  internal constant; remove the one-use getters without creating a vocabulary
  framework. Any ID/reason/invariant drift stops the batch for investigation.
- **Review gate:** Batch 1 bounded-maintenance review; independently deferrable
  to v0.2.2 with reviewed rationale and no partial diff.
- **Disposition:** complete_after_review

## Batch 1 Implementation Evidence

- **CS-1232:** Removed the schema-2 and schema-missing fingerprint writers from
  production. Historical fixtures now use
  `NOT-A-HASH-SCHEMA-REJECTED-BEFORE-COMPARISON`; schema-missing and schemas
  1–3 all retain the exact `causalstress_schema_migration_error` class and
  governed message before fingerprint comparison. Two representative schema-4
  hashes are hard-locked and passed before and after the edit. Legacy pin
  discovery and schema rejection remain in production.
- **CS-1233:** Removed the unused, process-mutating `cs_enforce_threads()`.
  `cs_thread_caps_env()` and `cs_with_envvar()` remain the only scoped cap
  boundary; focused tests prove all four caps apply and that initially set and
  unset caller values are restored after both success and error.
- **CS-1234:** Removed `cs_extract_estimator_result()` and its adapter-only test
  file. The list-ATT/value-QST, tabular-ATT/estimate-QST, missing-output, and
  typed-output cases now exercise `cs_normalize_estimator_outputs()` directly.
  No replacement alias or helper was introduced.
- **CS-1235:** Characterization first proved omitted, explicit `NULL`, and
  explicit `cs_tau_oracle` behavior identical. `cs_run_grid()` now resolves one
  `effective_tau` and makes one `cs_run_seeds()` call. The public `tau = NULL`
  default, sorted seed order, custom tau coordinates, result surfaces, and
  configuration, fit, truth, score-record, and score-row identities remain
  locked.
- **CS-1236:** Descriptor names are now the only source used to validate the
  four estimand IDs. The separate target-ID and non-comparable-reason getters
  are gone; the seven reasons remain as an adjacent validator literal. Tests
  lock the complete descriptor structures, name/embedded-ID invariant, every
  reason, and the exact invalid-target/reason classes and messages.
- **Use search:** No removed helper or vocabulary getter remains in `R/`,
  tests, `NAMESPACE`, `DESCRIPTION`, developer tools, or CI. No unpublished
  maintainer `:::` use was declared. No export or runtime dependency changed.
- **Scope:** Five production files changed by 32 insertions and 170 deletions
  (net -138 lines). The changes are limited to the five authorized findings;
  no DGP, oracle, RNG, persistence, runner-path unification, campaign API, or
  documentation-platform work was included.
- **Pre/post characterization:** The focused fingerprint, thread-cap, grid-tau,
  normalization, and governed-vocabulary suite passed on the pre-edit and
  post-edit trees.
- **Affected suite:** Nineteen fingerprint, resume, parallel, staging, runner,
  Airlock, collection, schema-4, and typed-scoring test files passed.
- **Full suite:** Passed in 233.1 seconds. The 56 warnings are existing
  experimental-DGP, optional-estimator CI fallback, and governed RNG-warning
  cases; none originates in Batch 1.
- **Registry and lint:** Strict/executable registry validation passed 24/24
  rows plus 153 focused expectations. Repository lint passed with
  `lint_count=0` and 34 recognized internal-helper false positives ignored by
  the governed lint script. `git diff --check` is clean.
- **Final-tree chronology:** After the full suite, validation, and governed lint
  completed, the fingerprint test received only a local readability edit that
  reuses its schema list and shortens its title. Its 20 expectations passed
  again on the final tree; no production or governance behavior changed.

Independent review returned **APPROVE** with no blocker, major, or note. The
reviewer independently reproduced both frozen schema-4 hashes, the complete F7
vocabulary, zero references to all six removed symbols, and 121 passing focused
expectations. CS-1232 through CS-1236 are `complete_after_review`. This
acceptance does not authorize Batch 2.

## Batch 2 — Quarto Platform and Source Migration

### CS-1237 — Establish the pinned Quarto/pkgdown build substrate

- **Batch:** 2
- **Source:** accepted v0.2.1 Quarto policy; independent review N1 and N3;
  `release_ci_playbook.md`
- **Motivation:** Quarto-authored vignettes make the external Quarto CLI and the
  `quarto` R package part of every documentation-aware build/check substrate.
  The requirement must be explicit and reproducible rather than IDE-dependent.
- **Files:** `DESCRIPTION`, `.Rbuildignore`, `_pkgdown.yml`, `.github/workflows/`,
  `inst/design/release_ci_playbook.md`, and minimal documentation-build scripts.
- **Constitutional check:** Documentation/build tooling only; `pkgdown` and
  `quarto` must not become runtime imports or alter package execution.
- **Test obligation:** Add `quarto` to `Suggests`, set
  `VignetteBuilder: quarto`, and use valid `quarto::html` article metadata.
  Select and record compatible pinned Quarto CLI/R-package versions. Provision
  and verify them for local Windows, local WSL, and remote R-release/R-devel
  branch/main/tag checks. Those `R CMD build`/`R CMD check` gates must build the
  vignettes and may not hide the requirement with `--no-build-vignettes` or
  `--ignore-vignettes`. Declare pkgdown as website tooling, not a runtime
  import, and retain `knitr`/`rmarkdown` as direct package dependencies only if
  the final build proves they are still required. Add a non-publishing
  site-build workflow; prove the ordinary package runtime remains independent
  of Quarto/pkgdown. Reconcile both contradictory command blocks in
  `release_ci_playbook.md`: “Required Release Order” step 2 and the “Local
  WSL/Ubuntu Gate” must document the mandatory full-vignette check for a
  documentation release. Any retained vignette-skipping command must be
  labelled only as an optional fast pre-check, never release evidence.
- **Review gate:** Batch 2 Quarto substrate and migration review.
- **Disposition:** complete_after_review

### CS-1238 — Migrate existing README and articles to Quarto

- **Batch:** 2
- **Source:** accepted v0.2.1 Quarto policy; independent review N2
- **Motivation:** The site must have one long-form narrative authoring system,
  while GitHub and pkgdown still consume the generated README.
- **Files:** `README.Rmd` → `README.qmd`, generated `README.md`, all four
  `vignettes/*.Rmd` → `.qmd`, `.Rbuildignore`, article support files, and
  source-format validation.
- **Constitutional check:** Format-only migration at this stage; no public API,
  scientific interpretation, example result, or generated contract may drift.
- **Test obligation:** Render `README.qmd` reproducibly to GFM `README.md` and
  all four articles through `quarto::html`; assert no current `.Rmd` remains at
  `README.Rmd` or under `vignettes/`; keep generated files synchronized. Carry
  every applicable CS-1229 passage forward exactly where it currently exists:
  conventional mean potential-outcome ATT does not exist under the governed
  Cauchy regime; the structural contrast is a signal anchor; it is not an ATT
  shootout; QST is the valid distributional comparison; CRN reduces Monte Carlo
  variance but does not eliminate empirical-quantile sampling uncertainty.
- **Review gate:** Batch 2 Quarto substrate and migration review, including a
  protected-prose diff against the v0.2.0 CS-1229 sources.
- **Disposition:** complete_after_review

### CS-1239 — Migrate and strictly publish all DGP dossiers through Quarto

- **Batch:** 2
- **Source:** accepted v0.2.1 DGP-report design and Quarto policy; scientific
  pass-2 adjudication; independent review N2
- **Motivation:** Each registered DGP needs one reproducible report generated
  from its authoritative YAML/QMD sidecars, with no copied narrative or
  warn-and-skip build behavior.
- **Files:** all 12 `inst/dgp_meta/<dgp_id>.Rmd` → `.qmd`, unchanged YAML
  sidecars, `dev/render_dossiers.R`, `_pkgdown.yml`, source validation, and
  ignored/generated site output rules.
- **Constitutional check:** Articles II and VII; no DGP generator, truth, oracle,
  seed, RNG, version, status, ID, YAML metadata, or scientific content changes.
- **Test obligation:** Enumerate installed registry IDs and construct exact
  `<id>.yml`/`<id>.qmd` paths; exclude `schema.yml`, `*_cache/`, `*_files/`, and
  generated artifacts; fail on missing, duplicate, unregistered, or
  unrenderable top-level sidecars. Render all 12 in clean processes through
  Quarto only, with explicit seeds and job-local caches; remove
  `rmarkdown::render()` and any R Markdown fallback; publish each exactly once
  in navigation; identify 2 stable and 10 experimental DGPs accurately; leave
  the source worktree clean. Preserve CS-1229 protected wording byte-for-byte
  in `synth_heavytail.qmd` and preserve the heavy-tail operating rule.
- **Review gate:** Batch 2 Quarto substrate and migration review with registry,
  scientific-content, clean-tree, and all-12 render evidence.
- **Disposition:** complete_after_review

## Batch 2 Implementation Evidence

- **CS-1237:** `DESCRIPTION` now pins Quarto CLI 1.9.38, the `quarto` R
  package 1.5.1, and pkgdown 2.2.1; declares `VignetteBuilder: quarto`; and
  keeps Quarto/pkgdown in `Suggests`, outside runtime `Imports`. The release
  workflow installs the exact documentation toolchain and runs full-vignette
  checks without vignette-ignore flags. A separate non-publishing site
  workflow builds and uploads a preview and has a clean-session job that
  installs, loads, and exercises CausalStress without Quarto or pkgdown.
  `release_ci_playbook.md` now makes the full-vignette gate mandatory for a
  documentation release and labels any skipping command as an optional fast
  pre-check only.
- **CS-1238:** `README.Rmd` and all four package-vignette R Markdown sources
  were migrated to QMD. `README.qmd` renders reproducibly to the checked-in
  GFM `README.md`, and all four articles use `quarto::html`. Obsolete Pandoc
  exits were removed. The optional GenGC chunks now require both GenGC and the
  explicit `CAUSALSTRESS_RUN_OPTIONAL_DOCS=true` opt-in because the installed
  optional package currently does not satisfy those pre-existing examples;
  substantive example reconciliation remains CS-1243. Automated checks retain
  the CS-1229 heavy-tail signal-anchor, no-ATT-shootout, QST, and empirical-
  quantile-uncertainty wording.
- **CS-1239:** All 12 registry-keyed DGP report sources were migrated from Rmd
  to QMD. Eight reports now explicitly attach their already-used `dplyr`
  dependency so they render in clean processes; no scientific prose changed.
  `dev/render_dossiers.R` rejects missing, extra, duplicate, or mis-keyed
  top-level sidecars, verifies exactly 2 stable and 10 experimental records,
  requires the pinned CLI, and renders each report in a fresh Quarto process
  with explicit parameters and job-local cache paths. The former copied
  `inst/dossiers/` outputs were removed; generated reports now exist only in
  the ignored pkgdown preview. A final all-12 render produced exactly 12
  registry-keyed HTML pages with correct titles and one navigation entry each.
- **Scientific boundary:** The Batch 2 diff contains no `R/` file and no DGP
  YAML change. DGP implementations, truths, oracle algorithms, RNG, IDs,
  versions, and statuses are untouched. Source comparison found only Quarto
  front-matter changes, the eight required `dplyr` attachments, and the
  article execution guards described above. The protected heavy-tail text is
  checked directly by `tools/ci-docs.R`.
- **Local validation:** The pinned Windows toolchain passed the documentation
  validator, strict 12-pair dossier validator, full all-12 render, README
  render, pkgdown build, governed lint (`lint_count=0`), and a full
  vignette-enabled `R CMD build` plus `R CMD check --no-manual` with
  `Status: OK`. The configured Ubuntu 20.04 WSL substrate independently passed
  the pinned validators and the same full-vignette build/check with
  `Status: OK`; unavailable optional estimator packages were informational
  only. Both workflow YAML files and `tickets.yml` parse successfully, and
  `git diff --check` is clean.
- **Gate chronology:** The Windows and WSL `R CMD check` runs preceded only the
  final developer-renderer staging cleanup and its two ignored-output rules;
  no installed package file or vignette source changed afterward. On the
  resulting tree, the full-vignette Windows `R CMD build`, all-12 dossier
  render, documentation validators, lint, YAML/disposition checks, and tarball
  source-policy check all passed. The full checks were not repeated for this
  developer-tool-only correction; remote CI remains mandatory after review and
  commit.
- **Generated-state boundary:** Quarto output, support files, local libraries,
  and caches are ignored. The final dossier job removed its job-local cache
  and created no source-side `*_files/`, cache, or accidental path artifacts.
  The unrelated concurrent maintainer edits in `inst/design/horizon.md` are
  explicitly outside Batch 2 and must be excluded from its review and commit.

Independent review returned **APPROVE WITH NON-BLOCKING NOTES**. The reviewer
confirmed the Batch 2 scope, exact documentation-tool pins, non-publishing CI,
format-faithful README/article migration, all-12 registry-keyed dossier
migration, renderer isolation/cleanup, CS-1229 protected prose, and honest gate
chronology. Two transparency notes recorded the necessary removal of obsolete
Pandoc exits plus repair of malformed vignette directives, and the inherent
fact that remote CI cannot run before commit; neither requires a correction or
executable rerun.

CS-1237 through CS-1239 are `complete_after_review`. This acceptance authorizes
the Batch 2 commit only. It does not authorize Batch 3, push, merge, tag, or
publication. Remote CI remains mandatory after a future authorized push.

## Batch 3 — Canonical Documentation and Reference Truthfulness

### CS-1240 — Author the canonical typed-scoring workflow

- **Batch:** 3
- **Source:** v0.2.1 specification; roadmap documentation commitment
- **Motivation:** Users need one executable, exported-API-only path from
  discovery through typed scoring, interpretation, persistence, and audit.
- **Files:** a new canonical-workflow `.qmd`, `_pkgdown.yml`, and only necessary
  example fixtures/assets.
- **Constitutional check:** Articles I, III, V, and VI; no cross-scoring, truth
  leakage, identity ambiguity, or persistence misstatement.
- **Test obligation:** From a clean installed package and without optional
  estimators, demonstrate discovery, `cs_run_single()`, `cs_run_seeds()` or
  `cs_run_grid()`, `cs_collect_scores()`, ATT/ATE/QST interpretation, explicit
  CATE staging, summaries/plots, provenance, persistence, and resume. Teach
  single/grid entry points as ordinary and planned batching as advanced; do not
  make dual-mode `cs_run_campaign()` the conceptual center.
- **Review gate:** Batch 3 scientific and user-documentation truthfulness review.
- **Disposition:** complete_after_review

### CS-1241 — Document the user-defined estimator contract

- **Batch:** 3
- **Source:** v0.2.1 specification; existing `cs_register_estimator()` contract
- **Motivation:** The supported estimator extension boundary exists but lacks a
  complete executable guide.
- **Files:** a new estimator-extension `.qmd`, `_pkgdown.yml`, and minimal
  example fixtures/assets.
- **Constitutional check:** Article III Airlock and estimator output contract;
  ordinary extensions receive no runner-owned truth.
- **Test obligation:** With public APIs only, demonstrate a minimal registered
  estimator and valid typed result; document registration lifetime, versioning,
  target capabilities, typed and legacy output shapes, optional dependencies,
  CI behavior, Airlock restrictions, and classed failures. Execute the core
  example from a clean installed package without optional estimators.
- **Review gate:** Batch 3 scientific and user-documentation truthfulness review.
- **Disposition:** complete_after_review

### CS-1242 — Document native DGP contribution without inventing a public API

- **Batch:** 3
- **Source:** v0.2.1 specification; Constitution Articles II and VII
- **Motivation:** Contributors need the package-development DGP contract, while
  users must not be told that runtime DGP registration exists.
- **Files:** a new contributor-facing native-DGP `.qmd`, `_pkgdown.yml`, and
  minimal example fixtures/assets.
- **Constitutional check:** Immutable versioned generators, truth/RNG review,
  registry status, and uppercase consecutive `X1...Xk` covariates remain
  explicit.
- **Test obligation:** Document generator/version immutability, sidecars, truth,
  validation, RNG, status, and review obligations; use `X1...Xk`; state
  prominently that CausalStress has no public runtime DGP-registration API; do
  not claim support for real-data DGPs or user-defined families.
- **Review gate:** Batch 3 scientific and user-documentation truthfulness review.
- **Disposition:** complete_after_review

### CS-1243 — Reconcile existing articles, roxygen, and reference truth

- **Batch:** 3
- **Source:** v0.2.1 documentation acceptance criteria; CS-1223 and CS-1229
  released truthfulness baselines
- **Motivation:** The new canonical articles cannot coexist with stale v0.1.x,
  return-shape, persistence, DGP-type, or parallelism claims elsewhere.
- **Files:** the four migrated `.qmd` articles, affected `R/*.R` roxygen,
  generated `man/*.Rd`, `README.qmd`/`README.md` only where truth corrections
  are required, and `_pkgdown.yml`.
- **Constitutional check:** Documentation-only contract repair; no signature,
  behavior, scientific status, or governed vocabulary change.
- **Test obligation:** Remove every known stale v0.1.x, `qs`, return-shape,
  target-support, DGP-type, parallelism, collector-authority, and runtime-DGP-
  registration claim; preserve CS-1229 heavy-tail/oracle wording; regenerate
  `.Rd`; verify every exported page and article against current code and the
  accepted specification. Do not use prose edits to deprecate or redesign the
  dual-mode campaign API.
- **Review gate:** Batch 3 scientific and user-documentation truthfulness review.
- **Disposition:** complete_after_review

## Batch 3 Implementation Evidence

- **CS-1240:** `canonical-workflow.qmd` is the ordinary exported-API path from
  suite/DGP/estimator discovery through single and grid execution, canonical
  typed score collection, target-specific ATT/ATE/QST interpretation, explicit
  mixed-request CATE staging, summaries/plotting, identities/provenance, and
  strict RDS-backed persistence/resume. It teaches single/seeds/grid as the
  ordinary surface and keeps planned batching advanced without deprecating or
  redesigning either `cs_run_campaign()` mode. QST truth interpretation runs
  without optional estimators; the optional QST estimator example has a visible
  installed-package plus environment opt-in guard.
- **CS-1241:** `estimator-extension.qmd` executes a deliberately simple
  process-local registered estimator through public APIs and typed output. It
  documents identity/version lifetime, exact target capabilities and QST
  coordinates, supported legacy shape, dependency declarations, estimator-
  owned CI evidence, Airlock grants, and classed boundary failures. Its core
  path needs no optional estimator package.
- **CS-1242:** `native-dgp-contribution.qmd` defines a maintainer-reviewed
  package-development contribution path: immutable `(dgp_id, version)`, the
  full synthetic schema and uppercase consecutive `X1...Xk`, governed RNG,
  truth/oracle uncertainty, matching YAML/QMD sidecars, registry lifecycle,
  validation, and review. It prominently states that no public runtime DGP-
  registration API, real-data DGP support, parameterized families, or user-
  defined families exist. The remaining dossier template was migrated from
  Rmd to executable `inst/templates/dgp_dossier_v1.qmd`.
- **CS-1243:** The four migrated articles now use current typed collectors,
  target-specific gatekeeper fields, strict schema-4 resume semantics, RDS
  staging, explicit experimental status, truthful optional-example guards, and
  moment-regime limits. The heavytail DGP is no longer put in a mean-based ATT
  scorecard. README source/output now identify the v0.2.1 documentation surface
  and link the three new guides. Affected roxygen and generated Rd now describe
  actual campaign return modes, estimator descriptor/return shapes, registry
  startup validation, tidy columns, and single-writer gathering. No runtime
  implementation, export, signature, DGP, truth, RNG, status, or governed
  vocabulary changed.
- **Source/index validation:** `tools/ci-docs.R` now requires the seven named
  package articles, the Quarto dossier template, and 12 Quarto DGP dossiers;
  rejects current Rmd sources and known stale public claims across README,
  articles, and generated Rd; requires every package article to be indexed
  exactly once; preserves the CS-1229 protected prose; and locks the new guide
  boundary statements. It passed with Quarto CLI 1.9.38, quarto R 1.5.1, and
  pkgdown 2.2.1.
- **Executable documentation:** All seven articles rendered from an isolated
  installed CausalStress 0.2.0 package without optional-document opt-in. The
  migrated dossier template also rendered with sample parameters. A focused
  installed-GenGC QST run passed for three tau coordinates, confirming the
  guarded optional example's current adapter path. The complete pkgdown site
  built every exported reference page and all seven articles successfully.
- **Package/runtime gates:** The full test suite passed 1,244 expectations with
  zero failures and 56 existing governed/optional-estimator warnings. Strict
  registry structure and executable validation passed 24/24 rows. Governed
  lint passed with `lint_count=0` and 34 recognized internal-helper false
  positives. A full-vignette Windows `R CMD build` plus
  `R CMD check --no-manual` completed in 342.6 seconds with 0 errors, 0 warnings,
  and 0 notes.
- **Build-substrate chronology:** The first full-vignette build attempt let a
  user-library CausalStress 0.1.10 shadow the package under test inside Quarto
  child sessions, producing missing-current-export errors. No source correction
  was made for that harness failure. Reordering the isolated current-source
  CausalStress 0.2.0 library first produced the final 0/0/0 build/check above.
- **Scope and state:** The unrelated concurrent maintainer edits in
  `inst/design/horizon.md` remain untouched and outside this batch. Batch 4,
  push, merge, tag, site publication, and release remain unauthorized.

Independent review returned **APPROVE WITH NON-BLOCKING NOTES**. The reviewer
independently confirmed the Batch 3 scope, all-comment `R/` diff, unchanged
signatures/NAMESPACE/DESCRIPTION, the scientific and extension boundaries in
the three new guides, reconciliation of the four existing articles and README,
the Quarto-only/indexing checks, all five protected CS-1229 passages, clean
generated Rd usage, and the validity of the final 0/0/0 build after correcting
the local stale-library search order. The remaining notes are advisory: remote
CI is inherently owed after a later authorized commit/push sequence, and the
release playbook may later document placing the under-test library before user
libraries for local full-vignette builds. Neither note requires a Batch 3
correction or executable rerun.

CS-1240 through CS-1243 are `complete_after_review`. This acceptance authorizes
the Batch 3 commit only, excluding `inst/design/horizon.md`. It does not
authorize Batch 4, push, merge, tag, site publication, or release.

## Batch 4 — Integrated Documentation Validation

### CS-1244 — Validate the complete Quarto/pkgdown publication artifact

- **Batch:** 4
- **Source:** v0.2.1 documentation/report acceptance criteria; independent
  review N1–N3
- **Motivation:** Individually rendered pages are not evidence that the combined
  site is complete, linked, truthful, reproducible, and clean.
- **Files:** documentation validation tests/scripts, `_pkgdown.yml`, CI workflow
  adjustments, and generated-but-untracked preview artifacts only.
- **Constitutional check:** Documentation truthfulness across Articles I–VII;
  no scientific or runtime authority is created by site assembly.
- **Test obligation:** Build from a clean installed package with the pinned
  Quarto toolchain; require QMD for all governed narrative sources; render and
  index exactly 12 registry-keyed reports; fail missing/extra/duplicate sources;
  execute core examples without optional estimators; visibly guard optional
  examples; verify all configured internal links and no unindexed committed
  articles; enforce typed no-cross-scoring and heavy-tail interpretation; prove
  `README.md` is current; and finish with a clean source worktree. Run focused,
  full, strict-registry, lint, coverage, and documentation gates required by the
  final diff before independent documentation and behavior-preservation review.
- **Review gate:** Batch 4 integrated-site and documentation-truthfulness review.
- **Disposition:** complete_after_review

Implementation evidence (2026-07-27):

- The site workflow now installs the source revision once into an isolated
  library, puts that library first for every executable-documentation child
  process, renders dossiers with an explicit installed-package mode, and fails
  on both tracked changes and untracked source artifacts. The coverage/lint
  workflow now runs on the active `v0.2.1` branch.
- `tools/ci-docs.R` rejects missing, extra, or duplicate DGP navigation entries
  in addition to its Quarto-only article, source, optional-example, typed-
  target, and CS-1229 truthfulness checks. `tools/ci-site.R` verifies that the
  resolved package comes from the declared clean library, matches the source
  version, contains exactly 12 registry-keyed reports indexed exactly once,
  and has no broken local page or fragment links.
- The first assembled-site crawl correctly failed on four README links that
  were valid on GitHub but invalid after pkgdown assembly. That finding was
  routed narrowly to the CS-1243-owned README source: the governance link now
  targets the repository and the three guide links target their published
  article pages. `README.md` was regenerated from `README.qmd`; no prose,
  behavior, API, target, DGP, truth, RNG, status, or identity changed.
- With Quarto CLI 1.9.38, quarto R 1.5.1, and pkgdown 2.2.1, README execution
  passed and reproduced the committed Markdown; pkgdown checks and the final
  seven-article site build passed; all 12 dossiers rendered from the isolated
  installed package; and the integrated crawl passed 121 HTML pages with zero
  broken internal links. The final source status contained only this Batch 4
  review diff plus a separately identified concurrent maintainer edit in
  `inst/design/horizon.md`; generated preview files added no source-tree delta.
- Strict validation passed 24/24 registry rows plus 153 focused expectations.
  Governed lint reported 0 findings and 34 recognized internal-helper false
  positives. The full suite passed with 0 failures, 56 expected warnings, and
  0 skips in 169.6 seconds. Coverage completed at 82.53% over 3,172 entries.
- An initial README/full-suite attempt inside the managed filesystem sandbox
  failed only at temporary `pins` storage with `EPERM`; both passed on the
  ordinary Windows filesystem substrate. A redundant nested pkgdown reinstall
  also produced an opaque Quarto subprocess failure; the final CI path removes
  that duplicate install and the exact single-install build command passed.

Independent review returned **APPROVE WITH NON-BLOCKING NOTES**. The reviewer
confirmed the isolated installed-package boundary, layered exactly-12 DGP
proof, recursive Quarto-source and index validation, internal page/fragment
link coverage, narrow CS-1243 ownership of the four README link corrections,
behavior preservation, and the recorded executable evidence. The two advisory
notes require no correction: asset references remain pkgdown's responsibility
rather than part of this anchor-navigation checker, and non-canonical absolute
self-links could evade the exact configured-URL rewrite even though pkgdown and
the corrected README use the canonical form.

CS-1244 is `complete_after_review`. This acceptance authorizes the Batch 4
commit only, excluding the concurrent maintainer edit in
`inst/design/horizon.md`. It does not authorize Batch 5, push, merge, tag, site
publication, or release.

## Batch 5 — Release Gate and Publication

### CS-1245 — Run the v0.2.1 release gate and publish in playbook order

- **Batch:** 5
- **Source:** `inst/design/release_gate.md`,
  `inst/design/release_ci_playbook.md`, and accepted v0.2.1 specification
- **Motivation:** The release must close with reproducible package, vignette,
  site, and CI evidence; no local preview or tag alone authorizes publication.
- **Files:** `DESCRIPTION`, `NEWS.md`, `.github/workflows/pkgdown-site.yaml`,
  `.github/workflows/R-CMD-check.yaml`,
  `.github/workflows/test-validation-substrate.yaml`,
  `inst/design/README.md`, `inst/design/roadmap.md`,
  `inst/design/release_ci_playbook.md`, packet ticket status files,
  `inst/design/causalstress_v0_2_1_spec_packet/release_closeout.md`, and only
  release-gate fixes routed back to their owning ticket.
- **Constitutional check:** Read `inst/design/release_gate.md` before gate work;
  no known constitutional violation may remain open or be deferred without an
  amendment.
- **Test obligation:** Run or explicitly defer-with-rationale every release
  gate and record exact commands, versions, substrate, paths, results, failures,
  reruns, URLs, and commit SHAs. Include full-vignette `R CMD build/check` with
  pinned Quarto on Windows, WSL, R-release, and R-devel; full tests; strict DGP
  validation; lint; coverage; acceptance criteria; clean-tree and audit routing;
  confirm the playbook no longer presents a vignette-skipping command as release
  evidence; then obtain green branch, main, and tag CI in the playbook order.
  Update version/date/NEWS and governance state. Publish the GitHub Release and
  Pages site only after their preceding gates and explicit maintainer
  authorization. Pages deployment must be manual, tag-only, version-matched,
  environment-scoped, and later than green tag CI; all automatic site builds
  remain non-publishing previews.
- **Review gate:** Final release-gate review before merge/tag/publication, then
  maintainer release decision.
- **Disposition:** open

**Maintainer decision (2026-07-27):** Publish the v0.2.1 pkgdown site through
GitHub Pages. This resolves the publication VALUE decision already reserved to
CS-1245; it is not a constitutional amendment or authority to bypass the
release order. The deployment mechanism passed independent review before any
push. Actual deployment remains gated on a clean release tree, green
branch/main/tag CI, and final maintainer acceptance. GitHub authentication,
the GitHub Actions Pages source, and the selected-tag `v*` environment rule
were configured and verified afterward as recorded below.

Pages-mechanism implementation evidence (2026-07-27):

- Current official GitHub guidance was checked before implementation. The
  workflow uses `actions/upload-pages-artifact@v4`,
  `actions/configure-pages@v5`, and `actions/deploy-pages@v4`; only the deploy
  job receives `pages: write` and `id-token: write`, and it targets the
  `github-pages` environment with serialized deployments.
- Pushes, pull requests, main/default-branch builds, tag pushes, and manual
  runs with `deploy_pages=false` still produce preview artifacts only. A Pages
  artifact and deployment job exist only for an explicit manual
  `deploy_pages=true` dispatch.
- The build fails closed unless the dispatch ref is a semantic `vX.Y.Z` tag
  whose value exactly equals `v` plus the `DESCRIPTION` version. The current
  pre-release `DESCRIPTION` remains 0.2.0, so a premature v0.2.1 deployment
  cannot pass before the ordinary CS-1245 version update.
- The deployment depends on both the complete site build and the runtime-
  without-documentation-tooling smoke job. The release playbook now records
  the manual post-tag-CI dispatch and rehearses documentation on Windows and
  WSL using the same isolated installed-package boundary accepted in Batch 4.
- Workflow and ticket YAML parse, the permission/environment/action-version
  assertions pass, `git diff --check` is clean, and the production/package diff
  is empty. `actionlint` is not installed locally; workflow semantics remain an
  explicit independent-review and later remote-CI obligation.
- At implementation and independent-review time, GitHub CLI 2.91.0 was present
  but its `blechturm` credential was invalid. No remote mutation occurred before
  review.

Independent review returned **APPROVE WITH NON-BLOCKING NOTES** on 2026-07-27.
It confirmed the authority boundary, fail-closed event matrix, version guard,
artifact handoff, job dependencies, scoped permissions, action versions,
environment, concurrency, playbook order, and truthful open-ticket state. The
review made two load-bearing operational controls explicit: ordinary tag CI
greenness remains a playbook prerequisite rather than a sibling-workflow query,
and the remote `github-pages` selected-tag rule remains mandatory before the
first deployment. Neither note requires a code correction.

The reviewed Pages mechanism may be committed. CS-1245 remains `open` because
the final release gate, branch/main/tag CI, maintainer release decision,
GitHub Release, and Pages deployment have not occurred. The review authorizes
none of those actions.

Remote Pages-configuration evidence (2026-07-27):

- The maintainer reauthenticated GitHub CLI as `blechturm` through the secure
  keyring with `repo` and `workflow` scope; no credential value is recorded.
- The public `blechturm/CausalStress` repository now has a GitHub Pages site
  with `build_type: workflow`, enforced HTTPS, and the expected URL
  `https://blechturm.github.io/CausalStress/`.
- The `github-pages` environment uses custom deployment policies rather than
  protected-branch selection. Its sole policy is the tag pattern `v*`. GitHub
  initially generated an additional `main` branch policy during setup; that
  exact policy was detected during read-back verification and removed.
- API read-back verified the workflow build type and the single tag policy.
  No workflow was dispatched and no merge, tag, release, or Pages deployment
  occurred during configuration.

Initial branch-CI correction routing (2026-07-27):

- The first `v0.2.1` branch push at `e42d216` produced green coverage/lint and
  pkgdown-preview workflows. In R-CMD-check run
  `https://github.com/blechturm/CausalStress/actions/runs/30292862144`, Ubuntu R
  release and R-devel passed, while Windows and macOS stopped before package
  check in `setup-r-dependencies`.
- Both failed jobs reported the same cause: the action's automatic Pandoc
  detection called `pak::pkg_deps()` over all dependency types and therefore
  tried to resolve optional non-CRAN Suggests package `GenGC`. The existing
  `dependencies: "hard"` boundary had already installed the required check and
  documentation packages successfully. The correction sets
  `install-pandoc: false`; the separately pinned Quarto CLI remains installed
  and `tools/ci-docs.R` remains the executable documentation-substrate gate.
- The same push exposed that `test-validation-substrate.yaml` still named
  `v0.2.0` but not `v0.2.1`, so the required test/validation/substrate branch
  workflow did not run. The correction adds only the active release branch to
  that existing trigger list.
- These are narrow release-evidence corrections owned by CS-1245: one prevents
  an irrelevant optional-Suggests scan before cross-platform checks, and one
  restores a required branch gate. They do not alter package runtime,
  scientific behavior, dependency declarations, test semantics, or release
  authorization. Both require independent review and fresh branch CI before
  they may count as evidence.

Independent correction review returned **APPROVE WITH NON-BLOCKING NOTES** on
2026-07-27. It independently confirmed the platform-specific `GenGC` failure
diagnosis, the two-line executable scope, the independence of the Pandoc and
Quarto inputs, unchanged full-vignette check semantics, the restored substrate
trigger, CS-1245 ownership, YAML/Markdown agreement, and the truthful open
closeout. The reviewer authorized only committing and pushing the correction
for fresh branch CI. It did not authorize the final release gate, merge, tag,
GitHub Release, or Pages deployment. Its lineage note correctly observed that
the correction sits after horizon-only commit `9586841`; fresh CI must be bound
to the new pushed head rather than the failed `e42d216` preflight.

**Second branch-CI correction (2026-07-27):** Fresh CI at `991185f` closed the
automatic-Pandoc failure and passed coverage/lint, test/validation/substrate,
the non-deploying site preview, Ubuntu R release check, and macOS R release
check. Windows then exposed a separate full-vignette build boundary: Quarto's
child R process could not see the package staged by the parent `R CMD build`.
CS-1245 therefore prepends a temporary installation of the exact checked-out
revision to `R_LIBS` on Windows only before `rcmdcheck`. This preserves
full vignette execution and all check strictness. The maintainer waived another
Claude review. Targeted Windows validation confirmed that an isolated child R
session loads the current package from that temporary library; fresh complete
branch CI remains mandatory.

**Final-candidate branch correction (2026-07-27):** At candidate evidence head
`61c9015a`, all R-CMD-check matrix jobs, test/validation/substrate, coverage,
lint, and the complete pkgdown site-build job passed. The
`runtime-without-documentation-tooling` negative control alone failed because
`setup-r-dependencies` restored a broad fallback cache containing `quarto` and
`pkgdown` even though dependency selection remained `"hard"` and automatic
Pandoc/Quarto installation was disabled. The absence assertion therefore
caught a contaminated fixture, not a runtime dependency. CS-1245 disables the
package cache for that job only. The site-build cache remains enabled; hard
dependencies are still resolved and installed; and no runtime, scientific, or
dependency-declaration behavior changes. Fresh complete branch CI is required
before merge.

## Release-Gate Requirement

CS-1245 is the required final release-gate ticket. It names
`inst/design/release_gate.md`, requires reading it before gate work, requires
every check to be run or explicitly deferred with rationale, and records all
evidence in `release_closeout.md`.
