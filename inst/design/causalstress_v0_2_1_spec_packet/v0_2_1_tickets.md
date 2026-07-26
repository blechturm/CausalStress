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
- **Disposition:** open

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
- **Disposition:** open

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
- **Disposition:** open

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
- **Disposition:** open

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
- **Disposition:** open

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
- **Disposition:** open

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
- **Disposition:** open

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
- **Disposition:** open

## Batch 5 — Release Gate and Publication

### CS-1245 — Run the v0.2.1 release gate and publish in playbook order

- **Batch:** 5
- **Source:** `inst/design/release_gate.md`,
  `inst/design/release_ci_playbook.md`, and accepted v0.2.1 specification
- **Motivation:** The release must close with reproducible package, vignette,
  site, and CI evidence; no local preview or tag alone authorizes publication.
- **Files:** `DESCRIPTION`, `NEWS.md`, `inst/design/README.md`,
  `inst/design/roadmap.md`, packet ticket status files,
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
  authorization.
- **Review gate:** Final release-gate review before merge/tag/publication, then
  maintainer release decision.
- **Disposition:** open

## Release-Gate Requirement

CS-1245 is the required final release-gate ticket. It names
`inst/design/release_gate.md`, requires reading it before gate work, requires
every check to be run or explicitly deferred with rationale, and records all
evidence in `release_closeout.md`.
