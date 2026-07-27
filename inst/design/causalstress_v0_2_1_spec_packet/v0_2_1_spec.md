# CausalStress v0.2.1 Quarto Documentation and Bounded Maintenance Spec

**Status:** FINAL — released and closed 2026-07-27
**Packet:** `causalstress_v0_2_1_spec_packet`
**Date opened:** 2026-07-26
**Branch:** `v0.2.1`
**Branch baseline:** `bdbe224839c69f8b326c7fd6df91bcee7cca7206`
**Target package version:** `0.2.1`
**Constitutional baseline:** `CAUSAL_STRESS_CONSTITUTION.md` v2.0.1
**Accepted:** 2026-07-26
**Independent review:** APPROVE WITH NON-BLOCKING NOTES; N1–N3 are mandatory
ticket-derivation constraints.
**Authority:** Historical final release specification. Implementation was gated on a
derived ticket and batch plan receiving independent review; work then stayed
within both this specification and the reviewed ticket boundary.
**Tickets:** Intentionally absent at acceptance. Tickets and a batch plan are
derived from this committed specification in a separate reviewed change.

## Objective

Publish the complete user-facing documentation surface promised after v0.2.0
and remove a bounded set of independently-audited accidental complexity without
changing observable package behavior, scientific semantics, public signatures,
or artifact identity.

The release has four concrete outcomes:

1. A governed, Quarto-authored pkgdown site with a reproducible report for each
   of the 12 registered DGP IDs, a canonical typed-scoring workflow, and
   truthful extension documentation.
2. A characterization-test net around the duplicated per-seed runner paths
   identified as Finding F1 in `audit/code-simplicity-audit.md`, with no F1
   production refactor in this release.
3. An independently reviewed disposition for each low-risk simplicity finding
   F2, F4, F5, F6, and F7: implement it with its behavior lock, or explicitly
   defer it to v0.2.2 without broadening the documentation release.
4. A clean patch-release gate demonstrating that documentation publication and
   representation-only maintenance preserve the v0.2.0 scientific, identity,
   RNG, persistence, and public-API contracts.

v0.2.1 is not a scientific capability release. It documents the instrument that
v0.2.0 shipped and makes narrowly scoped internal simplifications before the
families-versus-CATE planning gate.

## Sources and Authority

This specification consumes:

- `inst/design/roadmap.md`, which assigns the pkgdown site, all DGP reports, the
  canonical workflow, and extension-contract documentation to a separately
  versioned documentation release;
- `inst/design/audit/code-simplicity-audit.md`, especially F1–F7, the retained
  structures in §5, and the Pass-2 shortlist in §6;
- `inst/design/audit/pass2/maintainer-adjudication.md`, which governs the
  heavy-tail estimand-boundary interpretation and the distinction between
  atomic execution and invalid aggregate metrics;
- the final v0.2.0 Wave 1, correction, and CI packets as historical evidence of
  the released surface; and
- `inst/design/release_gate.md` and `inst/design/release_ci_playbook.md` for
  release evidence and publication sequencing.

No RFC is required for the scoped work because it adds no estimand, DGP family,
truth algorithm, fingerprint/schema member, constitutional amendment, or public
API. If implementation discovers that an acceptance criterion requires such a
change, work stops and the issue is routed through the RFC cycle or an amended
specification before proceeding.

## Scope

### Governance and release surfaces

- Reconcile `roadmap.md` so v0.2.0 is historical and v0.2.1 is the current
  documentation/bounded-maintenance release after this packet is activated.
- Update package version, date, `NEWS.md`, site URL, issue URL, and release-facing
  metadata only after activation.
- Add pkgdown and Quarto only as documentation/build tooling, never as runtime
  imports. The documentation gate must check that the declared Quarto CLI is
  available instead of relying on an IDE-bundled executable or an undeclared
  `PATH` entry.
- Add branch/main/tag documentation-build evidence to the governed release
  sequence. Preview builds do not authorize publication.

### F1 characterization tests only

Add tests that characterize the supported behavior of the currently separate
per-seed execution paths before any later unification. The test matrix covers:

- serial and experimental-parallel execution;
- direct board publication and worker staging followed by gather/consolidation;
- custom `tau` and ordinary `...` forwarding;
- thread-cap and parallel-backend provenance;
- warning counts/classes and progress behavior, including cached work;
- deterministic output ordering;
- `skip_existing`, `force`, current-schema resume, schema-migration rejection,
  configuration-fingerprint mismatch, and bootstrap-CI resume rejection; and
- grid execution and planned-batch execution as distinct contracts, without
  asserting that their return values or artifact grains are interchangeable.

The tests lock observable contracts: result completeness and correctness,
documented output order, resume/no-overwrite behavior, warning and error
classes, and score/fingerprint identities. They must not freeze incidental
implementation details such as exact progress text, internal helper call order,
or undocumented event timing. Progress coverage asserts only documented user-
visible semantics.

This work package changes tests and fixtures only. If a characterization test
exposes a defect, the defect is recorded and routed; production code is not
changed opportunistically under F1 authority.

### F2 — retire test-only historical fingerprint builders

- Remove the private production helpers
  `cs_build_config_fingerprint_schema2()` and
  `cs_build_config_fingerprint_legacy()`.
- Replace their use in rejection tests with explicit inert historical fixture
  strings that are unmistakably sentinels rather than plausible hashes. The
  value of a rejected historical fingerprint is not part of the current resume
  decision.
- Prove that missing and schema-1–3 resume artifacts still fail before
  fingerprint comparison with the exact governed migration error classes. At
  least one test must demonstrate that the sentinel is never consulted because
  schema rejection occurs first.
- Freeze representative current schema-4 fingerprint values before the change
  and prove that the simplification moves none of them.
- Preserve legacy artifact discovery and historical interpretation. This scope
  removes obsolete writers, not supported readers or forensic boundaries.

### F4 — remove the obsolete thread setter

- Confirm that no supported package path and no maintainer-declared unpublished
  script calls the private `cs_enforce_threads()` helper through `:::`.
- Remove `cs_enforce_threads()`.
- Retain `cs_thread_caps_env()` plus scoped environment restoration as the sole
  thread-cap mechanism.
- Prove thread-cap application and caller-environment restoration with focused
  tests.

### F5 — remove the unused estimator-result forwarding adapter

- Confirm that no supported package path and no maintainer-declared unpublished
  script calls the private `cs_extract_estimator_result()` helper through `:::`.
- Move any unique legacy-output fixture cases to the tests for
  `cs_normalize_estimator_outputs()`.
- Remove the adapter and its adapter-only tests without weakening the legacy
  `list(att, qst, meta)` normalization contract.
- Do not replace it with another alias or one-use wrapper.

### F6 — collapse the duplicated `cs_run_grid()` tau branch

- Characterize omitted, explicit `NULL`, canonical default, and custom `tau`
  behavior before editing the wrapper.
- Resolve one effective tau value and make one explicit `cs_run_seeds()` call.
- Preserve the exported signature, default, validation, warnings, output order,
  score coordinates, and all fit/configuration/score fingerprints.
- If omitted/`NULL` and explicit-default behavior are not observationally
  equivalent, stop and retain the branch pending a separately reviewed design.

### F7 — make governed vocabularies single-source

- Make the estimand descriptor collection the single source of valid estimand
  target IDs.
- Add an invariant that descriptor names equal embedded
  `estimand_target_id` values.
- Keep the non-comparable reason vocabulary adjacent to its validator, either as
  a direct literal or one clearly named internal constant. Do not create a
  generic vocabulary framework.
- Preserve every exact estimand ID, target descriptor field, non-comparable
  reason, error class, and public error message.
- Treat any vocabulary-member change as outside this representation-only scope.

### Documentation site and DGP reports

- Add `_pkgdown.yml`, a governed reference index, article navigation, site
  metadata, and a GitHub Actions site-build workflow.
- Adopt a Quarto-only policy for authored long-form site sources:
  - migrate `README.Rmd` to `README.qmd`, with Quarto rendering the committed
    GFM `README.md` used by GitHub and the pkgdown homepage;
  - migrate all four current `vignettes/*.Rmd` sources to `.qmd` and author all
    new site articles as `.qmd`, using valid Quarto-vignette front matter and
    the `quarto::html` vignette engine;
  - migrate all 12 `inst/dgp_meta/<dgp_id>.Rmd` model sidecars to `.qmd`; and
  - reject new current-site `.Rmd` sources in CI.
- Update `DESCRIPTION`, `.Rbuildignore`, and article front matter for the Quarto
  toolchain. `quarto` must be a declared documentation dependency and vignette
  builder; retain `knitr`, `rmarkdown`, or other tooling only where the final
  build demonstrably requires it. `R CMD build` and `R CMD check` must exercise
  the declared final arrangement successfully.
- “Quarto-only” governs authored long-form site inputs. It does not prohibit
  pkgdown's standard generated or structured inputs: roxygen-generated `.Rd`
  reference files, `NEWS.md`, `_pkgdown.yml`, the Quarto-generated `README.md`,
  governance Markdown, or generated HTML. These are not competing narrative
  authoring formats.
- Preserve `inst/dgp_meta/<dgp_id>.yml` and `.qmd` as the single authoritative
  metadata and narrative sources for DGP reports. Migration changes the
  authoring format, not the scientific content, DGP implementation, truth, RNG,
  registry identity, or status.
- Adapt the existing `dev/render_dossiers.R` boundary rather than introduce a
  second renderer or commit twelve copied narratives. The script remains a
  thin orchestration boundary but must invoke Quarto for `.qmd` inputs; it must
  not retain an R Markdown rendering fallback. The build must:
  - enumerate the installed DGP registry;
  - construct the exact expected `inst/dgp_meta/<dgp_id>.yml` and
    `inst/dgp_meta/<dgp_id>.qmd` paths for every registered DGP ID rather than
    treating every file in `inst/dgp_meta/` as a dossier;
  - exclude `schema.yml`, `*_cache/`, `*_files/`, and other generated artifacts
    from dossier enumeration, while failing on any additional top-level `.yml`
    or `.qmd` sidecar that does not correspond to a registered DGP ID;
  - require exactly one YAML sidecar and one Quarto dossier for every registered
    DGP ID;
  - fail on missing, duplicate, unregistered, or unrenderable dossiers rather
    than warn-and-skip;
  - supply the authoritative sidecar metadata and DGP ID as render parameters;
  - render all reports in clean processes with explicit seeds and a job-local
    cache; and
  - write only ignored/generated site output, leaving the source worktree clean.
- Integrate the generated dossier pages into the pkgdown navigation without
  treating them as independent scientific sources.
- Display installed ID, version, status, tags, and scientific intent accurately.
  The site must distinguish the 2 stable DGPs from the 10 experimental DGPs and
  must not call all DGPs verified or validated.
- Preserve the accepted `synth_heavytail` operating rule: execute ATT estimators
  to diagnose breakdown, do not present an ATT winner-ranking/bias/RMSE
  shootout, and use QST for valid distributional comparison.
- Treat DGP Registry 1.4.0 as a superseded historical design proposal in the
  site and current governance indexes. Do not silently edit or version-bump it;
  any successor registry specification is deferred to the parameterized-
  families packet that supplies the missing scientific design.

### Canonical and extension documentation

- Add a canonical-workflow Quarto article that uses only exported APIs and
  teaches:
  DGP/estimator discovery, `cs_run_single()`, `cs_run_seeds()` or
  `cs_run_grid()`, the canonical `cs_collect_scores()` surface, ATT/ATE/QST
  interpretation, explicit CATE staging, summaries/plots, provenance, and
  persistence/resume.
- Teach `cs_run_single()` and `cs_run_grid()` as the ordinary entry points.
  Document planned/batched campaigns separately as an advanced lifecycle; do
  not present the dual-mode `cs_run_campaign()` signature as the conceptual
  center of the package.
- Add a user-defined-estimator Quarto article covering registration lifetime,
  versioning, declared target capabilities, typed and legacy output shapes,
  optional dependencies, CI behavior, airlock restrictions, errors, and an
  executable minimal estimator.
- Add a contributor-facing native-DGP Quarto article covering immutable
  versioned generator functions, uppercase consecutive `X1...Xk`, sidecars,
  truth, validation, RNG, registry status, and review obligations. It must state
  prominently that CausalStress has no public runtime DGP-registration API.
- Migrate, audit, and update the four existing vignettes as Quarto articles for
  the v0.2.1 surface rather than leaving two competing workflow narratives.
- Correct roxygen and generated `.Rd` files where the documentation audit finds
  stale or incomplete public claims. Documentation work must not silently
  change a public signature or contract.

## Non-Scope

- F1 production-code unification. It remains a v0.3.0 foundation candidate, or
  a separately governed v0.2.2 correction only if WP-02 exposes an actual
  runner defect.
- F3 redesign or deprecation of the dual-mode `cs_run_campaign()` public API.
  The long-term canonical campaign lifecycle remains a maintainer VALUE and
  governance decision before families implementation.
- Public `cs_register_dgp()` or user-defined family registration.
- CATE fitting, prediction, held-out evaluation, metrics, persistence, or UX.
- Parameterized DGP families, moment-regime implementation, kill plots, or any
  decision that preempts the v0.3.0 families-versus-CATE planning gate.
- Real-data DGPs, semantic feature rosters, or external-truth protocols.
- Registry-vocabulary expansion, population QTE, gatekeeper recalibration, or a
  constitutional amendment.
- DGP generator, truth, oracle, seed, RNG, or version changes.
- Fingerprint algorithm/schema changes, artifact migration, persistence-format
  changes, legacy `.qs` recovery, or an evidence lake.
- Parallel-backend replacement or promotion of experimental parallelism.
- Broad comment cleanup, function-count reduction, generic helper extraction,
  or refactoring of structures explicitly retained by the simplicity audit.
- Python, ACIC adapters, multi-arm treatment, CRAN submission, or unrelated
  estimator/inference work.

## Design

### Release classification and work-package boundaries

v0.2.1 is a patch release because it adds documentation and removes private
representation-only machinery while preserving supported behavior. Work is
divided into reviewable units; ticket identifiers are assigned only after this
specification is accepted:

1. Governance reconciliation and F1 characterization tests.
2. F2/F4/F5/F6/F7 low-risk maintenance, with each finding independently
   reviewable and revertible.
3. Quarto migration, pkgdown infrastructure, and single-source DGP-report
   publication.
4. Canonical workflow, estimator extension, native-DGP contribution, existing
   article, and roxygen/reference documentation.
5. Documentation-truthfulness review, behavior-preservation review, and the
   full release gate.

The documentation program is the release objective. F2, F4, F5, F6, and F7 are
independently reviewable and may each be deferred to v0.2.2 only through an
explicit reviewed disposition; one maintenance item does not automatically
block the documentation release. An unexpected failure of the F2 current-
schema identity lock or the F7 vocabulary invariant is different: implementation
must stop and investigate the apparent governed-contract violation rather than
quietly defer it. Maintenance must never broaden into “clean up nearby code.” A
finding that cannot meet its behavior-preservation acceptance criteria is not
fixed by changing the criterion—it is removed from this release and routed.

### Public API posture

No exported function is added, removed, renamed, or given a new signature.
Documentation may clarify which existing public entry point is recommended for
which lifecycle, but it may not deprecate a mode or change return semantics.

The user-defined estimator surface already exists through
`cs_register_estimator()` and may be documented. The analogous DGP runtime
surface does not exist; the native-DGP article is therefore a contribution guide
for package development, not a user extension claim.

### Quarto source policy, documentation build, and publication

The site build has two source tracks and one publication artifact:

1. Quarto renders every authored long-form site source. This includes
   `README.qmd`, all ordinary articles, and all DGP dossiers. The committed
   `README.md` is generated from `README.qmd` and must be in sync.
2. pkgdown assembles the homepage, Quarto articles, generated `.Rd` reference,
   and `NEWS.md` into the site. The adapted dossier orchestrator builds all
   registered DGP reports directly from their authoritative YAML/QMD pairs into
   the generated site tree, using Quarto rather than R Markdown.
3. The combined site is validated as one artifact: every configured internal
   link resolves, every registered DGP appears exactly once, and generated
   output remains untracked.

The current authored source tree may contain no `.Rmd` files at `README.Rmd`,
under `vignettes/`, or under `inst/dgp_meta/`. References to historical `.Rmd`
files in archived governance or audit evidence are not current site inputs and
must not be rewritten merely to satisfy this source-format rule. The build must
declare and verify compatible pkgdown, Quarto R-package, and Quarto CLI versions.
Neither Quarto nor pkgdown becomes a runtime import.

The ordinary `.qmd` articles use Quarto-vignette HTML (`quarto::html`) so
pkgdown can apply the site chrome. The DGP `.qmd` sidecars remain outside the
ordinary vignette directory and are materialized by the single dossier
orchestrator. `README.qmd` renders to GitHub-flavoured Markdown. These three
destinations share Quarto as their authoring/rendering system without pretending
that their output formats or installation roles are identical.

Branch and pull-request CI build the complete site and retain it as review
evidence but do not publish it. Main and tag sequencing follows
`release_ci_playbook.md`; external Pages publication occurs only at the
maintainer-authorized publication stage and must identify the package version
and source commit used to build it.

Site examples must execute in fresh R processes against the package revision
under review. Core workflows must not require optional estimators. Optional
sections use explicit availability checks and state when they were not run. DGP
report builds redirect oracle caches and other generated state to job-local
temporary locations and may not mutate committed evidence or the user's cache.

### Simplicity constraints

The audit's §5 retained structures are not candidates for this release:

- immutable versioned DGP functions and registry-dispatched generators;
- RNG capture/restoration boundaries;
- distinct fit, score-record, and score-row fingerprint operations;
- the atomic RDS boundary and caller-specific validators;
- registration-time plus execution-time oracle-grant checks; and
- explicit current-versus-legacy pin-name handling.

Low textual call count alone is not authority to remove a boundary. Removal is
permitted only for F2, F4, F5, F6, and F7 under the exact evidence and tests in
this specification.

## Constitutional Compliance

- **Article I:** ATT, ATE, QST, CATE descriptors and all governed vocabulary
  members remain byte-for-byte semantically unchanged. F7 changes only their
  internal source representation.
- **Article II:** no DGP or oracle algorithm changes. Documentation/report builds
  use explicit seeds, governed RNG behavior, and job-local caches.
- **Article III:** estimator-extension documentation demonstrates the existing
  airlock and never grants ordinary estimators truth columns.
- **Article IV:** gatekeeper policy and registry consequences are documented as
  released and are not recalibrated.
- **Article V:** F2 proves current schema-4 identity values do not move; F6
  proves the same run/score identities under equivalent tau inputs.
- **Article VI:** RDS persistence, staging, resume, and worker isolation are
  characterized and documented but not redesigned.
- **Article VII:** no released DGP implementation or version changes. DGP pages
  reproduce declared signatures and statuses without promoting experimental
  rows or rewriting immutable scientific history.

No constitutional amendment is needed. Any observed change to a governed ID,
reason, fingerprint, DGP output, truth value, RNG result, or public contract is a
release blocker and evidence that the change exceeded this specification.

## Acceptance Criteria

### Governance

- The spec receives independent review and every finding is accepted, rejected
  with rationale, or incorporated before the packet becomes `ACTIVE`.
- Tickets and a batch plan are created only after spec acceptance and agree with
  this scope; no implementation commit predates activation.
- `roadmap.md`, `README.md`, packet status, package version/date, and `NEWS.md`
  agree before release closeout.

### F1 characterization

- The test matrix covers every boundary listed under “F1 characterization tests
  only” on serial and applicable experimental-parallel paths.
- Planned-batch and grid execution are characterized separately.
- The tests assert observable supported behavior and documented ordering; they
  do not lock exact progress wording, internal helper call order, or other
  undocumented implementation details.
- The F1 work-package production diff is empty.
- Any discovered defect is routed rather than silently repaired.

### F2/F4/F5/F6/F7 maintenance

These criteria apply to each maintenance item retained in v0.2.1. An item
deferred to v0.2.2 instead requires an explicit independent review disposition,
an updated scope table, and no partial implementation left in the release tree.

- Schema-missing and schemas 1–3 resume attempts retain their exact classed
  fail-closed behavior before fingerprint comparison, using an unmistakable
  non-hash sentinel, and representative schema-4 fingerprint values are
  unchanged.
- `cs_build_config_fingerprint_schema2()`,
  `cs_build_config_fingerprint_legacy()`, `cs_enforce_threads()`, and
  `cs_extract_estimator_result()` have no remaining package/test callers or
  definitions after their unique required tests are relocated.
- Scoped thread caps still apply and restore the caller environment.
- All supported legacy estimator-output shapes still normalize through
  `cs_normalize_estimator_outputs()`.
- Omitted, `NULL`, default, and custom tau cases retain their specified
  validation, results, row order, score coordinates, and fingerprints.
- Descriptor names equal embedded estimand IDs; the exact four IDs and every
  non-comparable reason, error class, and public message remain unchanged.
- No new generic framework, replacement alias, public export, or runtime
  dependency is introduced by the maintenance work.

### Documentation and reports

- A clean pkgdown build succeeds locally and in CI using declared, checked
  compatible versions of pkgdown, the Quarto R package, and the Quarto CLI.
- `README.qmd`, every ordinary article/vignette source, and all 12 model dossier
  sources are `.qmd`; no current authored `.Rmd` site source remains in the
  governed source locations.
- `DESCRIPTION`, `.Rbuildignore`, and all article front matter name the final
  Quarto build arrangement consistently; ordinary articles use the
  `quarto::html` vignette engine, and package build/check succeeds with it.
- `README.md` is reproducibly generated from `README.qmd` and is current on the
  final tree. Generated Markdown, `.Rd`, `NEWS.md`, `_pkgdown.yml`, governance
  Markdown, and generated HTML are not violations of the Quarto-only authoring
  rule.
- The site contains exactly one successfully rendered report for each of the 12
  registered DGP IDs, generated from the authoritative YAML/QMD pair.
- Report status/version/tags agree with the installed registry and sidecars;
  two DGPs are identified as stable and ten as experimental.
- For each registered ID, the build checks the exact expected `<id>.yml` and
  `<id>.qmd` paths. `schema.yml`, `*_cache/`, and `*_files/` are excluded from
  dossier enumeration; missing, extra, duplicate, or unrenderable DGP dossiers
  fail the documentation build.
- The complete site has no unresolved configured internal links or unindexed
  committed articles.
- Every dossier is rendered through Quarto; no `rmarkdown::render()` or R
  Markdown fallback remains in the current site-build path.
- Core examples and Quarto articles execute from a clean installed package without
  optional estimator packages.
- Optional examples are explicitly guarded and their skipped state is visible.
- The canonical workflow uses the typed score surface and never cross-scores
  ATT, ATE, QST, or staged CATE.
- Heavy-tail pages and examples obey the accepted estimand-boundary operating
  rule and do not report a conventional ATT shootout.
- The estimator-extension article demonstrates airlock-safe registration and a
  valid typed result using only public APIs.
- The native-DGP article is clearly contributor-facing, uses uppercase
  consecutive `X1...Xk`, and contains no claim that a public runtime DGP
  registration API exists.
- The four migrated pre-existing articles and all exported reference pages contain no
  known stale v0.1.x, `qs`, return-shape, target-support, DGP-type, parallelism,
  or collector-authority claims.
- Generated site/report/cache files leave the source worktree clean.

### Release gate

- Focused tests, the full test suite, strict registry validation, lint, coverage
  evidence, substrate evidence, and `R CMD check` pass on the final tree.
- Windows and WSL/local evidence plus remote branch, main, and tag CI are
  recorded under the release playbook.
- Documentation truthfulness and maintenance behavior preservation receive
  independent review as distinct objectives.
- External site publication, tag creation, and GitHub Release publication occur
  only after their preceding gates and explicit maintainer authorization.
- No known constitutional violation, unrouted audit finding, or dirty worktree
  remains at closeout.

## Audit Findings Consumed

Ticket identifiers are deliberately not assigned in the draft specification.

| Finding | Draft disposition | Planned work package |
| --- | --- | --- |
| Simplicity F1 — duplicated per-seed execution | Characterization tests only; production unification deferred | Governance/characterization |
| Simplicity F2 — test-only historical fingerprint builders | Remove with exact migration and schema-4 identity locks | Bounded maintenance |
| Simplicity F3 — dual-mode `cs_run_campaign()` | Defer implementation; document current lifecycles separately; governance decision before families | v0.3.0 planning or separately scoped packet |
| Simplicity F4 — obsolete thread setter | Remove after private-use confirmation | Bounded maintenance |
| Simplicity F5 — unused result adapter | Remove; preserve canonical normalization coverage | Bounded maintenance |
| Simplicity F6 — duplicated tau forwarding branch | Collapse only after equivalence characterization | Bounded maintenance |
| Simplicity F7 — one-use vocabulary getters | Single-source without changing vocabulary | Bounded maintenance |
| Scientific pass-2 — heavy-tail estimand boundary | Preserve accepted documentation and reporting rule | Documentation |
| Scientific pass-2 — moment-regime aggregation | Deferred; no families/aggregation implementation | v0.3.0 planning / RFC-3 |
| Scientific pass-2 — stale Registry 1.4.0 | Label historical/superseded; defer successor registry spec to parameterized families; do not edit 1.4.0 | Documentation / future families packet |

## Maintainer Decisions

The maintainer accepted the Quarto-only site-source policy and the two audit-
review dispositions below on 2026-07-26. They are binding ticket-derivation
constraints for v0.2.1.

| Decision | Disposition | Consequence |
| --- | --- | --- |
| DGP Registry 1.4.0 | Treat it as historical/superseded in current documentation and defer a successor registry specification to the parameterized-families packet. | v0.2.1 makes no throwaway registry version bump and does not edit historical 1.4.0. |
| F2/F4/F5/F6/F7 coupling | Keep the five findings independently reviewable and allow explicit reviewed deferral of an individual item to v0.2.2. | The documentation objective is not held hostage by unrelated low-risk maintenance; F2/F7 invariant failures still require investigation. |
| Site authoring format | Migrate the README source, all ordinary vignette/article sources, and all DGP model dossiers from R Markdown to Quarto; accept only `.qmd` for new long-form site sources. | The pkgdown site has one narrative authoring format. Standard generated `.Rd`, `NEWS.md`, `_pkgdown.yml`, rendered `README.md`, governance Markdown, and generated HTML remain valid inputs/artifacts. |
