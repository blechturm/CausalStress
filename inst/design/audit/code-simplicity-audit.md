# CausalStress code simplicity audit

## 1. Executive summary

This Pass 1 audit examined the complete current worktree at `HEAD` `9c16cd2d6a8915aef808bb3c865b8224778fa227` on branch `v0.2.0`. The worktree was already dirty before this report was created: the three modified files were release-gate evidence in `inst/design/README.md` and the two active v0.2.0 closeouts. None is a production-code or test change, and none appears to be current Batch 1 implementation work. Consequently, this audit has **zero** findings marked `FIX BEFORE COMMIT`.

The mechanical baseline found 66 production R files with 7,905 nonblank, non-comment physical lines; 87 test R files with 5,417 such lines; and an approximate test-to-production LOC ratio of 0.69. There are 169 package-level named function definitions: 88 exported functions and 81 internal functions, an exported-to-internal ratio of 1.09. There are no registered S3 methods. These measurements are descriptive and are not themselves findings.

The repository is not broadly overengineered. Its accidental complexity is mild and concentrated in three patterns:

- parallel runner paths that independently implement the same per-seed execution and persistence sequence;
- internal helpers that are now dead or exist only to manufacture rejected test fixtures; and
- small duplicated representations or branches that make future changes easier to apply inconsistently.

Seven main findings met the evidence threshold. The three most valuable follow-ups are:

1. converge the grid/campaign per-seed execution paths after characterization tests;
2. retire the schema-2 and schema-missing fingerprint builders from production code, using minimal historical fixtures instead; and
3. decide whether the two incompatible modes of the public `cs_run_campaign()` API should remain combined.

This is a triage audit, not an implementation plan. It does not authorize refactoring, alter release scope, or relax scientific, identity, RNG, resume, or persistence contracts.

## 2. Method and limitations

### Repository state at audit start

| Item | Observed state |
| --- | --- |
| HEAD | `9c16cd2d6a8915aef808bb3c865b8224778fa227` |
| Branch | `v0.2.0` |
| Dirty before report | Yes |
| Files changed from HEAD before report | `inst/design/README.md`; `inst/design/causalstress_v0_2_0_ci_packet/release_closeout.md`; `inst/design/causalstress_v0_2_0_correction_packet/release_closeout.md` |
| Apparent Batch 1 changes | None. The three changes record post-CS-1228 release-gate evidence and contain no production or test diff. |

The governance review covered `inst/design/README.md`, the complete Constitution, `contracts.md`, both active packets identified by the index, the final v0.2.0 and v0.1.10 specifications, and the historical v0.1.9 batching specification. Historical documents were used only where legacy schemas, batching, pin names, or persisted artifacts were relevant.

### Mechanical baseline

Comments and blank lines were excluded from the primary LOC figures. A line was classified as comment-only when its trimmed text began with `#`; code followed by an inline comment remained a code line. Physical totals are included to make that convention auditable.

| Surface | Files | Physical lines | Blank lines | Comment-only lines | Nonblank, non-comment lines |
| --- | ---: | ---: | ---: | ---: | ---: |
| `R/**/*.R` | 66 | 10,260 | 800 | 1,555 | 7,905 |
| `tests/**/*.R` | 87 | 6,291 | 750 | 124 | 5,417 |

The approximate test-to-production ratio is `5,417 / 7,905 = 0.69`.

The function inventory used parsed top-level assignment expressions, rather than regex alone, so the infix definition ``%||%`` was included. It found 169 package-level named definitions. `NAMESPACE` contains 89 `export()` directives: 88 match package-level functions and one exports the data constant `cs_tau_oracle`. The remaining 81 package-level functions are internal, giving `88 / 81 = 1.09`. A separate structural scan found 35 named local function assignments, 14 named callback function arguments, and 263 function literals in total. Callback arguments were not counted as independently addressable package functions. No `S3method()` registrations were present.

Largest production files by physical and primary code LOC:

| File | Physical LOC | Nonblank, non-comment LOC | Package-level definitions |
| --- | ---: | ---: | ---: |
| `R/cs-runner.R` | 818 | 678 | 2 |
| `R/cs-contracts.R` | 785 | 694 | 21 |
| `R/cs-run-campaign.R` | 356 | 261 | 1 |
| `R/dgp-synth-nonlinear-heteroskedastic.R` | 341 | 219 | 5 |
| `R/cs-fingerprint.R` | 323 | 289 | 13 |
| `R/cs-dgp-registry.R` | 303 | 267 | 3 |
| `R/cs-estimator-registry.R` | 289 | 218 | 4 |
| `R/est-gengc-dr.R` | 285 | 240 | 1 |
| `R/est-gengc.R` | 279 | 242 | 1 |
| `R/est-grf-dr.R` | 273 | 210 | 3 |
| `R/dgp-synth-overlap-stressed.R` | 262 | 175 | 4 |
| `R/cs-staging.R` | 253 | 213 | 6 |

Files with the most package-level named definitions were `R/cs-contracts.R` (21), `R/cs-fingerprint.R` (13), `R/cs-scale-helpers.R` (8), `R/cs-staging.R` (6), `R/cs-oracle-truth.R` (6), `R/cs-accessors.R` (5), `R/cs-pins.R` (5), `R/cs-rng.R` (5), and `R/dgp-synth-nonlinear-heteroskedastic.R` (5). Neither file size nor definition count was treated as a defect.

### Inventory and review sequence

Files were enumerated with `rg --files R tests`; line classes were counted mechanically; top-level functions and short signatures were collected from parsed R expressions; exports were reconciled with `NAMESPACE`; and static production/test call counts were generated for every package-level name. Candidate generation also scanned local function assignments, forwarding wrappers, constant-returning helpers, large functions, equivalent branches, repeated record construction, and near-duplicate estimator and runner blocks.

Before classifying zero- or one-call candidates, the following dynamic-use patterns were searched across `R/`, `tests/`, `NAMESPACE`, and `DESCRIPTION`:

```text
get\s*\(|mget\s*\(|match\.fun\s*\(|do\.call\s*\(
UseMethod\s*\(|NextMethod\s*\(|getExportedValue|as\.formula\s*\(
generator|registry|register|callback
```

The DGP and estimator registries store generators as function objects and invoke them through descriptor fields. `cs_run_one_seed_internal()` is passed to `do.call()`. `.onLoad()` is invoked by R's namespace lifecycle. These were treated as dynamic uses, not dead code. No `UseMethod()`, `NextMethod()`, `match.fun()`, namespace-string lookup, or S3 registration was found.

Deep review proceeded from the pre-existing dirty diff to the central runner and contract files, then to mechanically flagged functions and their tests, and finally to relevant persistence, RNG, fingerprint, airlock, registry, estimator, and DGP boundaries. Near-duplicate review explicitly compared the seed runners, campaign modes, GenGC/GenGC-DR estimators, LM/IPW bootstrap shells, metadata projections, plan/run configuration merging, and versioned DGP bodies.

### Limitations

Static textual call counts cannot exclude unsupported external use through `CausalStress:::` or arbitrary reflection. No runtime call tracer or downstream reverse-dependency corpus was available. Public functions were therefore assumed externally used even when repository callers were sparse.

This was a structural audit, not a scientific-method review. All production files were inventoried and structurally scanned, but every optional estimator's mathematical body and every immutable versioned DGP body was not re-adjudicated line by line. Candidate estimator and DGP duplication was reviewed far enough to determine whether a structural recommendation was presently supported. No refactoring or characterization test was executed. The maintainer still needs to confirm whether any private internal helper is used by unpublished local scripts before it is removed.

During the completion check, unrelated untracked scientific-review artifacts appeared under `inst/design/audit/checks/`, `inst/design/audit/phase1-claims/`, and `inst/design/audit/phase0-triage.md`. Their own contents identify a separate Claude scientific-design review and record the same three-file dirty baseline plus the then-absence of this report. They were not created, edited, or removed by this audit. Therefore repository-global exclusivity cannot be asserted for the elapsed audit window; the only worktree change attributable to this audit is `inst/design/audit/code-simplicity-audit.md`.

## 3. Findings overview

| Priority | Construct | Location | Category | Disposition | Maintenance value | Behavioral risk | Evidence strength |
| -------: | --------- | -------- | -------- | ----------- | ----------------- | --------------- | ----------------- |
| 1 | Per-seed execution and resume machinery | `R/cs-run-one-seed.R`; `R/cs-runner.R`; `R/cs-run-campaign.R` | Near-duplicate logic / verbose control flow | `PROCEED AFTER CHARACTERIZATION TESTS` | High | Medium | Strong |
| 2 | Historical fingerprint fixture builders | `R/cs-fingerprint.R` | Excess machinery / test-only production code | `PROCEED TO PASS 2` | Medium | Low | Strong |
| 3 | Dual-mode `cs_run_campaign()` public API | `R/cs-run-campaign.R`; `R/cs-campaign-runner.R` | Excess machinery / incompatible lifecycle modes | `SEEK GOVERNANCE DECISION` | High | High | Moderate |
| 4 | `cs_enforce_threads(n_threads = 1L)` | `R/cs-scale-helpers.R` | Dead internal abstraction | `PROCEED TO PASS 2` | Medium | Low | Strong |
| 5 | `cs_extract_estimator_result(res)` | `R/cs-helpers.R` | Test-only forwarding adapter | `PROCEED TO PASS 2` | Medium | Low | Strong |
| 6 | Equivalent `tau` branches in `cs_run_grid()` | `R/cs-runner-grid.R` | Near-duplicate control flow | `PROCEED TO PASS 2` | Low | Low | Strong |
| 7 | One-use vocabulary getters | `R/cs-contracts.R` | Unnecessary extraction / redundant representation | `PROCEED TO PASS 2` | Medium | Low | Strong |

## 4. Detailed findings

### 1. One per-seed operation has parallel implementations

**Construct and location:** `cs_run_one_seed_internal(dgp_id, estimator_id, n, seed, ...)` in `R/cs-run-one-seed.R:1-140`; local `run_one_seed(s, p = NULL)` inside `cs_run_seeds()` in `R/cs-runner.R:721-774`; resume preparation in `cs_run_seeds()` at `R/cs-runner.R:630-719` and `cs_run_campaign()` at `R/cs-run-campaign.R:197-268`.

**Disposition and ratings:** `PROCEED AFTER CHARACTERIZATION TESTS`; maintenance value **High**; behavioral risk **Medium**; evidence strength **Strong**.

**Evidence and receipts:** The only production reference to `cs_run_one_seed_internal` is the function object passed as the first argument of `do.call()` by local `run_task()` in `cs_run_campaign()` (`R/cs-run-campaign.R:280-310`). There are no direct test calls and it is not exported or S3-registered. The dynamic call is real and was not misclassified as absence. Its only caller always supplies `skip_existing = FALSE`, making the internal helper's cache-hit branch at lines 31-77 unreachable through supported package code. The local `run_one_seed()` in `cs_run_seeds()` independently repeats worker-board selection, thread forcing, scoped environment caps, `cs_run_single()`, parallel provenance fields, staging-or-pin persistence, and row flattening. Resume fingerprint comparison and cache reads are also implemented separately by the two public orchestrators.

Reproducible searches:

```text
rg -n "cs_run_one_seed_internal|run_one_seed <- function|cs_run_single|cs_stage_result|cs_pin_write|cs_result_to_row" R tests NAMESPACE DESCRIPTION
rg -n "cs_run_campaign\(|cs_run_seeds\(" R tests
rg -n "get\s*\(|match\.fun\s*\(|do\.call\s*\(|UseMethod\s*\(|NextMethod\s*\(" R tests
```

The public paths have substantial tests, including `test-run-campaign.R`, `test-run-campaign-strategy-map.R`, `test-run-resume-bootstrap.R`, `test-runner-grid.R`, `test-resume.R`, `test-parallel-protocol.R`, and the v0.1.8/v0.1.9 parallel and batch suites. Those tests establish coverage of the public modes, not equivalence of the two internal execution implementations.

**Complexity cost:** A future change to thread caps, provenance, worker persistence, `...` forwarding, warning policy, or result flattening must be recognized and correctly repeated in two worker bodies. Cache/resume behavior is distributed across three places, while an unreachable generic cache branch makes `cs_run_one_seed_internal()` appear more reusable than it is. This raises drift risk at exactly the boundary that has recently changed for parallel staging and RDS persistence.

**Recommended direction:** Keep resume and deletion policy in the public orchestrators, where the unit of work and cached-result aggregation are known. Reduce the internal worker to one explicit execution boundary that owns only: effective config/thread caps, `cs_run_single()`, parallel provenance augmentation, staging-or-pin publication, progress notification, and row projection. Use that boundary from both grid-seed and campaign-grid execution. Do not merge planned-batch execution into it unless a separate review establishes identical error and artifact semantics.

**Risks and prerequisites:** Characterize serial and experimental-parallel execution; direct board writes versus staging; custom `tau`; `...` forwarding; DGP warning count; progress messages including cached seeds; thread-cap provenance; output ordering; force/skip behavior; bootstrap-CI resume rejection; and configuration-fingerprint mismatch classes. Preserve Constitution Article II RNG isolation, Article V section 5.1 concurrency, Article VI worker isolation/atomicity, and `contracts.md` sections “Batch Artifact Contract” and “RNG Contract.” The active v0.2.0 correction specification, “Minimum v0.2.0 correction,” lines 141-165, also fixes the current RDS persistence behavior and public signatures for this release. This is follow-up work, not current-release cleanup.

### 2. Production code builds fingerprints used only for rejected test fixtures

**Construct and location:** `cs_build_config_fingerprint_schema2(...)` at `R/cs-fingerprint.R:226-263` and `cs_build_config_fingerprint_legacy(...)` at `R/cs-fingerprint.R:287-323`.

**Disposition and ratings:** `PROCEED TO PASS 2`; maintenance value **Medium**; behavioral risk **Low**; evidence strength **Strong**.

**Evidence and receipts:** Each builder has zero production callers and exactly one test caller, both in `tests/testthat/test-v018-fingerprint-schema.R`. Neither is exported, S3-registered, stored in a registry, passed as a callback, or found in dynamic lookup searches. The wider search finds only their definitions and those two tests. The tests create schema-missing and schema-2 pins and assert that schema-4 resume fails. In `cs_run_seeds()`, `cs_assert_schema4_resume(stored_schema)` is called before the expected current fingerprint is constructed or compared (`R/cs-runner.R:669-676`). `cs_assert_schema4_resume()` rejects missing and pre-4 schemas at `R/cs-fingerprint.R:125-143`; therefore the historical fingerprint's computed value is immaterial to the behavior under test.

Reproducible searches:

```text
rg -n "cs_build_config_fingerprint_schema2|cs_build_config_fingerprint_legacy" R tests NAMESPACE DESCRIPTION inst/design
rg -n "config_fingerprint_schema|cs_assert_schema4_resume" R tests
rg -n "get\s*\(|match\.fun\s*\(|do\.call\s*\(" R
```

The legacy builder's comment—“must remain stable to keep v0.1.7 artifacts resumable in v0.1.8”—is historical and misleading in the current codebase, which deliberately refuses schemas 1-3 as resume targets.

**Complexity cost:** Approximately 75 lines of production fingerprint logic exist only to make two fixtures more realistic than the fail-closed test requires. Readers must decide whether these are supported migration writers, historical readers, resume comparators, or fixtures. They are none of those in current production paths.

**Recommended direction:** Remove the two production builders and give the rejected fixture metadata an explicit inert string such as a documented historical-fingerprint sentinel. Keep the tests focused on the actual contract: missing or pre-4 schema fails before fingerprint comparison. If exact legacy hash reproduction is desired for archival verification, move that responsibility to a separately governed migration/forensics tool or test fixture with a stated purpose rather than a callable production helper.

**Risks and prerequisites:** Confirm no unpublished local script uses these private helpers to manufacture historical evidence. Do not remove legacy pin discovery or historical artifact interpretation. `contracts.md`, “Fingerprints and Pins,” lines 141-177, describes v0.1.7 as migration/read support only and schemas 2-3 as historical; it does not require current production code to generate those hashes. Existing class/message assertions for `causalstress_schema_migration_error` must remain unchanged.

### 3. `cs_run_campaign()` is two public APIs behind one signature

**Construct and location:** exported `cs_run_campaign(plan = NULL, dgp_ids = NULL, ..., ...)` at `R/cs-run-campaign.R:72-356`, with its immediate plan-mode return at lines 100-111 and internal `cs_run_campaign_plan()` at `R/cs-campaign-runner.R:14-170`.

**Disposition and ratings:** `SEEK GOVERNANCE DECISION`; maintenance value **High**; behavioral risk **High**; evidence strength **Moderate**.

**Evidence and receipts:** `cs_run_campaign()` is exported in `NAMESPACE:26` and has many direct tests. Grid mode expands DGP/estimator/seed tasks, optionally writes individual results, and returns a result tibble. Plan mode ignores almost all grid arguments, requires staging, always enters the planned batching protocol, and invisibly returns executed batch IDs. The Rd source states both return contracts at `R/cs-run-campaign.R:50-51`. The branch occurs before grid validation and before most arguments are interpreted. Plan-mode tests are in `test-v019-runner.R` and grid-mode tests are in `test-run-campaign.R`, `test-run-campaign-strategy-map.R`, and `test-run-resume-bootstrap.R`.

Reproducible searches:

```text
rg -n "export\(cs_run_campaign\)|cs_run_campaign\(|cs_run_campaign_plan\(" NAMESPACE R tests inst/design
rg -n "@return|if \(!is.null\(plan\)\)|return\(cs_run_campaign_plan" R/cs-run-campaign.R
```

The v0.1.9 batching roadmap called for refactoring campaign execution around deterministic batches, so this may be a deliberate transition rather than accidental design. Conversely, the current active documentation still describes grid mode as the recommended heterogeneous-campaign entry point. Repository evidence cannot determine which mode is intended to be canonical long term; that is why evidence is Moderate rather than Strong.

**Complexity cost:** One public name has two input models, validation paths, persistence grains, parallel controls, resume semantics, and incompatible return types. Users and maintainers must know which arguments silently become irrelevant when `plan` is non-NULL. Tests and documentation must describe two largely unrelated lifecycles together.

**Recommended direction:** Decide on one canonical campaign lifecycle. A plausible direction is a clearly named public planned-batch runner, with `cs_run_campaign()` retaining grid behavior during a deprecation window; the reverse is also possible if planned batching is the intended primary API. Do not simply move the early branch into another private wrapper while retaining the same ambiguous public contract—that would add file navigation without removing a concept.

**Risks and prerequisites:** This is a public API and return-contract decision, not a private cleanup. It requires maintainer approval, a release specification, deprecation/migration policy, documentation updates, and tests for both old and new entry points. The active v0.2.0 correction specification lines 163-165 expressly keeps public signatures unchanged, so no split belongs in the present release. Historical and future design documents contain both call styles; downstream use must be inventoried before deciding.

### 4. An obsolete thread-setting helper remains callable internally

**Construct and location:** `cs_enforce_threads(n_threads = 1L)` at `R/cs-scale-helpers.R:51-63`.

**Disposition and ratings:** `PROCEED TO PASS 2`; maintenance value **Medium**; behavioral risk **Low**; evidence strength **Strong**.

**Evidence and receipts:** The exact-name search finds no production or test call. The only non-definition occurrences are historical prose in `archive/DESIGN_V0.1.9_BATCHING.md`, an old audit, and an “-style” reference in `horizon.md`. It is not exported, S3-registered, registered as a callback, or dynamically resolved. Current parallel execution instead uses `cs_thread_caps_env()` with scoped restoration through `cs_with_envvar()`.

Reproducible searches:

```text
rg -n "cs_enforce_threads" R tests NAMESPACE DESCRIPTION inst/design
rg -n "cs_thread_caps_env|cs_with_envvar|Sys\.setenv" R tests
rg -n "get\s*\(|match\.fun\s*\(|do\.call\s*\(" R
```

**Complexity cost:** The helper is a second thread-control model. Unlike the active scoped helper, it permanently mutates four process environment variables and returns their new values. Its presence suggests an available execution path that should not be used because it can leak state into the caller's session.

**Recommended direction:** Remove `cs_enforce_threads()` and keep the existing scoped `cs_with_envvar(cs_thread_caps_env(), expr)` boundary as the sole thread-cap mechanism. Historical specifications need not be rewritten; current forward-looking prose can describe scoped caps without naming the removed implementation.

**Risks and prerequisites:** Confirm no unpublished script calls this private helper through `:::`. Preserve the active scoped behavior and its tests. Constitution Article V section 5.1 governs effective thread limits, while Article II section 2.1 prohibits DGP external-state mutation; removal of an unused unscoped setter reduces rather than relaxes those risks.

### 5. A forwarding adapter is tested but not used by the package

**Construct and location:** `cs_extract_estimator_result(res)` at `R/cs-helpers.R:19-25`.

**Disposition and ratings:** `PROCEED TO PASS 2`; maintenance value **Medium**; behavioral risk **Low**; evidence strength **Strong**.

**Evidence and receipts:** The helper has zero production callers and three direct test calls, all in `tests/testthat/test-helpers-extraction.R`. It is not exported, S3-registered, registered, or dynamically looked up. Its body calls the canonical `cs_normalize_estimator_outputs(res)`, extracts `outputs$att$estimate`, returns `outputs$qst`, and introduces no independent validation or side-effect boundary. The only design-document mention outside the old audit is a historical patch-spec suggestion that named this as one possible implementation path.

Reproducible searches:

```text
rg -n "cs_extract_estimator_result" R tests NAMESPACE DESCRIPTION inst/design
rg -n "cs_normalize_estimator_outputs" R tests
rg -n "generator|registry|get\s*\(|match\.fun\s*\(|do\.call\s*\(" R tests
```

**Complexity cost:** A production concept and a dedicated test file exist for a compatibility projection no production path uses. The tests can give the false impression that package execution depends on this adapter, while all current normalization authority resides in `cs_normalize_estimator_outputs()`.

**Recommended direction:** Remove the adapter and its adapter-specific tests. Preserve or relocate any normalization cases that are not already covered into the canonical normalizer's tests. Do not replace it with another one-use alias.

**Risks and prerequisites:** Confirm no unpublished `:::` use. Constitution Article III section 3.1 and the v0.2.0 specification require the legacy `list(att, qst, meta)` shape to normalize into typed outputs, but they do not require this private, unused projection. Characterize the canonical normalizer for the three fixture shapes before deleting duplicate tests.

### 6. `cs_run_grid()` repeats an entire call to distinguish omitted `tau`

**Construct and location:** exported `cs_run_grid(..., tau = NULL, ...)` at `R/cs-runner-grid.R:27-118`, especially the equivalent branches at lines 69-114.

**Disposition and ratings:** `PROCEED TO PASS 2`; maintenance value **Low**; behavioral risk **Low**; evidence strength **Strong**.

**Evidence and receipts:** Both branches call `cs_run_seeds()` with the same 18 other arguments. The only difference is that the NULL branch omits `tau`, allowing the callee default `cs_tau_oracle`, while the other supplies `tau`. `cs_run_grid()` is exported; `cs_run_suite()` is its only direct production caller; and calls occur in `test-runner-grid.R`, `test-collect.R`, `test-airlock.R`, and two DGP integration tests. No current grid test explicitly contrasts NULL/default and custom `tau` behavior.

Reproducible searches:

```text
rg -n "cs_run_grid\(" R tests NAMESPACE
rg -n "if \(is.null\(tau\)\)|cs_run_seeds\(" R/cs-runner-grid.R
rg -n "tau\s*=" tests/testthat/test-runner-grid.R tests/testthat/test-collect.R tests/testthat/test-airlock.R
```

**Complexity cost:** Adding or changing a forwarded argument requires identical edits in two 20-line calls. The branch communicates a special semantic distinction that does not exist after default resolution.

**Recommended direction:** Resolve one effective tau value (`cs_tau_oracle` for NULL, otherwise the supplied value) and make one explicit `cs_run_seeds()` call. Keep the validation and public default unchanged.

**Risks and prerequisites:** Add a focused equivalence test for omitted/NULL tau and a custom-grid forwarding test, including output `tau_id`/score identity. This is a public wrapper, so argument names, default, warning behavior, output ordering, and persistence fingerprints must remain unchanged.

### 7. Governed vocabularies are hidden behind one-use getters

**Construct and location:** `cs_estimand_target_ids()` at `R/cs-contracts.R:12-14`, `cs_estimand_targets()` at lines 17-56, and `cs_non_comparable_reasons()` at lines 99-109.

**Disposition and ratings:** `PROCEED TO PASS 2`; maintenance value **Medium**; behavioral risk **Low**; evidence strength **Strong**.

**Evidence and receipts:** `cs_estimand_target_ids()` returns `c("att", "ate", "qst", "cate")` and has one production caller, `cs_compact_estimand_target_id()`. `cs_estimand_targets()` independently defines a descriptor list with exactly those four names and embedded IDs; it has one production caller through `cs_estimand_target()` and one direct test call in `test-v020-typed-scoring-core.R`. `cs_non_comparable_reasons()` returns a constant vector and is called only by `cs_check_non_comparable_reason()`, which has one production caller plus direct validation tests. None of these functions is exported, S3-registered, dynamically registered, or found in reflection searches.

Reproducible searches:

```text
rg -n "cs_estimand_target_ids|cs_estimand_targets|cs_non_comparable_reasons|cs_check_non_comparable_reason" R tests NAMESPACE DESCRIPTION inst/design
rg -n "att|ate|qst|cate|target_not_implemented|truth_unavailable" R/cs-contracts.R tests/testthat/test-v020-typed-scoring-core.R
rg -n "get\s*\(|match\.fun\s*\(|do\.call\s*\(|UseMethod\s*\(" R
```

**Complexity cost:** The valid estimand IDs have two authoritative-looking representations that can drift. Both constant getters add call navigation without owning a side effect, independent contract, callback, or variation point. The reasons getter does avoid duplicating the vector, but a function invoked only by its adjacent validator is not needed to achieve that.

**Recommended direction:** Make the descriptor list the single source of estimand IDs and validate against its names. Keep the non-comparable vocabulary adjacent to its validator without a separate one-use callable getter, or use one clearly named immutable internal binding if independent introspection is a concrete requirement. Do not combine estimand descriptors and non-comparable reasons into a generic vocabulary framework.

**Risks and prerequisites:** Constitution Article I section 1.7 makes estimand IDs and non-comparable semantics a governed vocabulary. The simplification must preserve every exact ID and reason; it does not authorize adding, removing, or renaming one. Add an invariant test that descriptor names equal embedded `estimand_target_id` values, and retain existing class/message behavior for invalid targets and reasons. No constitutional amendment is needed for a representation-only change, but any vocabulary change would require the applicable governance process.

## 5. Flagged structures cleared for retention

- **Immutable versioned DGP functions.** Repeated setup and small mutations across versions initially looked like strong near-duplicate candidates. They are retained because Constitution Article II section 2.1 freezes released DGP logic and Article VII sections 7.2-7.3 expressly prohibit simplifying or refactoring a published version unless section 7.2.d's bitwise corpus covers generated data and truth/oracle outputs. The registry dynamically stores these functions in its `generator` column, so low textual call counts are not evidence of non-use.

- **DGP and estimator registry generators.** Many exported estimator/DGP functions have no textual `name(` call. `cs_dgp_registry()` and `cs_estimator_registry_base()` store their function objects, `cs_get_dgp()`/`cs_get_estimator()` return them, and runners invoke the descriptor's generator. Constitution Article VII sections 7.4-7.5 requires deterministic version/status resolution; the estimator registry also provides the public extension boundary. Removing “unused” generators would break dynamic dispatch.

- **RNG capture/restoration helpers.** `cs_rng_state_capture()`, `cs_rng_state_restore()`, `cs_with_preserved_rng()`, and `cs_with_mandated_rng()` form a small chain and some calls are one-use. They were retained because they isolate a real global side effect, are used by validation, package load, oracle calculation, planning, and campaign shuffling, and are directly tested in `test-rng-isolation.R`. Constitution Article II sections 2.1-2.2 and `contracts.md`, “RNG Contract,” require the mandated RNG kind without leaking user-session state.

- **Fit, score-record, and score-row fingerprint helpers.** `cs_build_score_fingerprint()`, `cs_score_row_coordinate()`, and `cs_build_score_row_fingerprint()` are used from one identity-attachment path. They are retained as distinct domain operations because Constitution Article V section 5.2 requires separate fit and score grains, while `contracts.md`, “Fingerprints and Pins,” defines separate fit, score-record, and score-row identities. `test-v020-schema4-surfaces.R` independently checks row-coordinate and fingerprint invariants.

- **RDS boundary and caller-specific validators.** `cs_read_rds()`, `cs_write_rds_atomic()`, `cs_validate_staged_result()`, `cs_validate_batch_artifact()`, and `cs_validate_oracle_cache_payload()` contain repeated-looking checks. They protect independently callable filesystem/persistence boundaries with different logical identities. Constitution Article VI and the active correction specification's “Minimum v0.2.0 correction” require same-directory atomic publication, no overwrite, caller-specific validation of existing destinations, and classed fail-closed behavior. Consolidating the shared byte-write mechanics while retaining caller validation is the specified design, not excess defensive programming.

- **Repeated oracle-grant checks.** `cs_register_estimator()` validates public registration input, and `cs_oracle_columns_granted()` validates descriptors again at execution. The second check is not redundant with the first because core descriptors and worker-propagated descriptors do not necessarily cross the public registration call in the same process. Constitution Article III section 3.1 requires physical removal of truth columns except explicit column-scoped oracle grants; fail-closed validation at the execution airlock is justified.

- **Legacy pin-name helper.** `cs_result_pin_name_legacy()` is a trivial one-use wrapper in production, but it is also used directly by migration tests and gives an explicit name to the historical unversioned pin namespace. `contracts.md`, “Fingerprints and Pins,” lines 141-177 requires historical artifact interpretation, and removing the distinction would make current-versus-legacy candidate ordering less obvious. It should be reconsidered only together with the supported historical read boundary.

## 6. Pass 2 shortlist

1. **Unify the per-seed execution boundary (Finding 1).** This removes the largest credible drift risk across runner paths. First add characterization tests for serial/parallel execution, board/staging persistence, warnings, progress, `...`, custom tau, thread provenance, resume rejection, and ordering. The maintainer must approve the exact ownership split between orchestration/resume and execution/persistence. Blast radius is medium-to-high across `cs_run_seeds()`, grid-mode `cs_run_campaign()`, parallel staging, and related tests; planned batches should remain out unless proven equivalent. It can be implemented independently of the other findings. It is pre-existing and should **not** be fixed before the current evidence-only commit or folded into v0.2.0 without a scoped packet.

2. **Retire historical fingerprint fixture builders (Finding 2).** This removes two misleading production concepts with low runtime risk. Characterize that missing/schema-2 fixtures fail at `cs_assert_schema4_resume()` before fingerprint comparison and retain exact error classes. The maintainer must confirm there is no intentional external forensic use of the private builders. Blast radius is limited to `R/cs-fingerprint.R` and `test-v018-fingerprint-schema.R`; it is independent. It is pre-existing and is not a before-commit correction.

3. **Choose the long-term campaign API (Finding 3).** This offers high conceptual value because it can separate incompatible lifecycle, persistence, and return contracts. Before deciding, inventory downstream/public usage and characterize both current modes. The maintainer must choose the canonical mode and deprecation policy through governance. Blast radius is high across public API, docs, examples, plan/grid tests, and future roadmap work; it should be its own release-scoped design item. It must not be changed before the current commit or in the frozen v0.2.0 correction scope.

4. **Remove obsolete/test-only internals (Findings 4 and 5).** `cs_enforce_threads()` and `cs_extract_estimator_result()` are independent low-risk removals that can share a small maintenance ticket. Preserve scoped thread-cap tests and move any unique adapter cases to canonical output-normalization tests. The maintainer need only confirm absence of unpublished `:::` use. Blast radius is limited to two source files and one adapter test file; either removal can proceed independently. Neither belongs before the current evidence-only commit.

5. **Make governed vocabularies single-source (Finding 7).** This prevents estimand ID drift and removes one-use getters without creating a framework. Add descriptor-name/embedded-ID invariants and retain exact invalid-target/reason tests. The maintainer must choose an adjacent literal versus a clearly named internal constant for non-comparable reasons; no vocabulary value may change. Blast radius is low and centered on `cs-contracts.R` and typed-scoring tests; it is independent. It is pre-existing and not a before-commit correction.

## 7. Appendix: deferred mechanical candidates

| Construct | Location | Why flagged | Why excluded or deferred |
| --------- | -------- | ----------- | ------------------------ |
| Duplicate local `resolve_config()` blocks | `R/cs-plan-campaign.R:96-102`; `R/cs-run-campaign.R:183-189` | Textually equivalent defaults/override merging at planning and execution | Small and readable; the surrounding input models differ. Reassess with the campaign API/worker design rather than add a helper solely for six lines. |
| Current-schema metadata projections | `cs_science_payload()`, `cs_meta_flatten()`, `cs_result_to_row()`, `cs_pin_write()`, `cs_audit()` | Identity fields are projected repeatedly and could drift | The outputs intentionally have different public shapes, list-column rules, and historical fallbacks; a generic projection may obscure them. Existing schema-surface tests currently synchronize the important fields. |
| `est_gengc()` and `est_gengc_dr()` | `R/est-gengc.R`; `R/est-gengc-dr.R` | Similar validation, bootstrap, error, and result-construction shells | The model engines and scientific estimands differ. No evidence yet shows that a shared abstraction would reduce concepts rather than hide scientific control flow. Characterize numerical and failure behavior before reconsidering. |
| `est_lm_att()` and `est_ipw_att()` bootstrap shells | `R/est-lm-att.R`; `R/est-ipw-att.R` | Similar input checks, seed requirements, bootstrap CI, and result metadata | These are short, independently readable estimator modules. Sharing their scientific execution envelope would add coupling for modest duplication; evidence did not meet the main threshold. |
| Local `wrap_call <- function(expr)` | `R/cs-run-one-seed.R:98-103`; `R/cs-runner.R:733-738` | Named functions used once | Lazy evaluation gives one scoped environment wrapper around a long call without duplicating it. Revisit as part of Finding 1; isolated removal would make control flow longer or denser. |
| Local `tick()` progress helper | `R/cs-runner.R:722-725` | One-use named local function | Can likely be inlined into `on.exit()`, but the maintenance gain is negligible and it will disappear or move if Finding 1 proceeds. |
| `cs_abort_target_not_implemented()` | `R/cs-contracts.R:147-159` | One direct production caller | Names a staged governed failure and owns a stable class/message. Inlining provides little value while Wave 1/Wave 2 staging remains active. |
| `cs_pin_exists()` | `R/cs-pins.R:115-127` | Thin boolean wrapper over `cs_find_result_pin()` | Used in production and directly in persistence/resume tests; it expresses a common query and is part of historical-pin handling. No material over-extraction established. |
| `cs_validate_ci()` | `R/utils-bootstrap.R:25-31` | One production caller | Isolates the CI validity tuple returned by the bootstrap boundary. Small, but independently meaningful; no drift or navigation cost was demonstrated. |
| Repeated DGP registry columns and descriptions | `R/cs-dgp-registry.R` | Large parallel vectors are verbose and can misalign | Registry invariants, generator ordering, versions, and statuses are governed and validated. A row-wise representation might read better but would be a broad mechanical rewrite with no demonstrated current failure. |
| Roxygen and inline comments that restate simple behavior | Several runner and registry files | Mechanical scan found examples such as “version specified” and “pre-flight governance warning” | Mostly harmless navigation aids; redundant-comment cleanup alone would create churn and falls outside the active release's minimum documentation work. |

## 8. Verdict

`MILD ACCIDENTAL COMPLEXITY — TARGETED FOLLOW-UP RECOMMENDED`
