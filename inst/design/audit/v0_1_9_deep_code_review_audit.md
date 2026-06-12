# CausalStress v0.1.9 Deep Code Review Audit

**Date:** 2026-06-11
**Scope:** Full `R/` source (~8,500 LOC), `inst/design/` documents, DESCRIPTION/NAMESPACE packaging, `inst/dgp_meta/` sidecars.
**Method:** Five parallel review passes (runner core/airlock/RNG; v0.1.9 batching pipeline; registries/validation/gatekeeper; DGPs/truth oracles; estimators/bootstrap/pins), each briefed against the Constitution (v1.8.1), the v0.1.8 Patch Spec, and the v0.1.9 Batching Design. All CRITICAL findings were independently re-verified against source.
**Status:** OPEN — findings to be routed into the next release cycle. No fixes have been applied.
**Revision 2 (2026-06-11):** Amended after an independent adversarial peer review (Codex) that re-verified every finding against source and reproduced the critical claims at runtime. Verdicts: C2, C3, C5 and 15 majors CONFIRMED with repros; C1 and C4 DOWNGRADED/RESTATED; M14 REFUTED (moved to minors, restated); M17 DOWNGRADED to minor; M9, M10 RESTATED; one new MAJOR added (M19). Finding IDs are kept stable; amendments appear as **Rev 2** notes inside each affected finding. Post-review tally: **3 critical, 19 major**.
**Disposition rule:** Every finding below must be either (a) ticketed into the next spec packet, (b) explicitly deferred with rationale, or (c) rejected with rationale. Silent omission is not a valid disposition.

---

## 1. Summary Verdict

The scientific core is sound. The placebo sharp nulls, structural ATT definition, canonical QST grid, oracle Monte Carlo (including the v1.6.0 CRN variance reduction), and the non-oracle airlock path are all correctly implemented and verified (see Section 7).

The operational machinery around that core has serious defects: **3 critical** (broken resume, package-load RNG mutation, broken packaging — all reproduced at runtime by the adversarial review), **19 major** (including silent loss of non-estimator task errors in batches and a wrong estimand in `est_tmle`), and a long tail of minors. Several of the worst defects survive because tests were written around the bug rather than through the documented contract (see Section 8).

---

## 2. Critical Findings

### C1. Non-estimator batch task errors are silently discarded
> **Rev 2: DOWNGRADED to MAJOR; mechanism restated.** Adversarial review reproduced both paths: an ordinary *estimator* failure is caught inside `cs_run_single()` (`R/cs-runner.R:143-161`) and survives as a `success = FALSE` result row; it does **not** vanish. What vanishes entirely (no result row, no error row) are tasks whose error escapes `cs_run_single()` — DGP generation failures, registry lookup failures, contract-validation aborts, and the M15 `packageVersion()` crash. Repro: estimator error → `success=FALSE, errors_nrow=0`; batch-level lookup error → `results_len=0, errors_nrow=0`. Severity remains high because combined with M15 a missing Suggests package silently erases *every* task using that estimator from every batch, and the consolidator never reconciles result counts against the plan.
- **Where:** `R/cs-run-batch.R:94`
- **Evidence:**
  ```r
  error = function(e) {
    ...
    error_rows[[length(error_rows) + 1L]] <- tibble::tibble(...)
  ```
- **Defect:** Inside the `tryCatch` error *handler function*, `<-` performs complex assignment on a local copy of `error_rows`; the enclosing variable in `cs_run_batch()` is never updated (requires `<<-`). The `results[[...]] <- res` at line 87 is unaffected because the tryCatch *expr* evaluates in the loop frame. (Handler-assignment semantics confirmed by minimal repro: `handler_assign_<- = 0`, `handler_assign_<<- = 1`.)
- **Consequence:** Tasks failing outside the estimator path appear in neither `results` nor `errors` — the batch artifact under-reports task count with no error trail and no count reconciliation. Violation of DESIGN_V0.1.9_BATCHING §4.2 ("Catch errors, log to errors tibble, continue").
- **Test gap:** `tests/testthat/test-v019-worker.R` only exercises the loop-level "missing n" path (line 44), which assigns in the loop frame and works.
- **Fix:** One character (`<<-`); a test forcing a DGP/registry-level error inside a batch; and a consolidator check that `length(results) + nrow(errors) == nrow(tasks)` (count reconciliation closes the residual silent-omission class).

### C2. Resume (`skip_existing = TRUE`) is broken for all default configurations
> **Rev 2: CONFIRMED with runtime repro** — `resume = "Configuration fingerprint mismatch for synth_placebo_tau0 x lm_att seed 1. Stored: 794315… Current: 793b94…"`.
- **Where:** `R/cs-runner.R:108-119` (store path) vs `R/cs-runner.R:473-498` (resume path); `R/cs-fingerprint.R:21-37`
- **Evidence:** The store path injects `ci_method` **and** `ci_method_source` (`"runner_bootstrap"` / `"runner_none"`) into `config_local` before fingerprinting. `cs_build_config_fingerprint()` strips `seed` and `ci_method` from the payload but **not** `ci_method_source`. The resume-side expected-fingerprint builders (`apply_runner_defaults` / `build_expected_fp_schema2`; also `cs-run-campaign.R:175-244`, `cs-run-one-seed.R:36-76`) inject only `seed`/`n_boot`, never `ci_method_source`. Additionally, for `bootstrap = FALSE` the stored fingerprint's `ci_intent` is `"none"` (derived from the injected `ci_method`) while the expected fingerprint's is `"default"`.
- **Consequence:** Any pinned run where the user did *not* explicitly pass `config$ci_method` — i.e., the documented default usage — can never be resumed; `skip_existing = TRUE` always stops with "Configuration fingerprint mismatch". The skip/resume safety mechanism is effectively non-functional.
- **Test gap:** The "Happy Resume" test (`test-usability-permutations.R:19,34`) passes `config = list(ci_method = "none")` explicitly; `test-resume.R:30-42` hand-crafts a fake pin whose fingerprint omits the injected fields. Both dodge the default path.
- **Related (see M3, M12):** parallel runs fingerprint the *forced* `num_threads = 1L`; the planner hashes raw configs bypassing the schema-2 normalizer.
- **Fix:** Requires a deliberate fingerprint schema-3 decision: strip `ci_method_source` (and any runner-injected bookkeeping field) from the hashed payload, normalize `ci_intent` derivation so store and resume paths agree, and add `dgp_version` (M11) in the same bump. Provide deterministic legacy comparison for schema-2 pins.

### C3. `library(CausalStress)` permanently mutates the user's global RNG state
> **Rev 2: CONFIRMED with runtime repro** — on load: `sample.kind` flips `Rejection → Rounding` and `.Random.seed` is overwritten; campaign shuffle under the same `campaign_seed` differs across RNG-kind states (`identical = FALSE`).
- **Where:** `R/zzz.R:9-11` → `R/cs-validate-registry.R:125-146` → `R/cs-rng.R:8-14`
- **Evidence:** `.onLoad` runs `cs_validate_dgp_registry(strict = FALSE)` (default `causalstress.validate_on_load = TRUE`), which executes all 24 registered generators with `seed = 1L`. Each generator calls `cs_set_rng()`, which sets `RNGkind("Mersenne-Twister", "Inversion", "Rounding")` and `set.seed(1)` with no save/restore anywhere.
- **Consequence:** Merely attaching the package switches the user's session to the legacy `sample.kind = "Rounding"` sampler and overwrites `.Random.seed`, silently changing the behavior of any unrelated simulation in the same session. This violates the side-effect-isolation intent of Constitution Art. II §2.1 (the RNG-lock carve-out covers benchmark execution, not package load). It also interacts with C2's cousin: the campaign shuffle permutation (`withr::with_seed` at `cs-run-campaign.R:281` preserves seed but not kind) depends on the ambient `sample.kind`, so plans generated with `validate_on_load` disabled differ from plans generated after a normal load — and resume matches by `batch_id` only, silently remapping tasks.
- **Fix:** Wrap load-time validation (and any registry generator probe) in full RNG state save/restore (`.Random.seed` + `RNGkind`), e.g. `withr::with_preserve_seed()` plus explicit kind restore. Pin `sample.kind` explicitly inside `cs_plan_campaign` so the shuffle is unconditional.

### C4. `est_tmle` reports an ATE under the `tmle_att` id in an ATT benchmark
> **Rev 2: DOWNGRADED to MAJOR.** The wrapper is self-documenting: the roxygen header reads "TMLE estimator (ATE target)" (`R/est-tmle.R:1-6`) and meta records `estimand_target = "ATE"` plus a warning string (`:146-147`). So this is a documented-but-wrong target, not a silent one. It remains top-priority because the `estimator_id` is `"tmle_att"` in both the wrapper (`:142`) and the registry (`R/cs-estimator-registry.R:22`), its results flow into ATT bias/coverage columns, and the adversarial repro quantified the damage: `fit_ATE = 1.04`, `fit_ATT = 2.41`, `wrapper_est = 1.02`, `true_ATT = 1.69` — and confirmed `fit$estimates$ATT` is available, so the fix is one line.
- **Where:** `R/est-tmle.R:83-84` (also `:112`, `:131` per review)
- **Evidence:**
  ```r
  est <- fit$estimates$ATE$psi
  ci  <- fit$estimates$ATE$CI
  ```
- **Defect:** An estimator named and registered as `_att` extracts the ATE. The `tmle` package computes `fit$estimates$ATT` natively with the correct ATT influence curve (the ATT clever covariate differs from the ATE's).
- **Consequence:** Under effect heterogeneity (e.g. `synth_baseline`'s τ(X) = 1 + 0.5·X₁), bias is computed against the wrong estimand and the reported CI is the ATE's, so coverage statistics are doubly wrong in every cross-estimator comparison that includes tmle_att.
- **Fix:** One line (`fit$estimates$ATT`), plus a heterogeneous-effect regression test comparing tmle_att against oracle ATT. Alternatively rename/relabel the estimator to `tmle_ate` and exclude it from ATT scoring — either resolution is acceptable; the current id/estimand mismatch is not.

### C5. Packaging breaks fresh installs and R CMD check
- **Where:** `DESCRIPTION`
- **Evidence:** `dplyr`, `purrr`, `tibble` are used via `::` across most of `R/` but are declared nowhere in DESCRIPTION (neither Imports nor Suggests). `pins` is Suggests-only yet used unguarded in 9+ core files (zero `requireNamespace("pins")` guards anywhere). `withr` is Suggests-only but on the core campaign path (`cs-run-campaign.R:281`).
- **Consequence:** R CMD check error ("'::' calls not declared from"); hard runtime failure for any user installing without those packages already present; core persistence fails without `pins`.
- **Fix:** Move `dplyr`, `purrr`, `tibble`, `pins`, `withr` to Imports (or guard genuinely optional paths with `requireNamespace`). Run R CMD check in the release gate (see governance proposal).

---

## 3. Major Findings — Constitutional / Governance

### M1. Airlock oracle path over-grants; the constitutional `use_true_propensity` mechanism does not exist
- **Where:** `R/cs-airlock.R:4-7`; `R/cs-runner.R:98-99`
- **Evidence:** `if (isTRUE(oracle_allowed)) return(df)` — the full raw dataframe (`y0`, `y1`, `p`, `structural_te`, plus all attributes, with no attribute stripping) is returned for any estimator whose *registry entry* sets `oracle = TRUE`. `config$use_true_propensity` / `config$use_oracle` (Constitution Art. III §3.1: oracle estimators "MAY access `p`") appear nowhere in `R/`.
- **Consequence:** A propensity-oracle-only estimator cannot be expressed; any registry-flagged oracle estimator receives full counterfactual truth. The exception leaks strictly more than the Constitution permits.

### M2. Gatekeeper mislabels CI-less estimators; QST gate counts them as PASS
- **Where:** `R/cs-gatekeeper.R:36-43` (ATT), `R/cs-gatekeeper.R:93-115` (QST)
- **Evidence:** ATT path: all-NA `att_covered` → `mean(..., na.rm=TRUE)` is `NaN` → `ifelse(NaN >= threshold, ...)` → `status = NA` (Constitution Art. IV §4.2.3 requires "Unverified"). QST path: a run whose `covered` is entirely NA gets `run_fail = FALSE`, so an estimator producing QST point estimates with **no CIs at all** ends with `run_fail_rate = 0` → `"PASS"`; the `is.na(run_fail_rate)` → "UNVERIFIED" branch is unreachable for this case.
- **Consequence:** The gate's core purpose — refusing to certify estimators that cannot be tested — is defeated for QST.

### M3. Parallel-computed pins are permanently unresumable
- **Where:** `R/cs-runner.R:605-607`; `R/cs-run-one-seed.R:110-114`
- **Evidence:** Under experimental parallel, `config_eff$num_threads <- 1L` flows into `cs_run_single()` and is fingerprinted (`num_threads` is not stripped in `cs-fingerprint.R`); the resume-side expected fingerprints are built from the *unmodified* config.
- **Consequence:** Resuming a staged-and-gathered parallel campaign hits "Configuration fingerprint mismatch" unless the user happened to set `num_threads = 1` themselves. Fold into the C2 schema-3 fix.

### M4. Plan-mode campaigns bypass the v0.1.8 experimental-parallel gate entirely
- **Where:** `R/cs-run-campaign.R:85-96`; `R/cs-campaign-runner.R:62-64`; `R/cs-scale-helpers.R:51-63`
- **Evidence:** `cs_run_campaign()` returns into `cs_run_campaign_plan()` *before* `cs_require_experimental_parallel()` (line 148) is reached; `cs_campaign_runner` then sets `future::plan(multisession, workers = detectCores() - 1L)` with no flag, no loud warning, no provenance (`experimental_parallel`, `parallel_backend`, `thread_caps_applied`). Inside workers, `cs_enforce_threads(1L)` mutates `OMP_NUM_THREADS` etc. via permanent `Sys.setenv` with no restore — the v0.1.8 Patch Spec mandates scoped (`with_envvar`-style) caps, which the grid path correctly uses (`cs_with_envvar`, `cs-scale-helpers.R:65-80`).
- **Consequence:** The entire v0.1.8 Patch A1/A3 governance machinery is absent from the package's newest and highest-throughput execution path.

### M5. `cs_run_campaign` silences DGP governance warnings with no pre-flight
- **Where:** `R/cs-run-campaign.R:81,304`; `R/cs-run-one-seed.R:14`
- **Evidence:** Defaults `quiet = TRUE` and contains no `cs_get_dgp(..., quiet = FALSE)` pre-flight, unlike `cs_run_grid` (`cs-runner-grid.R:54`) and `cs_run_seeds` (`cs-runner.R:648-653`).
- **Consequence:** Deprecated/invalidated/experimental DGP selection in a campaign emits no warning. Constitution Art. VII §7.4: "Internal runners MUST NOT silence warnings."

### M6. Stable DGP versions are never validated (first-match bug)
- **Where:** `R/cs-validate-all.R:11-13`
- **Evidence:** `gen <- reg$generator[[match(id, reg$dgp_id)]]` — `match()` returns the first row per `dgp_id`, which is the *deprecated* version (e.g. `synth_baseline` v1.3.0). The stable v1.6.0 generators are validated zero times; deprecated ones are validated repeatedly.
- **Fix:** Iterate over registry rows, not ids.

### M7. Pin identity and config fingerprint omit `dgp_version`
- **Where:** `R/cs-pins.R:5-8`; `R/cs-fingerprint.R:108-125`
- **Evidence:** Pin name is `results__dgp={id}__est={id}__n={n}__seed={seed}` — no DGP or estimator version. The schema-2 fingerprint includes `estimator_version` but not `dgp_version`.
- **Consequence:** After a DGP version bump (routine under Art. VII), `skip_existing = TRUE` finds the old-version pin, the fingerprint *matches*, and stale old-version results are silently returned as if computed under the newly resolved stable version; `force = TRUE` deletes the historical pin, against Art. VII §7.6. Fold into the C2 schema-3 fix.

### M8. Oracle truth depends on unversioned constants; cache is fingerprint-less and non-atomic
- **Where:** `R/cs-oracle-truth.R:8-9, 99, 133`
- **Evidence:** Truth values are a deterministic function of `ORACLE_SEED <- 99999L`, `chunk_n <- 200000L` (chunking changes RNG interleaving and which 10⁶ treated draws are retained), `quantile type = 7`, and the retention algorithm — none captured by `(dgp_id, version)` or the cache key; the `.qs` cache stores no fingerprint of the oracle algorithm. The cache write is plain `qsave` (non-atomic) and race-prone under parallel runs (the in-process recursion guard does not span processes).
- **Consequence:** Editing any oracle constant silently changes truth only on cold-cache machines — machine-dependent "truth" with no detection mechanism; concurrent workers can read a partially-written cache file.
- **Fix:** Version the oracle algorithm explicitly; include an oracle-algorithm fingerprint in the cache key; temp-write + rename for the cache.

### M9. Bitwise-identity mandate at risk from BLAS/libm dependence
- **Where:** `R/dgp-synth-hd-sparse-plm.R:37-39` (also 114-116, 187-189)
- **Evidence:** `chol(Sigma)` and `Z %*% L` route through the platform BLAS; OpenBLAS/MKL/reference BLAS give last-ulp differences, so Art. II §2.2 "bitwise identical regardless of operating system" cannot hold for this DGP. Generally: every DGP feeds `plogis(...)` (libm `exp`) into `rbinom`, so a 1-ulp platform difference in `p` can flip a treatment assignment; this also affects the lazily computed, machine-local oracle truths (M8).
- **Disposition needed:** Either implement a pure-R Toeplitz transform (no BLAS), or amend the Constitution to scope the bitwise guarantee to "same platform/BLAS" and document the limitation.
> **Rev 2: RESTATED.** The BLAS/libm sensitivity of the cited operations is verified in source, but cross-platform divergence was not reproducible from a single Windows host. Treat as a verified *risk* requiring the constitutional disposition above, not a demonstrated divergence.

### M10. YAML-sidecar validation is warning-only at load; sidecar fields incomplete
> **Rev 2: RESTATED (narrowed).** The adversarial review found that testthat tests *do* cover sidecar noise/effect drift, so "drift can never fail" was too strong — drift in those fields would fail the test suite. The remaining gaps: the package's own load path is warning-only (`strict = FALSE`), `strict = TRUE` is never exercised by the package itself, sidecar `version`/`status` fields are never compared to the registry, and one sidecar per `dgp_id` is compared against every registered version's executable meta (per-version metadata drift is unrepresentable).
- **Where:** `R/cs-validate-registry.R:74-113`; `R/zzz.R:10`
- **Consequence:** v0.1.8 Patch B4's acceptance criterion is only partially met: noise/effect drift is test-covered, but version/status drift and per-version claims are not.

### M11. Deprecated/invalidated warning omits `date_status_changed`
- **Where:** `R/cs-dgp-registry.R:217-223`
- **Evidence:** Warning includes dgp_id, version, status, rationale but not the date-of-status-change required by Art. VII §7.4, despite `row$date_status_changed` existing in the registry.

### M12. Planner fingerprints bypass the schema-2 normalizer
- **Where:** `R/cs-plan-campaign.R:111-132`
- **Evidence:** The plan hashes raw config lists with bare `digest::digest(cfg)` while labeling rows `config_fingerprint_schema = 2L`. The actual schema-2 normalizer (`cs-fingerprint.R:39-126`) sorts names, canonicalizes numerics, and rejects functions/environments.
- **Consequence:** Semantically identical strategy maps with different key order or `200` vs `200L` produce different `task_fingerprint`s; a closure smuggled into `strategy_map` hashes its environment via serialization (session-dependent) instead of erroring; the schema label is false.

---

## 4. Major Findings — Correctness / Robustness

### M13. All-bootstrap-failure still reports `success = TRUE`; no warning below the 0.9·B gate
- **Where:** `R/cs-runner.R:158-178`; `R/utils-bootstrap.R:80`
- **Evidence:** `success` only flips on estimator error; `n_boot_ok` is copied from estimator meta and never affects it. The `n_boot_ok >= 0.9 * n_boot` gate silently NAs the CIs (`ci_fail_code = "low_boot_success"`) with no `warning()`.
- **Consequence:** Violates the documented runner contract ("all-bootstrap-failure → success = FALSE"; "warn if n_boot_ok < 0.9·B"). Failed inference is indistinguishable from "CI not requested" without inspecting fail codes.

### M14. ~~Requested `tau` is never validated against the canonical grid~~
> **Rev 2: REFUTED as stated.** The Constitution explicitly says the opposite of this finding's premise: "Estimators MUST calculate QST only for the tau values provided by the Runner. **The canonical grid applies to truth tables, not estimator inputs**" (Constitution Art. III §3.1, line 102), and the estimator-output contract correctly checks the *requested* tau (`R/cs-contracts.R:258-261`). Non-canonical tau is constitutionally legal. The residual issue — non-canonical values left-join to NA truth at `R/cs-runner.R:193-216` and produce silently-NA bias/coverage rows with no message — is a reporting/handling gap, reclassified as **MINOR** (see minors list, item 39). The original finding conflated a v0.3.0 design-doc rule with the Constitution; the Constitution wins.

### M15. Missing optional estimator package kills the whole grid
- **Where:** `R/cs-runner.R:252-261`
- **Evidence:** After the estimator's own `requireNamespace` abort is correctly caught (`success <- FALSE`), the provenance block calls `utils::packageVersion(pkg)` on the missing package **uncaught**, aborting `cs_run_single` — and with it the entire `cs_run_grid`/suite — instead of producing a `success = FALSE` row.

### M16. `cs_run_campaign`'s `...` is documented as forwarded but silently dropped
- **Where:** `R/cs-run-campaign.R:38, 287-315`; `R/cs-run-one-seed.R:22, 116-132`
- **Evidence:** `run_task` passes `dots` into `cs_run_one_seed_internal`, whose signature accepts `...` but whose body never forwards it to `cs_run_single()`. Related: the planner docs advertise a `tau` strategy field, but `cs_run_batch` hardcodes `tau = cs_tau_oracle` — the user's value is ignored yet still perturbs the fingerprint (`cs-run-batch.R:71` vs `cs-plan-campaign.R:20`).

### M17. `cs_tidy_run` is defined twice; collation order silently picks the alias
> **Rev 2: DOWNGRADED to MINOR.** The duplicate definition and duplicated Rd alias are real (`R/cs-tidy-run.R:21`, `R/cs-tidy.R:31`, `man/cs_tidy.Rd:5`), but the review found the alias behavior largely equivalent for single-run inputs, so the user-visible impact is the R CMD check WARNING and dead code, not wrong results. Retained here for ID stability; treat as minor in routing.
- **Where:** `R/cs-tidy.R:31` (`cs_tidy_run <- cs_tidy`) vs `R/cs-tidy-run.R:21` (standalone function)
- **Evidence:** With no `Collate` field, C-locale ordering sources `cs-tidy-run.R` first, so the alias in `cs-tidy.R` wins and the documented standalone implementation is dead. `\alias{cs_tidy_run}` appears in two Rd files — an R CMD check WARNING.

### M18. Batching pipeline spec gaps (error schema, consolidator validation, tidy propagation, idempotency)
- **Where:** `R/cs-run-batch.R:44-51, 94-101`; `R/cs-consolidate.R:36-71, 47-50, 101`; `R/cs-result-to-row.R:16-55`
- **Evidence:**
  - Error rows omit `task_fingerprint` and `error_class` (spec §3.2 mandates both).
  - The consolidator validates `schema_version`, `batch_id`, and `tau_id`-when-qst, but never `task_fingerprint` / `config_fingerprint_schema` (spec §4.3 step 1; Testing Strategy #4).
  - `cs_result_to_row` propagates neither field into tidy rows (spec §5), although the worker stamps both into `res$meta`.
  - Consolidator idempotency is name-only and TOCTOU (`pin_exists` → `pin_write` window); a stale pin from a different plan with the same `batch_id` silently wins; skipped duplicate staged files are never cleaned and re-warn forever.

### M19. `cs_validate_dgp()` certifies DGPs with no potential-outcome columns *(added in Rev 2 by the adversarial review)*
- **Where:** `R/cs-validate-dgp.R:29-38, 64`
- **Evidence:**
  ```r
  true_y0 <- df$true_y0 %||% df$y0
  true_y1 <- df$true_y1 %||% df$y1
  schema_ok <- ... && !any(is.na(true_y0)) && !any(is.na(true_y1))
  ```
- **Defect:** When both column variants are absent, `%||%` yields `NULL`, and `!any(is.na(NULL))` evaluates to `!any(logical(0))` = `TRUE` — the schema check passes vacuously. Reproduced: a synthetic DGP with no `y0`/`y1` at all returns `valid=TRUE, schema=TRUE, determinism=TRUE`.
- **Consequence:** The public DGP validator green-lights structurally invalid synthetic DGPs. `cs_check_dgp_synthetic()` would still reject them at run time, but the tool whose purpose is pre-flight certification gives a false all-clear — exactly the kind of gap that lets an unconstitutional community DGP appear vetted.
- **Fix:** Require the columns explicitly for `type = "synthetic"` (`if (is.null(true_y0) || is.null(true_y1)) schema_ok <- FALSE`), and add a negative test.

---

## 5. Minor Findings (condensed)

**Validation / registry**
1. `cs_validate_tau_grid` uses exact `identical()` on doubles — `(1:99)/100` is rejected; inconsistent with the package's own tolerant `cs_tau_id()` (`cs-validate-tau.R:22`).
2. `rlang::abort` with un-interpolated glue braces — user sees literal `{toString(.valid_dgp_status)}` (`cs-dgp-registry.R:186-188`).
3. Resolver alone accepts malformed semver (`package_version()` parses `"1.3"`, `"1.2-3"`); the strict regex lives only in registry validation (`cs-dgp-registry.R:198-203`).
4. Synthetic contract never checks covariate columns; Constitution says `x1...xk` (lowercase), DGPs return `X1..Xk` — both the naming mismatch and the absence of any check (`cs-contracts.R:44`).
5. No real-DGP contract path: `cs_check_dgp_synthetic` is applied unconditionally (`cs-runner.R:93`); any future `type = "real"` DGP is rejected at run time.
6. `cs_check_estimator_output` never requires `meta$ci_type` when CIs are reported (`cs-contracts.R:174-269`).
7. Registry construction re-reads 24 YAML files on every call; a missing sidecar aborts the constructor, which prevents package load entirely (`cs-dgp-registry.R:162-175`); the `file.path("inst", ...)` fallback only works from the package root.
8. `$` partial-matching hazards in contract extraction (`att$estimate`, `att_ci$ci_lo`) — use `[[` with exact names (`cs-contracts.R:210`; `cs-runner.R:174-175`).
9. `cs_validate_dgp`: determinism probe hardcodes `seed = 123` ignoring the `seeds` argument; `cv_true_att = sd/abs(mean)` is NaN/Inf for placebos (`cs-validate-dgp.R:43-61`).
10. Two different definitions of "placebo": suite membership by `grepl("placebo", dgp_id)` (`cs-suite.R:14`) vs gatekeeper by YAML tags (`cs-gatekeeper.R:18-20`). Coincide today; will drift silently.

**Runner / RNG / campaign**
11. `cs_set_rng` never restores caller RNG state (`on.exit`/withr absent) — every runner call leaves the session in Rounding-kind state (`cs-rng.R:8-13`). (Load-time variant is C3.)
12. Double pin write in the serial board path: `cs_run_single` pins internally, then the wrapper pins again (`cs-runner.R:598, 641-643`; same in `cs-run-one-seed.R:103, 146-148`).
13. `cs_run_campaign` defaults `B = 0L` (others default 200): `bootstrap = TRUE, B = 0` produces a half-configured state — estimator bootstraps with its own default while `ci_intent` stays `"default"` and the resume boot-CI guard is skipped (`cs-run-campaign.R:72, 255`; `cs-fingerprint.R:30`).
14. Locale-dependent timeout classification — English-only `grepl` on condition messages (`cs-runner.R:300`).
15. grf/tmle point estimates differ between `ci_method = "none"` and `"bootstrap"` because the final full-data fit runs *after* the bootstrap consumed RNG draws (`est-grf-dr.R:183-187`; `est-tmle.R:116-120`); gengc fits the point estimate first and is unaffected.
16. Worker `file.rename` result unchecked in the atomic staging write (`cs-run-batch.R:149-151`) — on Windows AV/indexer locks a failed rename silently loses the batch; contrast `cs_stage_result` which checks.
17. `n_seeds` scalar gotcha: a single value is always a count (`n_seeds = c(5)` means seeds 1..5) (`cs-plan-campaign.R:77-81`).
18. UUID entropy claim illusory (`sample.int` in fully-determined RNG state); `node_info[["nodename"]]` can throw uncaught in the consolidator (`cs-run-batch.R:139-145`; `cs-consolidate.R:79`).
19. `cs_audit` reads every pin's full payload just to derive metadata; `git_hash` always NA for batch pins; consolidate's character timestamp ignored by `extract_ts` (`cs-provenance.R:39-83`; `cs-consolidate.R:90-97`).
20. `cs_stage_result` errors opaquely ("argument is of length zero") when a meta field is NULL — `glue` returns `character(0)` (`cs-staging.R:9-14`).

**Estimators / statistics**
21. `est_tmle`/`est_bart` take *every* non-`y`/`w` column as confounders — no local defense against oracle columns if called outside the runner airlock; all other estimators defend locally (`est-tmle.R:57`; `est-bart.R:96`).
22. IPW: no trimming/clipping/warning as `e_hat → 1`; weights explode, `e_hat == 1` yields NaN ATT; undocumented (`est-ipw-att.R:87-90`).
23. Bootstrap is iid, not stratified by treatment, and undocumented; replicates with zero treated/controls are dropped, conditioning CIs on resample composition exactly where overlap-stressed DGPs bite (`utils-bootstrap.R:39,70`).
24. `est_tmle`/`est_bart` meta omit `version`/`capabilities`; they are also the only estimators that never self-check with `cs_check_estimator_output()` (`est-tmle.R:141-161`; `est-bart.R:164-182`).
25. gengc/gengc-dr emit legacy `value` column in QST (contract: `estimate`), surviving only via a compat shim (`est-gengc.R:60-64`; `cs-contracts.R:230`).
26. `cs_summary` mixes NA policies: one failed run NAs all bias metrics while coverage silently drops NAs; no `n_failed` count (`cs-summary.R:59-64`).
27. Deprecated `.data` pronoun in tidyselect contexts — lifecycle warnings on current tidyr/dplyr (`cs-summary-qst.R:18`; `cs-plots.R:73`).

**Pins / provenance / hygiene**
28. Git hash provenance taken from the session's *current working directory* repo — can record an unrelated repo's HEAD; full `sessionInfo()` serialized into every pin's YAML metadata (`cs-pins.R:10-14, 55`).
29. Unescaped IDs interpolated into a regex in `cs_delete_campaign` — an id containing `.` can match and delete another DGP's pins (`cs-pin-management.R:32-35`).
30. `include_truth = FALSE` hard-errors on half the DGPs (heavytail, qte1, tilt_mild, all placebos return NULL truth into a validator that requires a tibble); the other half return an NA-tibble and pass — inconsistent contract handling (`dgp-synth-heavytail.R:74,101`).
31. `dgp_synth_baseline` (both versions) never runs `cs_check_dgp_synthetic`, unlike every other DGP family (`dgp-synth-baseline.R:60-84, 135-159`).
32. Zero treated units → `mean(numeric(0)) = NaN` truth with an opaque downstream error rather than a "no treated units" diagnostic (`cs-truth.R:9-11`).
33. Placebo zero-truth keyed on the `synth_placebo` name *prefix*, not on verified sharp-null behavior — a future mis-built placebo would silently get exactly-zero truth (`cs-oracle-truth.R:72-80`).
34. Exported legacy `cs_true_qst` computes "truth" from realized finite-sample quantiles — the exact pattern Art. I §1.4 forbids; unused by any DGP but exported, documented as truth, and still described as the mechanism in `CAUSAL_STRESS_MVP_STATUS.md` (`cs-truth.R:23-29`).
35. `synth_heavytail` v1.6.0 changes the data-path RNG draw order vs v1.3.0 (mix indicators interleaved differently) while its docstring implies only the oracle changed; every other v1.6.0 bump preserved the predecessor's stream order (`dgp-synth-heavytail.R:134-151`).
36. `synth_hd_sparse_plm` accepts `oracle_only` in all three signatures but never branches on it (`dgp-synth-hd-sparse-plm.R:29, 106, 180`).
37. Dead/placeholder files: `R/cs-run-single.R`, `R/cs-run-suite.R`, `R/estimator-gengc.R`, `R/estimator-grf-dr.R` are 0 bytes; `tmp_eval.R` and `tmp_flaky.R` are tracked at the repo root; duplicated statement at `cs-runner.R:277-279`; dead `y`/`trt`/`w` bindings shadowing the treatment name in `est-bart.R:69-71`.
38. Doc/return mismatches: `cs_validate_dgp_registry` documents "Invisible TRUE" but returns `invisible(reg)`; `cs_run_suite` missing `@param` entries; docs refer to `cs_tau_oracle()` as a function but it is an exported vector.
39. *(reclassified from M14 in Rev 2)* Non-canonical requested `tau` values left-join to NA truth (`cs-runner.R:193-216`) and yield silently-NA bias/coverage rows with no message. Constitutionally legal (Art. III §3.1 scopes the canonical grid to truth tables only), but the silent NA propagation deserves an explicit warning or a documented `truth_available` flag per tau row.

---

## 6. Design-Document Findings (`inst/design/`)

These are defects in the normative documents themselves.

### D1. The v0.3.0 design's runner pseudocode leaks the airlock
`CAUSALSTRESS_DESIGN_v0.3.0.md:430-480` contains two duplicated, mutually contradictory airlock blocks (one conditions on `use_oracle || use_true_propensity`, the second only on `use_oracle` and rebuilds `df_run` from scratch), and then passes the **unsanitized** `dgp$df` to the estimator (line ~454) and to every bootstrap replicate (line ~477). The implementation does it correctly — but the binding spec teaches the bug.

### D2. `cs_collect_att` pseudocode is syntactically invalid
Lines 1409-1417: duplicated `unnest_wider(result)` without a pipe; references an undefined `include_drafts` variable; the `mode = c("certified", "draft")` signature is compared with `==` without `match.arg`.

### D3. Appendix C's DGP template violates Article I §1.4
The template computes QST truth as `quantile(y1[w==1], tau) - quantile(y0[w==1], tau)` from the realized sample — the realized-quantile pattern the Constitution forbids for truth, and the same pattern as the deprecated-but-exported `cs_true_qst` (minor #34). A contributor following the template produces an unconstitutional DGP.

### D4. Stale and conflicting cross-references
- Design v0.3.0 says "aligned with Constitution v1.7.0"; the Constitution is at 1.8.1.
- Pin naming conventions conflict: DESIGN v0.3.0 §5.2 specifies `results/{dgp}_v{ver}/{est}_v{ver}/seed_{seed}`; METADATA spec specifies `results__dgp={id}__est={id}__n={n}__seed={seed}` (what the code implements). The versioned convention was the better one (see M7).
- `CAUSAL_STRESS_DGP_REGISTRY_1.4.0.md` documents `synth_placebo_tilted` as `plogis(1.0·X1 + 1.2·X2)`; the tightened stable version uses `plogis(0.6·X1 + 0.8·X2)` (`dgp-synth-placebo-tilted.R:97`).
- The LaLonde truth table lists 1794 for all four variants while its own v0.2 note says NSW and DW truths differ by ~$900.
- `CAUSAL_STRESS_MVP_STATUS.md` describes the long-superseded realized-quantile truth mechanism as current.

### D5. No authority model
There is no index declaring which of the 16 documents is binding, in what order, or for which version. The v0.3.0 design is marked "Frozen for MVP" yet describes APIs that never shipped (`cs_board`, `cs_run_design`, suite pins); the patch specs amend the Constitution; the registry spec and the metadata spec contradict each other. A reader (human or LLM agent) cannot determine the current normative state without archaeology. **This is the root cause addressed by the companion governance proposal** (`governance_spec_packet_proposal.md`).

---

## 7. Verified Sound

For the record, the following constitutional requirements were checked and confirmed correct. **Rev 2 note:** the adversarial review independently re-checked this section and concurred on the CRN oracle, IPW/LM/GRF ATT targeting, and the gatekeeper arithmetic — with one amendment: the "QST gatekeeper arithmetic" item below is correct *for runs that have CIs*, but is incomplete as an all-clear because the all-NA (CI-less) case passes instead of being labeled Unverified (see M2). Read that item jointly with M2.

- **Sharp nulls (Art. IV §4.1):** all five placebos set `y1 <- y0` by copy with a single noise draw; `true_att` exactly 0; `true_qst` exactly 0 at all 99 points.
- **Structural ATT (Art. I §1.3):** τ(X) is a deterministic function of X only in every DGP; `true_att = mean(structural_te[w==1])`; no DGP computes truth from realized `y1 − y0`.
- **QST truth (Art. I §1.4):** canonical 99-point grid enforced; N = 10⁶ immutability enforced; estimand correctly conditions on simulated W = 1; `quantile(type = 7)` consistent across both arms.
- **CRN v1.6.0 oracle:** identical transform applied to both arms under shared draws; marginals unchanged, so oracle quantiles remain unbiased while Q₁−Q₀ variance drops. v1.3.0 oracles untouched, properly version-bumped, separate cache keys.
- **Kang–Schafer:** transforms, propensity, and outcome match Kang & Schafer (2007) exactly.
- **Non-oracle airlock:** drops exactly `y0, y1, p, structural_te` and strips all non-essential attributes; bootstrap operates on the sanitized df inside estimators.
- **Bootstrap RNG:** seeded via `cs_derive_seed` + `cs_set_rng` under the mandated kind; hard-aborts when bootstrap is requested without a seed.
- **Semver resolution:** numeric comparison (1.10.0 > 1.9.0), correct stable → experimental+warning → error order, deterministic tie errors, ≤1 stable per dgp_id enforced.
- **QST gatekeeper arithmetic:** denominator is the actual grid length; `> 0.10` per-run threshold matches the "10/10 rule"; ATT pass rule uses `>= 0.90`.
- **Estimand targeting:** `est_lm_att` (controls-only outcome model, predicted counterfactuals at treated X), `est_ipw_att` (Hájek-normalized `p/(1−p)` control weights), `est_grf_dr` (`target.sample = "treated"`, `ci_type = "wald"`) all genuinely target ATT.
- **Worker isolation (Art. VI):** workers never touch pins; staging writes are temp + rename; the controller alone gathers and pins.
- **Legacy fingerprints:** the v0.1.7 scheme is kept frozen in a separate function with deterministic comparison and an explicit refusal to resume legacy pins under finite `max_runtime`.

---

## 8. Process Observations

1. **Tests written around bugs, not through contracts.** C1 (worker error loss) and C2 (broken resume) both survive because the tests avoid the documented contract path: the resume tests pass explicit configs or hand-craft fingerprints; the worker test avoids the estimator-error path. The next cycle should add adversarial contract tests that exercise the *documented* default behavior (resume with default config; estimator error inside a batch; CI-less estimator through the gatekeeper; missing Suggests package mid-grid).
2. **Spec drift is systemic, not incidental.** Five of the major findings are "the spec mandates X, the code does Y, nothing detects the gap" (M2, M4, M10, M18, D4). The governance proposal addresses this with release-gate enforcement.
3. **The fingerprint system needs one coordinated schema-3 change** rather than piecemeal fixes: strip runner-injected fields (C2), add `dgp_version` (M7), route the planner through the normalizer (M12), and keep deterministic legacy acceptance for schema-2 and v0.1.7 pins.
4. **(Rev 2)** The adversarial review process worked as designed: of 28 findings, it confirmed 22 with independent evidence and runtime repros, killed one outright (M14 — the auditor conflated a stale design-doc rule with the Constitution), corrected the blast radius of two (C1, C4), and found one MAJOR the original sweep missed (M19) in a file it had only lightly covered. Lesson for the governance proposal: audits feeding a fix cycle should get an adversarial verification pass *before* ticket cut, not after.

## 9. Recommended Fix Order (Rev 2)

| Priority | Findings | Rationale |
|---|---|---|
| 1 | C4 | One-line fix (`fit$estimates$ATT`) or relabel to `tmle_ate`; large scientific payoff |
| 2 | C1 | One-character fix (`<<-`) + consolidator count reconciliation; closes silent task omission |
| 3 | C5 | Unblocks clean installs and CI; prerequisite for a release gate |
| 4 | C2 + C3 + M3 + M7 + M12 | One coordinated fingerprint schema-3 + RNG-isolation change (first RFC candidate) |
| 5 | M2 + M6 + M19 | Small fixes protecting the instrument's core certification claims |
| 6 | M4 + M5 | Restore v0.1.8 parallel governance on the campaign path |
| 7 | M13, M15, M16, M18 | Runner/batch contract conformance |
| 8 | M1, M8–M11 | Constitutional conformance requiring design decisions |
| 9 | Minors (incl. M17, ex-M14 item 39) + design docs | Batch into the cleanup portion of the next packet |
