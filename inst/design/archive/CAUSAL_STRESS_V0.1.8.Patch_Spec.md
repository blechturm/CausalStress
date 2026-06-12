## **Phase 1 — Authoritative Answers (with binding pointers)**

**Q1: Parallel execution in v0.1.x**

-   Decision: “v0.1.x is serial-only” is a documentation artifact, but the spec/code mismatch must be closed.

-   Action (normative): Amend [inst/design/CAUSAL_STRESS_CONSTITUTION.md]{.underline} Article V §5.1 to: **“Serial by default; parallel execution permitted only in experimental mode under strict protocol (Article VI).”**

-   Action (code governance): gate parallel=TRUE behind experimental_parallel=TRUE (default FALSE), emit a loud warning, record provenance flags (experimental_parallel, parallel_backend, staging_dir_used, thread_caps_applied).

-   Threat-model alignment: per [inst/design/THREADMODEL.md]{.underline}, this is not “security”; it’s anti-footgun governance.

**Q2: Definition of “science payload”**

-   Decision: science payload excludes logs/warnings/timestamps/runtimes; includes estimands + declared uncertainty + minimal identifiers.

-   Action: define accessors:

    -   cs_science_payload(x) (canonicalized, stable ordering, excludes logs/warnings and all provenance)

    -   cs_provenance(x) (timestamps, runtimes, host/session, warnings/errors/logs, timeouts, thread caps, parallel flags)

    -   cs_meta_flatten(x) (flat, analysis-friendly identifiers: dgp_id, dgp_version, estimator_id, estimator_version, n, seed, tau_id, config_fingerprint, etc.)

**Q3: Estimator sandboxing**

-   Decision: out of scope for v0.1.x by design (explicitly) per [inst/design/THREADMODEL.md]{.underline}.

-   Action: document “honest estimator assumption”; Airlock + contracts remain the integrity boundary; do not imply adversarial security anywhere.

## **Phase 2 — v0.1.8 Patch Scope (MUST-FIX)**

### **Patch A — Constitutional & Reproducibility Blockers**

**A1) Parallel mismatch (docs + code)**

-   Docs: amend Constitution (Article V) to match gated experimental parallel mode.

-   Code: require experimental_parallel=TRUE when parallel=TRUE.

-   Provenance: record experimental parallel usage in provenance (not science payload).

**A2) Timeout nondeterminism**

-   Include max_runtime (and any runtime guards) in config_fingerprint and in flattened meta.

-   Back-compat requirement: v0.1.8 must not “brick” v0.1.7 pins; resume logic must detect legacy fingerprints and compare with the legacy scheme (or store a fingerprint schema version and accept both deterministically).

**A3) Wide & Shallow enforcement**

-   When outer parallel is enabled (and experimental_parallel=TRUE):

    -   force config\$num_threads \<- 1L for estimator calls (record requested vs effective)

    -   wrap estimator calls in withr::with_envvar(c(OMP_NUM_THREADS=1, MKL_NUM_THREADS=1, OPENBLAS_NUM_THREADS=1, VECLIB_MAXIMUM_THREADS=1))

-   Provenance fields (examples): requested_num_threads, effective_num_threads, thread_caps_env.

### **Patch B — Auditability & Reputation**

**B4) DGP metadata correctness (sidecars)**

-   Fix YAML sidecars that contradict executable behavior (no inference, just truth-in-labeling). Concrete mismatches visible now:

    -   [inst/dgp_meta/synth_qte1.yml]{.underline}: noise: gaussian but code uses Student‑t noise ([R/dgp-synth-qte1.R]{.underline}).

    -   [inst/dgp_meta/synth_heavytail.yml]{.underline}: effect: constant but [τ(X)=1+0.5X1]{.underline} ([R/dgp-synth-heavytail.R]{.underline}).

    -   [inst/dgp_meta/synth_overlap_stressed.yml]{.underline}: effect: constant but [τ(X)=1+0.5X1]{.underline} ([R/dgp-synth-overlap-stressed.R]{.underline}).

    -   [inst/dgp_meta/synth_tilt_mild.yml]{.underline}: effect: constant but [τ(X)=1+0.5X1]{.underline} ([R/dgp-synth-tilt-mild.R]{.underline}).

    -   [inst/dgp_meta/synth_nonlinear_heteroskedastic.yml]{.underline}: effect: heterogeneous but τ(X)≡1 ([R/dgp-synth-nonlinear-heteroskedastic.R]{.underline}).

-   Add explicit executable meta fields **without changing DGP code** (to respect Article VII immutability):

    -   Implement a deterministic lookup cs_dgp_executable_meta(dgp_id, version) used by runner + validator.

    -   Store these fields in run meta/provenance (not in DGP output), and validate YAML against them.

**B5) Estimator version provenance**

-   At runner boundary: enforce result\$meta\$estimator_version == registry version (from cs_get_estimator()), regardless of estimator self-reporting.

-   Preserve estimator self-reported meta\$version only as secondary provenance (e.g., meta\$estimator_reported_version) if needed.

-   Ensure **every estimator** is validated by cs_check_estimator_output() on success (not just “most”).

### **Patch C — RNG Hygiene**

**C6) RNGkind invariant**

-   Goal: “mandated RNGkind always holds” per Constitution Article II §2.1.

-   Governance constraint: avoid editing published DGP implementations unless you ship a regression corpus proving seeded (dgp_id, version, seed) outputs are unchanged (Article VII §7.2 performance-only refactor clause).

-   Patch-safe approach for v0.1.8:

    -   Enforce RNGkind at runner entry points (cs_run_single() / seed/campaign runners) *and* inside the oracle truth generator (already done).

    -   Add tests that DGP generation under the runner always happens under mandated RNGkind; treat direct DGP calls as non-guaranteed unless you explicitly decide to change DGP code + provide a v0.1.7 regression corpus.

## **Phase 3 Deliverables**

### **1) Patch Map (file-by-file)**

**Docs (normative)**

-   [inst/design/CAUSAL_STRESS_CONSTITUTION.md]{.underline}

    -   Edit Article V §5.1 to permit experimental parallel under protocol; explicitly reference Article VI staging/atomic persistence.

    -   Add a short “experimental parallel governance” clause: required flags + provenance capture.

-   [inst/design/THREADMODEL.md]{.underline}

    -   Add one sentence clarifying: “no adversarial sandboxing; parallel gating is governance, not security.”

-   (Optional but recommended) [inst/design/CAUSAL_STRESS_ROADMAP.md]{.underline}

    -   Mark deterministic batching as *deferred* until after v0.1.8 patch closure (to avoid roadmap conflict).

**Runner / orchestration**

-   [R/cs-run-campaign.R]{.underline}, [R/cs-runner.R]{.underline}, [R/cs-run-one-seed.R]{.underline}

    -   Add experimental_parallel gating + loud warning + provenance flags.

    -   Apply Wide & Shallow enforcement when in experimental parallel mode.

    -   Ensure parallel remains default FALSE.

-   [R/cs-fingerprint.R]{.underline}

    -   Include max_runtime and any runtime guards in fingerprint.

    -   Add fingerprint schema versioning and deterministic legacy-compat comparison for v0.1.7 pins.

-   [R/cs-provenance.R]{.underline} + [R/cs-result-to-row.R]{.underline}

    -   Add flattened provenance fields for parallel flags, thread caps, timeouts.

-   [R/cs-scale-helpers.R]{.underline} (or similar internal helper)

    -   Centralize: gating check, envvar wrapper, thread override logic.

**Science payload accessors**

-   New: [R/cs-science-payload.R]{.underline} (or similar)

    -   cs_science_payload(x) canonicalizes: stable ordering, no logs/warnings, no timestamps/runtimes.

    -   cs_provenance(x) returns only provenance.

    -   cs_meta_flatten(x) for analysis tables.

**DGP meta validation**

-   New: [R/cs-dgp-executable-meta.R]{.underline}

    -   cs_dgp_executable_meta(dgp_id, version) returns authoritative fields derived from code/spec (manual mapping).

-   Update sidecars:

    -   [inst/dgp_meta/\*.yml]{.underline} corrections for mismatches listed above.

-   Update validator:

    -   [R/cs-validate-dgp.R]{.underline} / [R/cs-validate-registry.R]{.underline} (wherever you validate) to check YAML claims == executable meta.

**Estimator provenance**

-   [R/cs-runner.R]{.underline} (or cs_extract_estimator_result path)

    -   Overwrite/validate estimator version fields at runner boundary.

-   [R/est-\*.R]{.underline}

    -   Only if required for contract compliance (avoid behavior changes); ensure every estimator returns minimal required meta and is validated.

### **2) Test Plan (testthat)**

**Parallel gating**

-   Test: parallel=TRUE + experimental_parallel=FALSE → error with required message.

-   Test: experimental_parallel=TRUE → loud warning + provenance flag set.

-   Test: parallel=TRUE + persistence → staging required (existing tests may be extended).

**Fingerprint covers max_runtime**

-   Test: same task with max_runtime=Inf vs max_runtime=1 produces different fingerprint (and resume mismatch is explicit, not silent).

-   Test: legacy pins (no fingerprint schema version) are compared deterministically using legacy scheme.

**Wide & Shallow thread cap spy**

-   Register a temporary estimator in tests that asserts:

    -   config\$num_threads == 1L when experimental parallel enabled

    -   env vars are set to 1 within estimator call

-   Run via cs_run_campaign(..., parallel=TRUE, experimental_parallel=TRUE) under future::plan(sequential) (so you test wrappers without depending on OS multiprocess nondeterminism).

**DGP YAML vs executable meta**

-   For each DGP in registry: read YAML ([inst/dgp_meta/\<id\>.yml]{.underline}) and compare to cs_dgp_executable_meta(id, version) for fields like noise, effect, linearity, overlap (as defined).

-   This must not use statistical inference; it’s a label consistency check.

**RNGkind invariant when seed=NULL**

-   Under runner path: call cs_run_single(..., seed=1) and assert RNGkind() equals mandated afterward (and/or inside a wrapper estimator).

-   If you decide to change DGP code later: add a v0.1.7 regression corpus test ensuring seeded outputs unchanged.

### **3) Acceptance Criteria (what “closed” means)**

**A1 Parallel mismatch**

-   Constitution text matches actual behavior.

-   Code refuses parallel unless explicitly opted in (experimental_parallel=TRUE).

-   Every experimental parallel run emits warning and records provenance flags.

**A2 Timeout nondeterminism**

-   max_runtime is part of fingerprint identity (or timeouts forbidden under an explicit “repro mode”).

-   Resume cannot silently accept a mismatched timeout; it must error with explicit mismatch info.

-   v0.1.7 pins remain readable/resumable under deterministic legacy handling.

**A3 Wide & Shallow**

-   In experimental parallel mode: num_threads effectively 1 and envvar caps applied for every estimator call.

-   Provenance records requested vs effective threads.

**B4 DGP metadata correctness**

-   All YAML sidecars agree with executable meta mapping.

-   CI/test fails if a YAML claim drifts from executable meta.

**B5 Estimator version provenance**

-   Runner output always has estimator_version == registry version.

-   Contract validation runs for all estimators on success paths.

**C6 RNG hygiene**

-   Under runner execution path, mandated RNGkind always holds.

-   Any future DGP-code edits require an explicit v0.1.7 regression corpus (or a DGP version bump, which is out of scope for v0.1.8 patch-only).

### **4) Deferred Items List (explicitly out of scope for v0.1.8)**

-   Deterministic randomized batching / “batch_id strategy” / cs_run_batch_internal (roadmap Phase 3).

-   Any new DGPs, DGP version bumps, or estimator additions.

-   Performance experiments/optimizations beyond Wide & Shallow enforcement.

-   Subprocess sandboxing / adversarial estimator isolation.

-   “Kill curves”, campaign dashboards, database adoption.

## **Notes on “don’t invalidate v0.1.7”**

-   Any change to fingerprints *will* affect resume semantics; v0.1.8 must include deterministic legacy handling so v0.1.7 results remain auditable and resumable.

-   Avoid modifying published DGP generator code unless you are willing to ship a regression corpus proving seeded outputs are unchanged (Constitution Article VII §7.2).