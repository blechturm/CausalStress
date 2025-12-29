# CausalStress 0.1.8

## Patch-Only: Reproducibility & Audit Closure
* Experimental parallel gating: `parallel=TRUE` now requires `experimental_parallel=TRUE`, emits a `causalstress_experimental_parallel` warning once per call, and records provenance flags.
* Fingerprint schema v2: `config_fingerprint_schema=2` added; `max_runtime` is now part of the config fingerprint; legacy v0.1.7 pins remain resumable (with `max_runtime=Inf` only).
* Wide & Shallow in experimental parallel: forces `num_threads=1` and applies thread-cap env vars (OMP/MKL/OpenBLAS/Veclib) with provenance capture.
* DGP sidecar metadata corrected + validated against runner-side executable meta mapping (no DGP generator changes).
* Added accessors: `cs_science_payload()`, `cs_provenance()`, `cs_meta_flatten()`.

# CausalStress 0.1.7

## Scale Readiness & Protocol Hardening
* Enforced "no parallel persistence without staging" protocol (`parallel=TRUE` + `board` requires `staging_dir`).
* Deterministic campaign execution order via optional `campaign_seed` (no ambient `sample()` usage).
* Pin metadata now includes ATT/CI summary fields for pin-meta-first resume checks.
* Stage-and-gather persistence leaves staged files in place on write failures for retry.

# CausalStress 0.1.6

## Major Improvements (Hardening Phase)
* **RNG Locking:** Implemented strict worker-side RNG enforcement (Constitution Article II).
* **Robust Bootstrap:** Added tryCatch protection and a 90% success quality gate for CIs.
* **New Estimators:** Added `bart_att` (BART) and `tmle_att` (TMLE) with soft dependency checks.
* **Validation:** Added `cs_validate_registry()` tool; verified 12 baseline DGPs.

## Oracle & Truth Generation:
* Refactor `cs_get_oracle_qst`: Removed "Shadow Library" of duplicate simulators.
* Implemented "Registry-First" truth generation: Oracle now calls `cs_get_dgp()`.
* Added recursion guard (cache sentinel) to prevent infinite loops (DGP->Oracle->DGP).
* Enforced `ORACLE_SEED <- 99999L` to isolate truth generation from user seeds.

## Registry & Metadata:
* Created YAML sidecars (`inst/dgp_meta/*.yml`) for ALL DGPs (including placebos).
* Refactored `cs_dgp_registry` to read `tags` dynamically from these YAMLs.
* Refactored Gatekeeper to filter placebos via metadata tags, not regex.
* Added `yaml` to Imports.

## Runner & Code Health:
* Extracted `cs_extract_estimator_result` helper to DRY up `cs_run_single`.
* Unified result parsing logic for main execution and bootstrap loops.
* Added `test-integrity-capabilities.R` to prevent Registry/Estimator drift.

## Operational Hygiene:
* Exported `cs_gather_results` for explicit parallel support.
* Renamed `dgp-synth-placebo.R` -> `dgp-synth-placebo-tau0.R` (Constitutional fix).
* Updated .Rbuildignore/.gitignore to exclude sidecar cache artifacts.


# CausalStress 0.1.5

## Scientific Safety & Oracle Refactor
* **Oracle Truth:** Completely refactored `cs_get_oracle_qst` to strictly adhere to the "Single Source of Truth" principle. It now calls the official DGP generators from the Registry (using a reserved seed `99999L`) rather than using a hardcoded "shadow library" of simulators. Implemented a recursion guard to safely handle synthetic DGPs calling the Oracle.
* **Gatekeeper Robustness:** Eliminated brittle "magic string" matching (regexing "placebo" in filenames). The Gatekeeper now identifies placebo DGPs by querying strict `tags` ("placebo") from the Registry.
* **Registry-Sidecar Integration:** The DGP Registry now dynamically reads metadata (tags, stress profiles) from immutable YAML sidecars (`inst/dgp_meta/*.yml`) instead of hardcoded lists.

## Operational Hygiene & API
* **Parallel Staging:** Exported `cs_gather_results()` to the public API to officially support "Stage & Gather" parallel patterns.
* **Build Artifacts:** configured `.Rbuildignore` and `.gitignore` to exclude machine-specific RMarkdown cache directories (`*_cache`, `*_files`) from the package source.
* **Constitutional Compliance:** Renamed `dgp-synth-placebo.R` to `dgp-synth-placebo-tau0.R` (and associated tests) to strictly match the DGP ID.

## Internal Code Health
* **DRY Runner:** Refactored `cs_run_single` to use a unified helper `cs_extract_estimator_result` for parsing estimator outputs, eliminating logic duplication between the main run and the bootstrap loop.
* **Integrity Tests:** Added `test-integrity-capabilities.R` to enforce that estimator outputs (oracle, QST support) match their Registry declarations.

# CausalStress 0.1.5

- Parallel execution: Added `parallel=TRUE` support to `cs_run_seeds` and `cs_run_campaign` with future/furrr, plus optional staging_dir for crash-safe runs.
- Load balancing: Shuffled task ordering and chunked scheduling to reduce idle workers in wide campaigns.
- Staging & resume: Stage-and-gather workflow for pins boards; improved fingerprint hashing now includes estimator config and tau to avoid stale caches.
- Progress: Wired progressr into parallel runs for clearer feedback during campaigns.
- Robustness: Timeout/safety checks in runners; fingerprint mismatch now surfaces clear errors with force/skip overrides.


# CausalStress 0.1.4

- GenGC Integration: Added est_gengc adapter with soft dependency.
- Distributional Intelligence: Runner now bootstraps QST curves, computes pointwise CIs, and enriches outputs.
- Governance: Added cs_summarise_qst and implemented the Gatekeeper 10/10 rule for QST (fails if >10% of quantiles exclude zero).
- Visualization: Updated cs_plot_qst to support confidence bands and truth overlays ("Blue Band").
# CausalStress 0.1.3

### Safety & Suites
-   **Suites Layer:** Introduced `cs_run_suite()` and the internal Suite Registry. Users can now run curated batches (e.g., `suite_id = "placebo"`) with a single command.
-   **Gatekeeper Protocol:** Added `cs_summarise_gatekeeper()` to automate Article IV validation. It applies the 90% coverage rule to detect "unsafe" estimators.
-   **Visualization:** Added `cs_plot_att_error()` and `cs_plot_placebo()` for standardized `ggplot2` reporting.
-   **Instrumentation:** Run results now capture granular timing metadata (`run_time_dgp`, `run_time_est`, `run_time_total`) to identify bottlenecks (e.g., bootstrap overhead).

### Governance & Infrastructure
-   **Article VII Compliance:** Refactored the DGP registry to support immutable, versioned helpers. Added status flags (`stable`, `experimental`) and rationale metadata.
-   **Registry Validation:** Added `cs_validate_dgp_registry()` and `cs_validate_tau_grid()` to enforce constitutional invariants.
-   **Run Fingerprinting:** Implemented `cs_build_config_fingerprint()` to prevent unsafe resumes when simulation settings (e.g., `bootstrap`, `n`) change.
-   **Dependencies:** Added `ggplot2` and `tidyr` to Imports.

# CausalStress 0.1.2

### Core Features
-   **Atomic Persistence:** Enabled crash recovery via `pins`. Results are saved to disk immediately after each seed is generated.
-   **The Airlock:** Introduced `cs_airlock()` to centrally strip forbidden columns (`y0`, `y1`, `p`, `structural_te`) before data reaches an estimator.
-   **Provenance:** Implemented `cs_audit()` to track git hashes, timestamps, and session info for every run.
-   **Tidy Output:** Added `cs_tidy()` and collectors (`cs_collect_att`, `cs_collect_qst`) to flatten rich result objects into analysis-ready tibbles.

### Safety & UX
-   **Resume Guards:** Runners now raise informative errors when `skip_existing = TRUE` encounters a pin computed with mismatched settings (e.g., missing bootstrap CIs).
-   **Documentation:** Added the "From Run to History" vignette and a comprehensive README.
-   **Aliases:** Added `cs_run_campaign()` as an alias for `cs_run_seeds()` for consistency with design docs.

# CausalStress 0.1.0

-   **First MVP:**
    -   Synthetic DGPs: `synth_baseline`, `synth_heavytail`.
    -   Estimators: `oracle_att`, `lm_att`, `ipw_att`, `grf_dr_att`.
    -   Registry system for DGPs and estimators.
    -   Runners: `cs_run_single()`, `cs_run_seeds()`.
    -   Contracts + tests for DGPs, estimators, runners, truth helpers.
