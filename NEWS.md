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