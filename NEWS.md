# CausalStress 0.1.0

-   First MVP:
    -   Synthetic DGPs: `synth_baseline`, `synth_heavytail`
    -   Estimators: `oracle_att`, `lm_att`, `ipw_att`, `grf_dr_att` (optional)
    -   Registry system for DGPs and estimators
    -   Runners: `cs_run_single()`, `cs_run_seeds()`
    -   Summary: `cs_summarise_runs()`
    -   Contracts + tests for DGPs, estimators, runners, truth helpers

# CausalStress 0.1.2

-   **Documentation:** Added comprehensive `README.Rmd` and "From Run to History" vignette.
-   **UX:** Implemented `cs_audit()` for provenance tracking and `cs_tidy()` for unified output.
-   **Resilience:** Enabled atomic persistence and crash recovery via `pins`.
-   **Fixes:** Silenced console spam from runners and added progress bars.# CausalStress 0.1.2

### Major Improvements

-   Added **bootstrap-resume guard**: the runner now refuses to reuse cached results if they were generated without bootstrap CIs but the user requests `bootstrap = TRUE`. Prevents silent reuse of incompatible pins.

-   Introduced **`cs_airlock()`**, a centralized helper for dropping forbidden columns (`y0`, `y1`, `p`, `structural_te`) and attributes, hardening the constitutional “Airlock” rule. Integrated in all runner paths.

-   Added **`cs_collect_att()`** and **`cs_collect_qst()`**, lightweight collectors for ATT/QST fields, improving ergonomics when analyzing large campaigns.

### Runner & Resume Behavior

-   Runners now raise a clear error when `skip_existing = TRUE` encounters a pin computed under mismatched bootstrap settings.
-   Added full resume tests (including spy estimators) verifying Airlock behavior even after pins reload.

### Documentation

-   README updated to clarify that **v0.1.x is serial-only**, with parallelism planned for v0.2.0.
-   Added clearer examples for bootstrap usage and tidy collectors.
-   Constitution updated to Version **1.7.2**, including revised Gatekeeper rules and prohibitions.

### Developer Experience

-   Added alias `cs_run_campaign()` for design doc consistency.
-   Expanded internal triage roadmap with planned improvements (config fingerprint, expanded provenance, parallel safety).
-   400+ tests passing, ensuring that refactors are fully covered.

## v0.1.3 – Safety & Suites (Planned)

**Status:** Planned (Alpha line, still serial-only)

### Goals

-   Tighten alignment between code and Constitutional / design specs.
-   Reduce `skip_existing` footguns via basic run fingerprinting.
-   Provide minimal, official support for test suites and basic plots.
-   Clarify documentation around experimental DGPs and LLM usage.
-   Record runtime metadata (DGP × estimator × bootstrap) in run results for
    easier provenance and performance inspection.
-   Capture additional timings: DGP generation vs estimator time, bootstrap loop
    overhead, and (optional) pin I/O / batch timings for grid/suite blocks.
-   Add metadata documentation describing all fields written to results/pins
    (runtime, fingerprints, governance fields). Likely location: inst/design/
    or a vignette dedicated to provenance/metadata.

### Planned Changes (In Scope)

1.  **Tau Grid & Registry Validation**
    -   Add `cs_validate_tau_grid()` and call it in relevant truth/DGP paths.
    -   Implement `cs_validate_dgp_registry()` to check schema and basic invariants.
    -   Add tests ensuring the DGP registry is Constitution-compliant.
2.  **Run Configuration Fingerprint (v1, Minimal)**
    -   Implement `cs_build_config_fingerprint()` capturing:
        -   `dgp_id`, `estimator_id`, `n`, `seed`, `bootstrap`, `B`, `oracle`, and (if present) `estimator_version`.
    -   Store `meta$config_fingerprint` in run metadata.
    -   On resume with `skip_existing = TRUE`, error on fingerprint mismatch with a clear, user-facing message.
3.  **Suite Registry & Runner**
    -   Implement `cs_get_suite(suite_id)` returning DGP/estimator sets.
    -   Implement `cs_run_suite(suite_id, ...)` as a thin wrapper around `cs_run_grid()`.
    -   Provide at least:
        -   `"placebo"` suite (all sharp-null DGPs + core estimators).
        -   `"stress_basic"` suite (baseline, heavytail, overlap-stressed).
4.  **Minimal Plot Helpers**
    -   Add `cs_plot_att_error()` and `cs_plot_placebo()` helpers.
    -   Ensure they accept `cs_tidy()` output and return ggplot objects.
    -   Add tests that they run without error on small example runs.
5.  **Documentation Updates**
    -   Update README:
        -   Explicitly separate **validated** vs **experimental** DGPs.
        -   Add DGP experimental disclaimer: no publications based on unvalidated DGPs.
        -   Add short section on LLM-assisted development and human-governed validation.
        -   Add a brief explanation of `skip_existing` + configuration fingerprint behavior.
    -   Optionally update or add a short vignette:
        -   “Running Suites and Interpreting ATT Scorecards”.
6.  **Article VII Compliance & Governance Refactor**
    -   Introduced immutable, versioned DGP helpers and upgraded the DGP registry with governance metadata (status, rationale, design_spec, version/date).

    -   Added canonical DGP accessor cs_get_dgp() with semver-based selection, registry-only dispatch, and loud warnings for non‑stable/invalidated entries.

    -   Strengthened registry validation and tau-grid checks to enforce constitutional invariants.

    -   Refactored runners to resolve DGPs exclusively via the registry, propagate governance warnings once per call, and mark validated vs. experimental DGPs transparently.

    -   Centralized airlock and persistence safeguards to prevent oracle leakage and resume footguns, preserving reproducibility and legal alignment.

### Explicitly Deferred to v0.2.0+

-   Parallel runner and manifest-based parallel safety.
-   QST bootstrap and advanced Article IV features.
-   Rich plotting utilities and dashboards.
-   Full configuration fingerprint (all hyperparameters, tau grids, etc.).
-   Manual, citation-backed DGP validation vignettes.
