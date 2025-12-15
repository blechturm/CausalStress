# CausalStress Roadmap

*(Updated with post-v0.1.2 enhancements and future nice-to-haves)*

This file documents planned improvements that **do not** block v0.1.2 and are therefore scheduled for **v0.2.x and later**. These items improve robustness, usability, and scientific hygiene, but are not required for the MVP.

------------------------------------------------------------------------

## 1. Run Configuration Fingerprint (Planned for v0.2.x+)

### Current behavior (v0.1.x–v0.1.2)

-   `skip_existing = TRUE` checks only for the existence of a matching pin for `(dgp_id, estimator_id, n, seed, oracle)`.
-   It does **not** validate whether pinned results were generated with the same:
    -   bootstrap settings (`bootstrap`, `B`),
    -   estimator hyperparameters,
    -   configuration options,
    -   tau grid,
    -   DGP version,
    -   estimator version.
-   v0.1.2 adds only a **minimal guard**: if the user requests bootstrap but the cached run has no CIs, the runner errors.

### Planned improvement

Introduce a **run configuration fingerprint**:

-   Every run is associated with a **hash** that encodes all relevant run settings.
-   Stored inside the pinned object as `meta$config_fingerprint`.
-   On resume with `skip_existing = TRUE`, the runner:
    -   recomputes the fingerprint for the *requested* run,
    -   compares it to the stored fingerprint.

**Outcome:**

-   If fingerprints match → **safe reuse**
-   If fingerprints differ → **hard error** with a human-readable explanation

### Rationale

-   Prevents accidental reuse of incompatible runs.
-   Strengthens reproducibility guarantees.
-   Keeps resume workflow fast while scientifically safe.

``` r
# (Sketch only)
cs_build_config_fingerprint <- function(dgp_id, estimator_id, n, seed,
                                        bootstrap, B, config) {
  digest::digest(list(
    dgp_id       = dgp_id,
    estimator_id = estimator_id,
    n            = n,
    seed         = seed,
    bootstrap    = bootstrap,
    B            = B,
    config       = config  # possibly filtered/subset
  ))
}
```

------------------------------------------------------------------------

## 2. Parallel Execution Safety (v0.2.0+)

-   Not needed in v0.1.x (**serial-only**), but required for proper parallelism.
-   **Planned: Stage-and-Gather pattern:**
    -   Workers compute locally → controller serializes pins (avoids parallel writes and OS race conditions).
-   Deterministic handling of:
    -   RNG,
    -   `progressr` handlers,
    -   worker-level logs.

------------------------------------------------------------------------

## 3. Enhanced Progress Handling (v0.1.3 or v0.2.x)

-   Add explicit documentation for:

``` r
library(progressr)
handlers(global = TRUE)

with_progress({
  cs_run_grid(...)
})
```

-   Optional: `cs_with_progress(expr)` convenience wrapper.

-   Campaign probes (post-v0.2):
    -   `campaign_test()`: run small, downsampled subsets of planned grids to validate configs without heavy cost.
    -   `campaign_cost_estimator()`: use recorded run_time meta (DGP / estimator / bootstrap, pin I/O) to estimate total campaign cost on current hardware.

------------------------------------------------------------------------

## 4. Manifest / Provenance Helpers (v0.2.x+)

-   Extend provenance tooling with:
    -   **Manifest view**: aggregated metadata for quick inspection:
        -   counts by DGP,
        -   counts by estimator,
        -   counts by git hash,
        -   date ranges.
-   Possibly: `cs_manifest(board)` returning a `tibble` summarizing all runs pinned on a board.

------------------------------------------------------------------------

## 5. Estimator Registry Enhancements (v0.2.x)

-   Enforce explicit `capabilities = c("att", "qst")` fields for all estimators.
    -   Helps with future multi-output estimators.
-   Provide a documented template for user estimators:

``` r
cs_register_estimator(
  id = "...",
  generator = my_fn,
  version = "0.1.0",
  capabilities = c("att")
)
```

-   Encourage user estimators to declare `ci_type` explicitly.

------------------------------------------------------------------------

## 6. DGP / Estimator Version Compatibility Checks (v0.2.x+)

-   Extend the fingerprint system or implement lightweight checks to ensure:
    -   Estimators are not resumed across incompatible `estimator_versions`.
    -   DGPs are not resumed when DGP logic changes (enforced by version bumps in Constitution).

------------------------------------------------------------------------

## 7. Truth & Oracle System Enhancements (v0.2.x)

-   Document `options(causalstress.N_oracle = ...)` as a supported override.
-   Add helper: `cs_show_oracle_cache()` to list stored oracles for debugging.
-   Possibly allow oracle caching to disk if users run many campaigns.

------------------------------------------------------------------------

## 8. Documentation & UX Improvements (v0.1.3–v0.2.x)

-   Planned additions:
    -   Better explanation of:
        -   progress handlers,
        -   persistence model,
        -   airlock behavior,
        -   how `skip_existing` interacts with run config fingerprint,
        -   recommended workflows for large campaigns.
    -   A minimal **"quickstart" vignette** showing: run, pin, resume, audit, collect, summarise.
-   Planned metadata/provenance doc:
    -   Document all meta fields written to runs/pins (fingerprints, timing, governance fields, timestamps)
    -   Location: `inst/design/CAUSAL_STRESS_METADATA.md` (authoritative) plus a short vignette.

------------------------------------------------------------------------

## 9. Edge Case Handling (v0.2.x+)

-   **Non-git environments:**
    -   Warn if `git_hash` is `NA`.
-   **SessionInfo size:**
    -   Consider an opt-out option: `options(causalstress.slim_sessioninfo = TRUE)`
-   **File system scale:**
    -   Optional support for subdirectory **sharding** when thousands of runs are pinned on local filesystems.

------------------------------------------------------------------------

## 10. Literature Canon Integration (GRF Porting)

**Status:** Planned / To Be Determined  
**Priority:** Medium (Post-v0.2.0 Infrastructure)

**Context:** The `grf` package documentation and associated literature (e.g., Wager & Athey 2018, Nie & Wager 2017) rely on a specific set of synthetic data setups to demonstrate heterogeneous treatment effect (HTE) estimation. These setups have become de facto standards ("canon") in the field. To fulfill the CausalStress mission of being a "universal scoreboard", the framework must include these standard benchmarks alongside its own stress tests.

### The Challenge: Architectural & Constitutional Alignment

Porting these DGPs is not merely a copy-paste exercise. The `grf` examples are typically implemented as ad-hoc scripts. Integrating them requires solving specific Constitutional challenges:

1.  **Implicit Truth:** Truth in these scripts is often defined procedurally within the simulation loop. CausalStress requires truth to be a distinct, structural object adhering to the **Two-Tier Truth Contract** (Article I).
2.  **RNG Dependencies:** Ad-hoc scripts may rely on specific R versions or RNG states. CausalStress DGPs must follow the strict "Frozen Logic" rules (Article II) to ensure long-term reproducibility.
3.  **Variable Definitions:** We must ensure variables are explicitly separated into "structural truth" and "noise," satisfying the framework's strict data contracts.

### Key Decision Points

* **Adaptation Strategy:**
    * *Re-implementation (Recommended):* We will likely re-implement the exact logic of `grf` setups inside `CausalStress` DGP functions. This ensures immutability, air-gapped correctness, and independence from external package changes.
* **Truth Definitions:**
    * We must rigorously derive the **Structural ATT** and **Oracle QST** for these setups.
    * *Evaluation:* Some `grf` setups focus purely on CATE (Conditional Average Treatment Effect). We need to evaluate if they provide meaningful stress tests for *population-level* estimands (ATT/QST) or if they strictly serve as HTE benchmarks.
* **Naming Convention:**
    * We need a namespace strategy (e.g., `synth_grf_wager2018_setupA`) to clearly distinguish "Literature Canon" from "CausalStress Native" DGPs.

### Action Plan

1.  **Audit:** Review simulation scripts from Wager & Athey (2018) and related papers to identify the 3-5 most "canonical" setups.
2.  **Validation:** For each candidate:
    * Derive closed-form $\tau(X)$.
    * Confirm compatibility with CausalStress noise models.
3.  **Implementation:**
    * Create `R/dgp-synth-grf-canon.R`.
    * Implement immutable versions.
    * Generate "Sidecar" documentation (`.Rmd`) proving the implementation matches the statistical properties of the original literature.


## Summary

| Status | Feature | Planned Version |
|:---|:---|:---|
| **Completed in v0.1.2** | Airlock helper + hard tests | v0.1.2 |
|  | Pins/resume bootstrap fingerprint guard | v0.1.2 |
|  | Constitution & README clarifications | v0.1.2 |
|  | ATT/QST collectors | v0.1.2 |
|  | Spy tests for pins/resume | v0.1.2 |
|  | Robust resume behavior | v0.1.2 |
| **Planned for Future** | Full configuration fingerprints | v0.2.x+ |
|  | Parallel safe writes | v0.2.x |
|  | Manifest/provenance summary helpers | v0.2.x+ |
|  | Explicit estimator capabilities | v0.2.x |
|  | Oracle system documentation improvements | v0.2.x |
|  | Enhanced UX (progress, persistence, versioning) | v0.1.3–v0.2.x |
|  | Git/sessionInfo edge-case handling | v0.2.x+ |
|  | Optional deterministic sharding for huge campaigns | v0.2.x+ |

This roadmap is a living document—items may migrate between releases as capabilities and requirements evolve.


## Roadmap Note: Sensitivity Analysis via Unobserved Confounding (Proposed Amendment)

**Status:** Parked / Planned  
**Priority:** Medium (Post-MVP, Pre-Thesis Consolidation)  
**Related Work:** Chernozhukov et al. (2025), *A General Sensitivity Analysis Framework for Causal Inference*, arXiv:2510.09109

---

### Motivation

`CausalStress` currently performs **systematic stress testing of causal estimators** along multiple orthogonal axes:

- outcome distributional pathologies (heavy tails, heteroskedasticity),
- overlap violations,
- model misspecification (nonlinearity, sparsity),
- estimand choice (ATT vs QST / QTE),
- placebo and falsification regimes.

One major axis is currently **not explicitly represented**:

> **Sensitivity to unobserved confounding.**

Chernozhukov et al. (2025) formalize sensitivity analysis as robustness to latent confounding using partial-identification and variance-explained (R²) bounds. While their framework is algebraic and identification-focused, it highlights a conceptual gap in `CausalStress`:

- `CausalStress` stress-tests **estimators and estimands** under known DGP violations.
- Classical sensitivity analysis stress-tests **identification assumptions** under hidden confounding.

These approaches are complementary but not substitutes.

---

### Conceptual Alignment with CausalStress

The sensitivity framework of Chernozhukov et al. answers:

> “How strong would unobserved confounding need to be to explain away an estimated effect?”

`CausalStress` instead asks:

> “Which estimators fail first, and how, as assumptions are progressively violated?”

To remain aligned with the `CausalStress` Constitution (immutability, simulation-first validation, estimator-agnostic design), **sensitivity analysis should be implemented as a DGP stress axis**, not as a post-estimation correction or bound.

---

### Proposed Constitutional Amendment (High-Level)

**Proposed Amendment:**  
Introduce **Unobserved Confounding** as a first-class *stress axis* in `CausalStress`.

Sensitivity is operationalized via **controlled synthetic violations** of ignorability, not via analytical bounds. Robustness is evaluated empirically using the same philosophy as existing stress tests (heavy tails, overlap collapse).

---

### Proposed New DGP Class: Latent Confounding Stress

Add a new family of immutable DGPs, e.g.:

- `synth_hidden_confounding_mild`
- `synth_hidden_confounding_moderate`
- `synth_hidden_confounding_severe`

or fixed-parameter variants:

- `synth_hidden_confounding_rho_025`
- `synth_hidden_confounding_rho_050`
- `synth_hidden_confounding_rho_075`

**Core design:**

- Introduce a latent variable \( U \),
- \( U \) affects both treatment assignment \( W \) and outcome \( Y \),
- \( U \) is *excluded* from observed covariates,
- Confounding strength is governed by a single scalar parameter,
- All other DGP components remain unchanged.

This mirrors existing stress axes:
- heavy tails → moment instability,
- overlap stress → support violations,
- latent confounding → ignorability violations.

---

### Evaluation Protocol (Planned)

Robustness will be evaluated using **kill curves**, not bounds:

- x-axis: confounding strength
- y-axis: bias, RMSE, coverage
- estimands evaluated separately (ATT vs QST / QTE)

This enables questions such as:

- At what confounding strength does ATT collapse?
- Do quantile-based estimands (median / IQR QST) remain stable longer than means?
- Do flexible learners degrade gracefully or abruptly?

This framework naturally extends to:
- nonlinear estimators,
- distributional estimands,
- heavy-tailed outcome regimes where classical sensitivity bounds are ill-defined.

---

### Explicit Non-Goals

This amendment will **not**:

- reimplement R²-based sensitivity bounds,
- claim partial-identification guarantees,
- replace formal sensitivity analysis frameworks,
- modify existing estimators.

The goal is **comparative stress testing**, not identification theory.

---

### Strategic Value

- Extends `CausalStress` into a **multi-axis robustness framework**:
  Identification × Estimation × Estimand × Distribution.
- Complements algebraic sensitivity methods (e.g. Chernozhukov et al.).
- Supports QST/QTE, where sensitivity theory is underdeveloped.
- Strengthens the thesis narrative around *robust causal estimands under realistic data pathologies*.

---

### Conclusion

This amendment is conceptually justified, constitutionally compatible, and strategically valuable — but non-essential for the MVP. Parking it as a roadmap item is appropriate.

---

### Reference

Chernozhukov, V., Cinelli, C., Newey, W., Sharma, A., & Syrgkanis, V. (2025).  
**A General Sensitivity Analysis Framework for Causal Inference.**  
*arXiv preprint arXiv:2510.09109*.
