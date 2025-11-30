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
    -   A minimal **“quickstart” vignette** showing: run, pin, resume, audit, collect, summarise.

------------------------------------------------------------------------

## 9. Edge Case Handling (v0.2.x+)

-   **Non-git environments:**
    -   Warn if `git_hash` is `NA`.
-   **SessionInfo size:**
    -   Consider an opt-out option: `options(causalstress.slim_sessioninfo = TRUE)`
-   **File system scale:**
    -   Optional support for subdirectory **sharding** when thousands of runs are pinned on local filesystems.

------------------------------------------------------------------------

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