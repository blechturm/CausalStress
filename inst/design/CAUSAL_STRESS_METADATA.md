# CausalStress Metadata Specification

**Version:** Draft (for v0.1.3)\
**Status:** Working Draft — to be refined and frozen prior to v0.2.0\
**Author:** CausalStress Core Team\
**Scope:** Defines the metadata captured for each run and persisted via `pins`, including current fields and planned extensions.

---

## 1. Purpose

This document enumerates the metadata attached to CausalStress runs (single seed), how it is stored, and planned extensions. The goal is to provide a single reference for provenance, reproducibility, and performance instrumentation.

---

## 2. Run Object Structure (Rich List)

Each `cs_run_single()` call returns a **rich list** with four top-level entries:

- `att`: ATT-focused metrics
- `qst`: QST metrics (currently `NULL` for ATT-only estimators)
- `boot_draws`: Bootstrap draws (tibble or `NULL`)
- `meta`: Metadata (provenance, governance, timing, fingerprints)

### 2.1 `att` (current)
- `estimate`: Point estimate of ATT.
- `true`: Oracle ATT truth for treated units.
- `error`: `estimate - true`.
- `abs_error`: `abs(error)`.
- `ci_lo`: Bootstrap CI lower bound (NA if no bootstrap or no draws).
- `ci_hi`: Bootstrap CI upper bound (NA if no bootstrap or no draws).
- `boot_covered`: Logical indicating whether `true` lies in `[ci_lo, ci_hi]` (NA if no CI).
- `ci_width`: `ci_hi - ci_lo` (NA if no CI).

### 2.2 `qst` (current)
- `NULL` for ATT-only estimators.
- Planned: tibble with `tau`, `estimate`, `true`, CI fields, coverage.

### 2.3 `boot_draws` (current)
- Tibble with bootstrap draws (columns: `b`, `est_att`) when bootstrap is used.
- `NULL` if bootstrap not requested or no valid draws.

### 2.4 `meta` (current)
- `dgp_id`: DGP identifier.
- `estimator_id`: Estimator identifier.
- `n`: Sample size.
- `seed`: Seed used for this run.
- `oracle`: Logical; TRUE for oracle estimators.
- `supports_qst`: Logical; TRUE if estimator returns QST.
- `estimator_pkgs`: Flattened string of package versions (e.g., `CausalStress=0.1.3;grf=2.5.0`).
- `n_boot_ok`: Number of successful bootstrap replicates.
- `log`: Captured messages/warnings/errors from estimator calls (string or NA).
- `config_fingerprint`: Hash over `(dgp_id, estimator_id, n, seed, bootstrap, B, oracle, estimator_version)`.
- `timestamp`: POSIXct (with jitter) marking run creation time.
- `run_timestamp`: Same as `timestamp` (redundant field for pins metadata alignment).

### 2.5 Planned Meta Extensions (v0.1.3 scope)
- `run_time_total`: End-to-end wall-clock per seed.
- `run_time_dgp`: Time spent in DGP generation.
- `run_time_estimator`: Time spent in estimator call (excluding bootstrap loop).
- `run_time_bootstrap`: Time spent in bootstrap loop.
- `run_time_pin_io`: Optional time spent in pin write/read (if measured).
- `batch_time`: Optional total time per DGP × estimator block (grid/suite-level).

### 2.6 Planned Meta Extensions (post v0.1.3)
- `qst` metrics: coverage, CI width, error fields.
- Expanded fingerprint components (hyperparameters, tau grids, config lists).
- Parallel/manifest identifiers (once parallelism is enabled).

---

## 3. Pin Persistence

- Each run is persisted as a pin with name:
  - `results__dgp={dgp_id}__est={estimator_id}__n={n}__seed={seed}`
- `pins::pin_write()` stores the entire rich result object.
- Pin metadata includes:
  - `dgp_id`, `estimator_id`, `n`, `seed`
  - `timestamp` (mirrors `result$meta$timestamp`)
  - `git_hash`, `session_info`, `design_spec` (where available)
  - Governance fields from the registry (status, version)

Planned: include key timing fields and fingerprints in pin metadata for faster audit queries.

---

## 4. Audit Surface (`cs_audit()`)

`cs_audit()` reads pin metadata and result meta to expose a tabular audit view. Current columns include:
- `pin_name`
- `dgp_id`, `estimator_id`, `n`, `seed`
- `timestamp` (from result meta or pin metadata)
- `git_hash`, `session_info` (if recorded by `cs_pin_write()`)
- Governance fields where available (status/version/design_spec from registry)

Planned additions:
- Timing fields (run_time_*)
- Fingerprint (config_fingerprint)
- Status/rationale from registry for quick filtering

---

## 5. Governance & Registry Fields (for context)

The DGP registry rows include (current):
- `dgp_id`, `type`, `generator`, `version`, `description`
- `status`, `rationale`, `date_status_changed`, `design_spec`

Future: enforce status-driven selection and filtering in audit/collect helpers; surface status/rationale in audit outputs.

---

## 6. Recommended Documentation Targets

- Keep this spec under `inst/design/` as the authoritative metadata reference.
- Add a short vignette on provenance/metadata (how to read audit tables, interpret fingerprints, and timing fields).
- Cross-reference README for high-level disclaimers (experimental DGPs, LLM note) and caching/resume behavior.

---

## 7. Change Control

- This document should be updated whenever new fields are added to `result$meta`, pin metadata, or audit outputs.
- Before freezing in a release, verify alignment with:
  - Constitution (Articles II, VI, VII)
  - Design Specification v0.3.0+
  - Registry invariants and runner output contracts.
