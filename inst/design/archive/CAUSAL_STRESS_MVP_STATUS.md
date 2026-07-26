# CausalStress MVP Implementation Status

_Last updated: 2025-11-28_

## 1. Scope of the MVP (v0.3.0)

In scope for the MVP:

- Truth layer for synthetic benchmarks
  - Canonical tau grid for quantile treatment effects.
  - ATT and QST truth functions based on potential outcomes.
- Synthetic DGPs
  - DGP registry entries (at least one fully specified DGP).
  - Concrete generators that return data + oracle truth.
- Contract layer
  - DGP output contract (synthetic).
  - Estimator output contract.
- Minimal estimator layer
  - At least one simple estimator (oracle or baseline).
- Minimal runner
  - Single DGP × single estimator run with contracts enforced.
  - Placebo / sanity checks on synthetic data.

Out of scope for the MVP (deferred):

- Cross-language Hub & Spoke (“official” Python / Julia spokes).
- Real-world benchmark datasets (LaLonde, IHDP, etc.).
- Full campaign orchestration and reporting.
- Web assets, pkgdown site, and advanced documentation.

Reference: `CAUSALSTRESS_DESIGN_v0.3.0.md`.

---

## 2. Implemented components (MVP)

### 2.1 Truth layer & RNG

**Files**

- `R/cs-constants.R`
  - `cs_tau_oracle`: numeric vector, canonical tau grid (0.01–0.99, step 0.01).
- `R/cs-rng.R`
  - `cs_set_rng(seed = NULL)`: sets RNGkind and (optionally) the global seed.
- `R/cs-truth.R`
  - `cs_true_att(structural_te, w)`: ATT truth on treated units based on structural treatment effects τ(X).
  - `cs_true_qst(y0, y1, w, tau = cs_tau_oracle)`: QST truth based on potential outcomes (Y0, Y1) for treated units, on a given tau grid.

**Status**

- Implemented and tested.
- Tests:
  - `tests/testthat/test-truth-helpers.R`
  - `tests/testthat/test-reproducibility.R`

---

### 2.2 Contract layer

**Files**

- `R/cs-contracts.R`
  - `cs_chk_scalar_numeric(x, arg)` (internal): finite numeric scalar validator.
  - `cs_check_dgp_synthetic(dgp)` (internal):
    - Requires a list with components `df`, `true_att`, `true_qst`, `meta`.
    - `df` must contain `y`, `w`, `y0`, `y1`, `p` without NA.
    - `w` must be numeric/integer with values in {0, 1}.
    - `true_att` must be a finite numeric scalar.
    - `true_qst` must be a tibble with columns `tau`, `value`, with `tau` ⊆ `cs_tau_oracle`.
    - `meta` must be a list with:
      - `dgp_id`: character scalar.
      - `type`: exactly `"synthetic"`.
      - `structural_te`: numeric vector, length nrow(df), no NAs.
    - If `df$structural_te` is present, it must match `meta$structural_te`.
  - `cs_check_estimator_output(res, require_qst = FALSE, tau = NULL)` (internal):
    - Requires a list with components `att`, `qst`, `meta`.
    - `meta$estimator_id`: character scalar.
    - `att` must be a list or data.frame with an `estimate` component/column that is a finite numeric scalar.
    - `qst` rules:
      - If `require_qst = FALSE`: `qst` may be `NULL` or a tibble with `tau`, `value`.
      - If `require_qst = TRUE`: `qst` must be non-NULL tibble with `tau`, `value`; if `tau` is provided, `qst$tau` must be ⊆ `tau`.

**Status**

- Implemented and tested.
- Tests:
  - `tests/testthat/test-dgp-contract.R`
  - `tests/testthat/test-estimator-contract.R`

---

### 2.3 Baseline synthetic DGP

**Files**

- `R/dgp-synth-baseline.R`
  - `dgp_synth_baseline(n, seed = NULL)`:
    - Seed handling: if `seed` is non-NULL, calls `cs_set_rng(seed)`.
    - Covariates:
      - `X1, X2, X3, X4, X5 ~ N(0, 1)` independently.
    - Baseline outcome:
      - `mu0 = 1 + X1 + 0.5 * X2`.
    - Structural treatment effect:
      - `tau = 1 + 0.5 * X1`.
    - Propensity:
      - `p = plogis(0.5 * X1 - 0.5 * X2)`.
    - Treatment:
      - `w ~ Bernoulli(p)`.
    - Noise:
      - `eps0, eps1 ~ N(0, 0.5)` independently.
    - Potential outcomes:
      - `y0 = mu0 + eps0`.
      - `y1 = mu0 + tau + eps1`.
    - Observed outcome:
      - `y = ifelse(w == 1, y1, y0)`.
    - Truth:
      - `true_att = cs_true_att(structural_te = tau, w = w)`.
      - `true_qst = cs_true_qst(y0, y1, w, tau = cs_tau_oracle)`.

    Returns:

    - `df`: tibble with columns  
      `y`, `w`, `y0`, `y1`, `p`, `structural_te`, `X1`, `X2`, `X3`, `X4`, `X5`.
    - `true_att`: numeric scalar.
    - `true_qst`: tibble with columns `tau`, `value`.
    - `meta`: list with fields:
      - `dgp_id = "synth_baseline"`.
      - `type = "synthetic"`.
      - `structural_te = tau`.

**Status**

- Implemented and tested.
- Tests:
  - `tests/testthat/test-dgp-baseline.R`
    - Validity test: `dgp_synth_baseline()` output passes structural expectations.
    - Reproducibility test: same `seed` ⇒ identical `df`, `true_att`, `true_qst`, `meta$structural_te`.
  - `tests/testthat/test-dgp-contract.R`
    - `cs_check_dgp_synthetic()` accepts `dgp_synth_baseline()` output.

---

### 2.4 Runner placeholder

**Files**

- `tests/testthat/test-runner-placebo.R`
  - Currently minimal / placeholder tests to ensure the testing infrastructure for a future runner is wired.

**Status**

- Runner logic itself not yet implemented; only placeholder tests exist.

---

## 3. MVP backlog (NOT implemented yet)

These items are in scope for the MVP but not yet implemented.

### 3.1 Estimator layer

Planned:

- `est_oracle_att()`:
  - Reads `meta$structural_te` and computes ATT on treated units.
  - ATT-only estimator, no QST.
  - Used as a sanity-check estimator for the runner.
  - Must pass `cs_check_estimator_output(require_qst = FALSE)`.

- Future baseline estimators:
  - Simple outcome regression or IPW-based ATT estimator on synthetic data.
  - Conform to estimator contract and registry design.

### 3.2 Estimator registry

Planned:

- A simple lookup mechanism, e.g.:

  - `cs_get_estimator(estimator_id)` → returns a list containing:
    - `fn`: function to call.
    - `meta`: estimator metadata (e.g., `estimator_id`, `supports_qst`, `oracle = TRUE/FALSE`).
  - Initial registry seeded with `"oracle_att"` and a simple baseline estimator.

### 3.3 DGP registry interface

Planned:

- A lookup function, e.g.:

  - `cs_get_dgp(dgp_id)` → returns a generator function or descriptor.
  - Initially supports `"synth_baseline"` only.
  - Later extended with additional synthetic DGPs.

### 3.4 Minimal runner

Planned:

- `cs_run_single(dgp_id, estimator_id, n, seed, ...)`:
  - Uses DGP registry to instantiate a DGP with given `n` and `seed`.
  - Uses estimator registry to run a given estimator on the generated data.
  - Enforces:
    - `cs_check_dgp_synthetic()` on the DGP output.
    - `cs_check_estimator_output()` on the estimator output.
  - Returns a tibble or list with at least:
    - `dgp_id`, `estimator_id`, `n`, `seed`.
    - `true_att`, `est_att`.
    - Possibly error metrics (e.g., bias, absolute error).

- Tests:
  - Single DGP × oracle estimator smoke test.
  - Eventually placebo tests (e.g., W permuted) for robustness.

---

## 4. How to regenerate the technical audit

A separate, machine-generated audit of the codebase lives in:

- `design/CAUSALSTRESS_MVP_AUDIT.md`

Generation script:

- `dev/audit_mvp_state.R`

Usage:

```r
source("dev/audit_mvp_state.R")
# This overwrites design/CAUSALSTRESS_MVP_AUDIT.md
