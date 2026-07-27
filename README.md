

<!-- README.md is generated from README.qmd. Please edit that file. -->

# CausalStress

> **CausalStress v0.2.0 scientific status** CausalStress has a tested
> execution and evidence contract, but most bundled DGPs have not
> completed scientific validation. Use it for development and governed
> benchmarking, not as evidence that an estimator is scientifically
> valid across the bundled scenarios.

CausalStress is an R framework for benchmarking causal-inference
estimators against governed synthetic data-generating processes (DGPs).
It separates estimator-visible data from runner-owned truth, records
version and provenance metadata, and exposes typed score records for
analysis.

## Current scientific scope

Version 0.2.0 implements scoring for:

- **ATT**: average treatment effect on the treated;
- **ATE**: finite-sample average treatment effect;
- **QST**: quantile shift for the treated.

**CATE is registered but not implemented.** A CATE-only task fails
closed; when CATE is included in a mixed request, it is represented by a
`target_not_implemented` status row. This keeps the intended target
explicit without claiming a working CATE benchmark.

All bundled DGPs are **synthetic**. Real-data DGP support is planned,
but no real-data DGP or public DGP-registration API exists in v0.2.0.

The registry contains 12 DGP IDs and 24 immutable versioned entries. The
current default stable versions are `synth_baseline` and
`synth_heavytail`; the other ten DGP IDs are experimental and produce a
governance warning when selected.

**Heavy-tail boundary.** `synth_heavytail` is intentionally a
no-finite-mean regime. Its `true_att` is the governed finite-sample
structural signal anchor `mean(tau_i : W_i = 1)`, not a conventional
superpopulation mean potential-outcome ATT, which does not exist under
its Cauchy-mixture noise. Run ATT estimators there to diagnose
breakdown, but do not use their mean error, RMSE, coverage, or ranking
for an ATT shootout. Use QST for valid distributional comparisons.

| DGP ID                            | Current status |
|-----------------------------------|----------------|
| `synth_baseline`                  | stable         |
| `synth_heavytail`                 | stable         |
| `synth_placebo_tau0`              | experimental   |
| `synth_qte1`                      | experimental   |
| `synth_nonlinear_heteroskedastic` | experimental   |
| `synth_overlap_stressed`          | experimental   |
| `synth_tilt_mild`                 | experimental   |
| `synth_placebo_nonlinear`         | experimental   |
| `synth_placebo_heavytail`         | experimental   |
| `synth_placebo_tilted`            | experimental   |
| `synth_placebo_kangschafer`       | experimental   |
| `synth_hd_sparse_plm`             | experimental   |

Passing the package contract and validation suite establishes
deterministic execution, schema conformance, and truth separation. It
does not replace human review of a DGP’s scientific interpretation,
historical fidelity, or citations.

## What the framework enforces

- **Truth separation.** Synthetic DGPs return observed data and
  runner-owned structural/distributional truth through separate contract
  fields.
- **Airlock sanitation.** Estimators invoked through the runner do not
  receive `y0`, `y1`, `p`, or `structural_te` unless an explicitly
  governed oracle grant permits a named truth column. Code that bypasses
  the runner is outside this guarantee.
- **Deterministic task RNG.** Each task is seeded at the runner boundary
  under the governed RNG contract.
- **Versioned identity.** Results carry DGP/estimator versions, fit
  identity, score-record identity, score-row identity, truth version,
  and configuration identity.
- **Persistence and resume.** Serial runs can be written to a `pins`
  board. Parallel persistence uses worker staging followed by controlled
  consolidation.

The governing rules and operational contracts live under
[`inst/design/`](inst/design/README.md).

## Installation

CausalStress is currently installed from GitHub:

``` r
# install.packages("pak")
pak::pak("blechturm/CausalStress")
```

## Canonical workflow

`cs_run_single()` returns a structured result list. The typed `scores`
table is the canonical scoring surface; `att` and `qst` remain
compatibility projections.

``` r
library(CausalStress)
library(dplyr)

single <- cs_run_single(
  dgp_id = "synth_baseline",
  estimator_id = "lm_att",
  n = 500,
  seed = 1
)

names(single)
#> [1] "outputs"    "scores"     "att"        "qst"        "boot_draws"
#> [6] "meta"       "provenance"

cs_collect_scores(single) |>
  select(estimand_target_id, metric_id, estimate, truth, error) |>
  knitr::kable(digits = 3)
```

| estimand_target_id | metric_id   | estimate | truth |  error |
|:-------------------|:------------|---------:|------:|-------:|
| att                | point_error |    1.037 | 1.139 | -0.102 |

For repeated benchmarking, run a grid, optionally persist each serial
result, then summarize or audit it:

``` r
library(pins)

board <- pins::board_temp()

runs <- cs_run_grid(
  dgp_ids = "synth_baseline",
  estimator_ids = c("lm_att", "ipw_att"),
  n = 500,
  seeds = 1:3,
  board = board,
  show_progress = FALSE
)

cs_summarise_runs(runs) |>
  select(dgp_id, estimator_id, n_runs, mean_error, mean_abs_error) |>
  knitr::kable(digits = 3)
```

| dgp_id         | estimator_id | n_runs | mean_error | mean_abs_error |
|:---------------|:-------------|-------:|-----------:|---------------:|
| synth_baseline | ipw_att      |      3 |     -0.069 |          0.092 |
| synth_baseline | lm_att       |      3 |      0.002 |          0.072 |

``` r

cs_audit(board) |>
  select(dgp_id, estimator_id, seed, estimator_version) |>
  head() |>
  knitr::kable()
```

| dgp_id         | estimator_id | seed | estimator_version |
|:---------------|:-------------|-----:|:------------------|
| synth_baseline | ipw_att      |    1 | 0.2.0             |
| synth_baseline | ipw_att      |    2 | 0.2.0             |
| synth_baseline | ipw_att      |    3 | 0.2.0             |
| synth_baseline | lm_att       |    1 | 0.2.0             |
| synth_baseline | lm_att       |    2 | 0.2.0             |
| synth_baseline | lm_att       |    3 | 0.2.0             |

For new analysis code, prefer:

``` r
scores <- cs_collect_scores(runs)
```

`cs_collect_att()` and `cs_collect_qst()` are retained as compatibility
projections for existing ATT/QST workflows.

## Estimators

The package registry contains eight estimators. The three core
estimators need only the package’s hard dependencies; the remaining
estimators require their listed optional packages.

| Estimator ID | Target output | Dependency         |
|--------------|---------------|--------------------|
| `oracle_att` | ATT, ATE      | core               |
| `lm_att`     | ATT           | core               |
| `ipw_att`    | ATT           | core               |
| `gengc`      | ATT, QST      | GenGC              |
| `gengc_dr`   | ATT, QST      | GenGC              |
| `grf_dr_att` | ATT           | grf                |
| `bart_att`   | ATT           | bartCause          |
| `tmle_att`   | ATT           | tmle, SuperLearner |

User-defined estimators can be added with `cs_register_estimator()`. See
`?cs_register_estimator` for the current contract. A fuller
estimator-contract vignette is deferred to the dedicated documentation
release.

## Experimental parallel execution

Parallel execution exists, but remains explicitly experimental. It
requires both `parallel = TRUE` and `experimental_parallel = TRUE`,
emits a warning, and records the backend/thread-cap provenance. When a
`board` is supplied, a `staging_dir` is mandatory so workers do not
write directly to pins.

``` r
future::plan(future::multisession, workers = 2)

parallel_runs <- cs_run_grid(
  dgp_ids = "synth_baseline",
  estimator_ids = "lm_att",
  n = 500,
  seeds = 1:20,
  board = board,
  parallel = TRUE,
  experimental_parallel = TRUE,
  staging_dir = tempfile("causalstress-staging-")
)
```

The explicit opt-in is a readiness boundary, not a production-safety
claim.

## Documentation scope

This README and the generated function reference describe the v0.2.0
release surface. A separate documentation release will add the pkgdown
site, reports for every DGP, a canonical-workflow vignette, and contract
vignettes for user-defined estimators and DGPs. The future DGP vignette
does not imply that a public DGP-registration API already exists.

## Citation

To obtain the citation for the installed package version:

``` r
citation("CausalStress")
```
