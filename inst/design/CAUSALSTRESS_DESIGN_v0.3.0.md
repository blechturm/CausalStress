# CausalStress Design Document

**Version:** 0.3.0 (MVP)\
**Status:** Frozen for MVP\
**Author:** Max Thomasberger\
**Date:** 2025-05-27

------------------------------------------------------------------------

## 1. Overview

CausalStress is a standardized benchmarking framework for causal inference estimators. It provides:

-   A registry of data-generating processes (DGPs) with ground truth
-   A common interface for estimators (ATT, QST)
-   A runner that orchestrates experiments with optional bootstrap inference
-   Named test suites for common validation scenarios
-   Artifact storage via `pins`
-   Tidy outputs for analysis and visualization

**Terminology:**

-   **ATT**: Average Treatment Effect on the Treated
-   **QST**: Quantile Shift due to Treatment — the difference between quantiles of the observed treated distribution and the counterfactual control distribution. This is the estimand targeted by GenGC; it describes the *effect on the distribution*, not the *distribution of effects*.

**Design principles:**

1.  Tidyverse-native (tibbles, pipes, list-columns)
2.  Estimator-agnostic (any method that meets the contract)
3.  Reproducible (pins, seeds, session info, explicit versioning)
4.  Minimal viable scope (defer complexity)

------------------------------------------------------------------------

## 2. Core Contracts

### 2.1 DGP Contract

A DGP is a function that returns simulated data with ground truth.

``` r
dgp_fn <- function(N = 2000, seed = NULL, ...) {
  if (!is.null(seed)) set.seed(seed)
  
  list(
    df = tibble(
      y  = numeric(N),
      w  = integer(N),
      y0 = numeric(N),
      y1 = numeric(N),
      x1 = ...,
      x2 = ...,
      ...
    ),
    
    true_att = numeric(1),
    
    true_qst = tibble(
      tau   = seq(0.01, 0.99, 0.01),
      value = numeric(99)
    ),
    
    meta = list(
      dgp_id  = "synth_heavytail",
      version = "1.0.0",
      type    = "synthetic",
      params  = list(N = N, ...)
    )
  )
}
```

**Required fields:**

| Field | Type | Description |
|-------------------|------------------|-----------------------------------|
| `df` | tibble | Must contain `y`, `w`, covariates. |
| `true_att` | numeric(1) or `NA_real_` | Ground truth ATT. NA for real data. |
| `true_qst` | tibble(tau, value) or `NULL` | Ground truth QST at percentiles. NULL if unknown. |
| `meta$dgp_id` | character | Unique identifier |
| `meta$version` | character | Semantic version (e.g., "1.0.0") |
| `meta$type` | character | One of: `"synthetic"`, `"real"` |
| `meta$params` | list | DGP parameters for reproducibility |

**Notes (aligned with CausalStress Constitution v1.7.0):**

- **Synthetic DGPs (`meta$type = "synthetic"`):**  
  MUST provide the potential outcomes `y0` and `y1` in `df`,  
  and MUST include `meta$structural_te` (the structural treatment effect τ(X) at the unit level).  
  Synthetic DGPs also MUST supply `true_att` and `true_qst`.

- **Real DGPs (`meta$type = "real"`):**  
  MUST NOT include `y0`, `y1`, or `p` in `df`.  
  They MUST set `true_att = NA_real_` and `true_qst = NULL`.  
  Only observational covariates and observed outcomes belong in `df`.
  For real DGPs, `meta$structural_te` must be NULL.

- **Tau grid / QST truth:**  
  For **synthetic DGPs**, `true_qst` MUST be provided on the **canonical tau grid**  
  ```r
  tau_oracle <- seq(0.01, 0.99, 0.01) 

  Estimators may request any user-specified tau vector, but truth is always defined on this fixed grid. 
  cs_validate_tau() checks that requested tau values are a subset of the canonical grid.

------------------------------------------------------------------------

### 2.2 Estimator Contract

An estimator is a function that takes data and returns causal estimates.

``` r
estimator_fn <- function(df, tau = seq(0.05, 0.95, 0.05), config = list()) {
  
  list(
    att = numeric(1),
    
    qst = tibble(
      tau      = tau,
      estimate = numeric(length(tau))
    ),
    
    cf = list(
      y0_draws = matrix(nrow = nrow(df), ncol = M)  # nrow(df) units × M draws
      # M comes from config$n_draws (e.g., config$n_draws %||% 100)
    ),
    
    meta = list(
      estimator_id  = "GenGC",
      version       = "0.4.0",
      capabilities  = c("att", "qst"),
      target_level  = "population",
      config        = config,
      warnings      = character(),
      errors        = character()
    )
  )
}
```

**Required fields:**

| Field | Type | Description |
|-------------------|------------------|-----------------------------------|
| `att` | numeric(1) | ATT estimate. Required for all estimators. |
| `qst` | tibble(tau, estimate) or `NULL` | QST curve. NULL if estimator doesn't support. |
| `cf` | list or `NULL` | Counterfactual draws. Optional, for diagnostics. |
| `meta$estimator_id` | character | Unique identifier |
| `meta$version` | character | Semantic version (e.g., "0.4.0") |
| `meta$capabilities` | character vector | Subset of `c("att", "qst")` |
| `meta$target_level` | character | `"population"` (MVP). Future: `"unit"` for CATE. |
| `meta$config` | list | Estimator-specific settings |
| `meta$warnings` | character vector | Non-fatal issues encountered |
| `meta$errors` | character vector | Errors (if estimator recovered) |

**Notes:**

-   `tau` is passed in by user/runner. Estimator computes at those quantiles exactly.
-   **Tau grid constraint:** Estimators must return `qst` with the same tau grid that was passed in. All bootstrap replicates must use the same grid.
-   **ATT requirement (MVP):** All estimators must return a scalar `att`. Pure QST-only estimators (no ATT) are not supported in MVP.
-   **QST NULL rule (HARD):** If estimator does not support QST, it MUST set `qst = NULL` (not an empty tibble, not NA-filled rows). This ensures `cs_collect_qst()` works correctly.
-   ATT-only estimators (e.g., GRF-DR): `qst = NULL`, `cf = NULL`, `capabilities = c("att")`.
-   `cf$y0_draws`: matrix with `nrow(df)` rows (units) and M columns (Monte Carlo draws). M typically comes from `config$n_draws`.
-   **Thread safety:** Estimators wrapping `ranger` or `grf` must respect `config$num_threads` (see Section 4.5).
-   Future extensions may support `target_level = "unit"` (CATE/CQTE) with a different output schema.

- **Data access constraints (Constitution §3.1):**  
  Estimators MUST treat `y0`, `y1`, and `p` (true propensity) as inaccessible, even if
  these columns are present in `df`.  
  If an estimator is intentionally Oracle-based (e.g., uses `p`), it MUST be enabled
  via an explicit configuration flag (for example `config$use_true_propensity = TRUE`)
  and SHOULD set `meta$oracle <- TRUE` while documenting which oracle quantities it uses.

- **Confidence interval metadata:**  
  If an estimator reports confidence intervals for ATT or QST, it MUST specify
  the method in `meta$ci_type`, using values such as `"bootstrap"`, `"asymptotic"`,
  `"bayesian"`, `"jackknife"`, or another clearly documented label.  
  Gatekeeper placebo tests primarily act on bootstrap CIs but other types are allowed
  as long as they are labeled.

- **Side-channel prohibition:**  
  Estimators MUST NOT read pins, files, or global state (such as external seed logs)
  to infer truths or manipulate coverage. They may only use the inputs `df`, `tau`,
  and `config` passed into the estimator.

------------------------------------------------------------------------

### 2.3 Runner Output Contract

The runner calls DGP + estimator, optionally bootstraps, computes metrics.

``` r
list(
  att = list(
    estimate = numeric(1),
    ci_lo    = numeric(1),
    ci_hi    = numeric(1),
    true     = numeric(1),
    bias     = numeric(1),
    covered  = logical(1)
  ),
  
  qst = tibble(
    tau, estimate, ci_lo, ci_hi, true, bias, covered
  ),
  
  boot_draws = tibble(
    boot_id = integer(),
    att     = numeric(),
    qst     = list()
  ),
  
  meta = list(
    # Nested original metadata (full provenance)
    dgp       = list(),
    estimator = list(),
    
    # Flat copies for convenience
    dgp_id       = character(),
    estimator_id = character(),
    seed         = integer(),
    N            = integer(),
    B            = integer(),
    tau_grid     = numeric(),
    timestamp    = POSIXct(),
    
    # Status
    success      = logical(),
    n_boot_ok    = integer(),
    warnings     = character(),
    errors       = character()
  )
)
```

**Notes:**

-   `meta$dgp`: Full copy of `dgp$meta` from the DGP.
-   `meta$estimator`: Full copy of `est$meta` from the estimator.
-   **Flat copy requirements (HARD):** `meta$dgp_id` must be a scalar character identical to `meta$dgp$dgp_id`. `meta$estimator_id` must be a scalar character identical to `meta$estimator$estimator_id`. These are required for `cs_collect_*()` helpers to work.
-   **Success semantics:** `success = TRUE` iff the estimator returned without error AND (if bootstrap requested) at least one valid bootstrap replicate was obtained. If `bootstrap = FALSE`, success requires only that the estimator did not error.
-   `n_boot_ok`: Actual number of successful bootstrap replicates (may be \< B if some failed). Set to `NA` if `bootstrap = FALSE`.
-   **Real data handling:** If `true` is NA/NULL, then `bias = NA` and `covered = NA`. Columns are always present (never omitted) to simplify downstream code.
-   If estimator doesn't support QST, `qst = NULL`.
-   `boot_draws` stored for diagnostics; can be dropped to save space.

------------------------------------------------------------------------

## 3. Registries

### 3.1 DGP Registry

``` r
get_dgp_registry <- function() {
  list(
    synth_clean_good_overlap = dgp_synth_clean_good_overlap,
    synth_heavytail          = dgp_synth_heavytail,
    synth_overlap_stressed   = dgp_synth_overlap_stressed,
    synth_qte1               = dgp_synth_qte1,
    synth_placebo_tau0       = dgp_synth_placebo_tau0,
    synth_placebo_nonlinear  = dgp_synth_placebo_nonlinear,
    synth_placebo_heavytail  = dgp_synth_placebo_heavytail,
    synth_placebo_tilted     = dgp_synth_placebo_tilted,
    ...
  )
}

dgp_registry_table <- function() {
  tibble(
    dgp_id      = character(),
    version     = character(),
    type        = character(),
    description = character()
  )
}
```

### 3.2 Estimator Registry

``` r
get_estimator_registry <- function() {
  list(
    GenGC       = estimator_gengc,
    GRF_DR      = estimator_grf_dr,
    AIPW_linear = estimator_aipw_linear
  )
}

estimator_registry_table <- function() {
  tibble(
    estimator_id = character(),
    version      = character(),
    capabilities = list(),
    description  = character()
  )
}
```

### 3.2.1 Helper: Simple Estimator Wrapper
To lower the barrier to entry, `cs_wrap_att_only` converts simple functions into compliant estimators.

```r
cs_wrap_att_only <- function(fn, estimator_id) {
  # fn must be function(df, ...) -> numeric(1)
  function(df, tau, config) {
    list(
      att = fn(df, config),
      qst = NULL,
      meta = list(estimator_id = estimator_id, capabilities = "att")
    )
  }
}
```

### 3.3 Test Suite Registry

Named collections of DGPs for common validation scenarios.

``` r
get_suite_registry <- function() {
  list(
    placebo = c(
      "synth_placebo_tau0",
      "synth_placebo_nonlinear",
      "synth_placebo_heavytail",
      "synth_placebo_tilted"
    ),
    
    heavytail = c(
      "synth_heavytail",
      "synth_placebo_heavytail"
    ),
    
    signal = c(
      "synth_clean_good_overlap",
      "synth_qte1",
      "synth_nonlinear_heteroskedastic"
    ),
    
    stress = c(
      "synth_overlap_stressed",
      "synth_tilt_mild"
    ),
    
    all = names(get_dgp_registry())
  )
}

cs_get_suite <- function(suite_id) {
  suites <- get_suite_registry()
  
  if (!suite_id %in% names(suites)) {
    stop(
      "Unknown suite: ", suite_id, 
      ". Available: ", paste(names(suites), collapse = ", ")
    )
  }
  
  suites[[suite_id]]
}

cs_list_suites <- function() {
  suites <- get_suite_registry()
  tibble(
    suite_id    = names(suites),
    n_dgps      = map_int(suites, length),
    dgp_ids     = map(suites, identity)
  )
}
```

**Built-in suites:**

| Suite ID    | Purpose                             | DGPs                      |
|-------------|-------------------------------------|---------------------------|
| `placebo`   | Validate no false positives (τ = 0) | 4 placebo DGPs            |
| `heavytail` | Test robustness to heavy tails      | 2 heavy-tail DGPs         |
| `signal`    | Validate signal recovery            | 3 DGPs with known effects |
| `stress`    | Test overlap/propensity edge cases  | 2 stressed DGPs           |
| `all`       | Full benchmark                      | All registered DGPs       |

------------------------------------------------------------------------

## 4. Runner API

### 4.1 Single Run

``` r
cs_run_single <- function(
  dgp_id,
  estimator_id,
  seed,
  mode         = c("certified", "draft"), # <--- NEW ARGUMENT
  N            = 2000,
  tau          = seq(0.05, 0.95, 0.05),
  bootstrap    = FALSE,
  B            = 200,
  store_data   = FALSE,
  config       = list(),
  dgp_registry = get_dgp_registry(),
  est_registry = get_estimator_registry(),
  board        = NULL
) {


  # 0. Handle Draft Mode (Speed)
  if (mode == "draft") {
    bootstrap <- FALSE
    B <- 0 
  }
  # 1. Generate data
  dgp <- dgp_registry[[dgp_id]](N = N, seed = seed)
  
  # 2. Validate tau grid (Fuzzy Match)
  if (!is.null(dgp$true_qst)) {
    # Check if all requested taus exist in truth (within tolerance)
    valid_tau <- purrr::map_lgl(tau, function(t) {
      any(abs(dgp$true_qst$tau - t) < 1e-8)
    })
    
    if (!all(valid_tau)) {
      stop("Requested tau values must match canonical grid (0.01-0.99).")
    }
  }

  
  # 3. Get estimator
  est_fn <- est_registry[[estimator_id]]

  # 3.5 THE AIRLOCK (Sanitization)  <--- NEW SECTION
  # Per Constitution Article III, strictly remove forbidden columns.
  # Only estimators with explicit oracle clearance get the raw data.
  df_run <- dgp$df
  if (!isTRUE(config$use_oracle) && !isTRUE(config$use_true_propensity)) {
    df_run <- dgp$df %>% 
      dplyr::select(-any_of(c("y0", "y1", "p", "structural_te")))
  }

  # 3.5 THE AIRLOCK (Sanitization)
  df_run <- dgp$df
  if (!isTRUE(config$use_oracle)) {
    # 1. Strip Columns
    df_run <- dgp$df %>% 
      dplyr::select(-any_of(c("y0", "y1", "p", "structural_te")))
    
    # 2. Strip Attributes (Prevent Metadata Smuggling)
    # Keep only essential S3 classes and names
    attr(df_run, "structural_te") <- NULL 
    attr(df_run, "params") <- NULL
  }
  
  # 4. Point estimate
  est <- tryCatch(
    est_fn(dgp$df, tau = tau, config = config),
    error = function(e) {
      list(
        att  = NA_real_,
        qst  = NULL,
        cf   = NULL,
        meta = list(
          estimator_id = estimator_id,
          errors       = e$message
        )
      )
    }
  )
  
  # 5. Bootstrap (if requested)
  boot_draws <- NULL
  n_boot_ok <- 0
  
  if (bootstrap && !is.na(est$att)) {
    boot_draws <- map_dfr(1:B, function(b) {
      idx <- sample(nrow(dgp$df), replace = TRUE)
      est_b <- tryCatch(
        est_fn(dgp$df[idx, ], tau = tau, config = config),
        error = function(e) NULL
      )
      if (is.null(est_b)) return(NULL)
      tibble(boot_id = b, att = est_b$att, qst = list(est_b$qst))
    })
    
    n_boot_ok <- nrow(boot_draws)
    
    # Warn if too many bootstraps failed
    if (n_boot_ok < 0.9 * B) {
      warning(glue::glue(
        "Only {n_boot_ok}/{B} bootstraps succeeded. CI may be unreliable."
      ))
    }
    
    if (n_boot_ok > 0) {
      # Aggregate ATT CIs
      att_ci <- quantile(boot_draws$att, c(0.025, 0.975), na.rm = TRUE)
      
      # Aggregate QST CIs (if available)
      if (!is.null(est$qst)) {
        qst_ci <- boot_draws %>%
          filter(!map_lgl(qst, is.null)) %>%
          unnest(qst) %>%
          group_by(tau) %>%
          summarise(
            ci_lo = quantile(estimate, 0.025, na.rm = TRUE),
            ci_hi = quantile(estimate, 0.975, na.rm = TRUE),
            .groups = "drop"
          )
      }
    }
  }
  
  # 6. Compute metrics against truth
  #    - att$bias = att$estimate - dgp$true_att
  #    - att$covered = (dgp$true_att >= att$ci_lo) & (dgp$true_att <= att$ci_hi)
  #    - qst$bias, qst$covered analogously (joined on tau)
  #    - If true_att is NA or true_qst is NULL, bias/covered are NA
  
  # 7. Assemble result (Runner Output Contract, Section 2.3)
  # result <- list(
  #   att        = list(estimate, ci_lo, ci_hi, true, bias, covered),
  #   qst        = tibble(tau, estimate, ci_lo, ci_hi, true, bias, covered),
  #   boot_draws = tibble(boot_id, att, qst),
  #   meta       = list(dgp, estimator, dgp_id, estimator_id, seed, ...)
  # )
  
  # 8. Optionally pin result and/or data
  
  # cs_run_single() must return an object exactly matching 

  # the Runner Output Contract in Section 2.3.
  result
}
```

**Notes:**

-   `store_data = FALSE` by default. If TRUE, raw `dgp$df` is pinned separately.
-   B is the *target* number of bootstraps; actual may be lower if some fail.
-   Runner warns if `n_boot_ok < 0.9 * B`.
-   **Metrics:** `cs_run_single` populates `att$bias`, `att$covered`, and the analogous columns in `qst` whenever ground truth is available. If truth is NA/NULL, these fields are set to NA.
-   **CI Merge:** In implementation, `cs_run_single()` merges the bootstrap `att_ci` / `qst_ci` back into the `att` list and `qst` tibble before returning.
-   **When `bootstrap = FALSE`:** CI columns (`ci_lo`, `ci_hi`, `covered`, `mean_ci_width`) are set to `NA`. No bootstrap draws are stored. `n_boot_ok = 0`.
-   **When all bootstraps fail:** If `map_dfr()` returns a 0-row tibble, treat as total failure: set `boot_draws = NULL`, `n_boot_ok = 0`, CIs are `NA`, and `meta$success = FALSE`.
-   **Pinning:** If `board` is non-NULL, `cs_run_single()` calls `cs_pin_write()` to create a `results/` pin for this run. If `store_data = TRUE`, it also pins the raw DGP data to `data/`.

### 4.2 Campaign (Multiple Seeds)

``` r
cs_run_campaign <- function(
  dgp_id,
  estimator_id,
  seeds         = 1:100,
  N             = 2000,
  tau           = seq(0.05, 0.95, 0.05),
  bootstrap     = FALSE,
  B             = 200,
  parallel      = FALSE,
  config        = list(),
  board         = NULL,
  skip_existing = FALSE,
  batch_size    = 1,
  ...
) {
  # Handle concurrency (see Section 4.5)
  if (parallel) {
    config <- modifyList(config, list(num_threads = 1))
  }
  
  # Skip existing runs if requested (see Section 5.7)
  seeds_to_run <- seeds
  if (skip_existing && !is.null(board)) {
    existing <- purrr::map_lgl(seeds, ~ cs_exists(board, dgp_id, estimator_id, .x))
    seeds_to_run <- seeds[!existing]
    if (length(seeds_to_run) == 0) {
      message("All seeds already computed. Loading existing results.")
      return(cs_load_campaign(board, dgp_id, estimator_id))
    }
    if (length(seeds_to_run) < length(seeds)) {
      message(glue::glue("Skipping {sum(existing)} existing runs."))
    }
  }
  
  map_fn <- if (parallel) furrr::future_map else purrr::map
  
  results <- tibble(seed = seeds_to_run) %>%
    mutate(
      result = map_fn(seed, ~ cs_run_single(
        dgp_id       = dgp_id,
        estimator_id = estimator_id,
        seed         = .x,
        N            = N,
        tau          = tau,
        bootstrap    = bootstrap,
        B            = B,
        config       = config,
        board        = board,
        ...
      ))
    )
  
  results
}
```

**Notes:**

-   Returns tibble with `seed` column and `result` list-column (tidyverse-friendly).
-   `parallel = FALSE` by default. If TRUE, uses `furrr::future_map` (requires `future::plan()` set externally).
-   When `parallel = TRUE`, estimators are forced to single-threaded mode (see Section 4.5).
-   `skip_existing = FALSE` by default. If TRUE and `board` is provided, skips seeds already pinned.
-   Additional arguments (e.g., `store_data`) are passed through `...` to `cs_run_single()`.

Note on Batching: batch_size defaults to 1 (Atomic Persistence). In future versions, setting batch_size > 1 will allow workers to aggregate multiple seeds into a single pin to reduce I/O overhead on cloud storage (e.g., S3), compliant with the "Identifiable Partition" clause of Article VI.

### 4.3 Full Design

``` r
cs_run_design <- function(design, parallel = FALSE, config = list(), ...) {
  # design = tibble(dgp_id, estimator_id, seed, N, bootstrap, B)
  # Returns design tibble with added `result` list-column
  #
  # NOTE: design must contain columns: dgp_id, estimator_id, seed, N, bootstrap, B
  # Other arguments (tau, board, store_data) are passed via ... and apply to all rows.
  
  if (parallel) {
    config <- modifyList(config, list(num_threads = 1))
  }
  
  map_fn <- if (parallel) furrr::future_pmap else purrr::pmap
  
  design %>%
    mutate(
      result = map_fn(
        list(dgp_id, estimator_id, seed, N, bootstrap, B),
        function(dgp_id, estimator_id, seed, N, bootstrap, B) {
          cs_run_single(
            dgp_id       = dgp_id,
            estimator_id = estimator_id,
            seed         = seed,
            N            = N,
            bootstrap    = bootstrap,
            B            = B,
            config       = config,
            ...
          )
        }
      )
    )
}
```

### 4.4 Suite Runner

Convenience wrapper for running named test suites.

``` r
cs_run_suite <- function(
  suite_id,
  estimator_ids,
  seeds         = 1:50,
  N             = 2000,
  tau           = seq(0.05, 0.95, 0.05),
  bootstrap     = FALSE,
  B             = 200,
  parallel      = FALSE,
  board         = NULL,
  skip_existing = FALSE,
  ...
) {
  # Get DGPs for this suite
  dgp_ids <- cs_get_suite(suite_id)
  
  # Build design
  design <- tidyr::crossing(
    dgp_id       = dgp_ids,
    estimator_id = estimator_ids,
    seed         = seeds
  ) %>%
    mutate(
      N         = N,
      bootstrap = bootstrap,
      B         = B
    )
  
  # Run (forward skip_existing and board)
  results <- cs_run_design(
    design, 
    parallel      = parallel, 
    board         = board,
    skip_existing = skip_existing,
    ...
  )
  
  # Add suite metadata
  attr(results, "suite_id") <- suite_id
  attr(results, "estimator_ids") <- estimator_ids
  
  results
}
```

**Usage:**

``` r
# Run all estimators on placebo suite
results <- cs_run_suite(
  suite_id     = "placebo",
  estimator_ids = c("GenGC", "GRF_DR", "AIPW_linear"),
  seeds        = 1:100,
  bootstrap    = TRUE
)

# Run GenGC on stress tests
results <- cs_run_suite("stress", "GenGC", seeds = 1:50)

# Full benchmark
results <- cs_run_suite("all", c("GenGC", "GRF_DR"), seeds = 1:20)
```

### 4.5 Concurrency Strategy

Benchmarking simulations are embarrassingly parallel. To maximize throughput and avoid CPU oversubscription (thrashing), CausalStress defaults to a **"Wide & Shallow"** concurrency model.

**The Problem:**

Nested parallelism occurs when: 1. The runner spawns multiple workers (one per seed) 2. Each estimator (e.g., `ranger`) spawns its own threads

On an 8-core machine with 8 workers each spawning 8 threads, you get 64 threads fighting for 8 cores. Performance collapses.

**The Solution:**

| Level | Strategy | Default |
|--------------------|----------------------------|-------------------------|
| **Outer (Runner)** | Process-level parallelism via `furrr` | User sets `plan()` externally |
| **Inner (Estimator)** | Single-threaded when parallel | `num_threads = 1` |

**Rationale:**

Random Forest scalability is sub-linear with threads (8 threads ≠ 8× speedup). Simulation throughput scales linearly with processes. Running 10 seeds on 10 cores (1 thread each) is faster than 10 seeds sequentially (10 threads each).

**Implementation:**

When `parallel = TRUE`, the runner automatically overrides `config$num_threads = 1`:

``` r
# Inside cs_run_campaign / cs_run_design
if (parallel) {
  config <- modifyList(config, list(num_threads = 1))
}
```

**Estimator Contract (Thread Safety):**

Estimators wrapping `ranger` or `grf` must respect `config$num_threads`:

``` r
# Inside estimator_gengc()
n_threads <- config$num_threads %||% (parallel::detectCores() - 1)
ranger(..., num.threads = n_threads)
```

**User Responsibility:**

The user must set the `future` plan externally before calling parallel runners:

``` r
library(future)
plan(multisession, workers = parallel::detectCores() - 2)

results <- cs_run_suite("placebo", "GenGC", parallel = TRUE)

plan(sequential)  # Reset when done
```

**Overriding:**

For profiling single-run performance with multi-threaded estimators, set `parallel = FALSE` and configure `config$num_threads` manually.

**Pin Safety with Parallel Execution:**

When using parallel runners with `board` set, pin names must be unique per run (dgp × estimator × seed) to avoid write collisions. The naming convention enforces this automatically. However, avoid running multiple `cs_run_suite()` calls concurrently to the same board without unique timestamps.

## 4.6 Parallel Persistence Strategy (The Anti-Race Protocol)

To satisfy Article VI of the Constitution, the Runner implements the following safety protocols:

1.  Unique Naming Enforcement:

    The cs_pin_write helper automatically appends /seed\_{seed} to every result pin path. This offloads concurrency management to the underlying file system.

2.  Manifest Lockout:

    When parallel = TRUE, the Runner configures the pins board in "headless" mode (if supported) or explicitly skips write_board_manifest() calls inside the worker loop.

3.  High-Volume Staging (Optional):

    For campaigns with \$N \> 1000\$ seeds, the implementation MAY utilize a "Stage & Gather" pattern:

    -   **Stage:** Workers save raw `.rds` or `.qs` files to a temporary directory using `seed` as the unique key.

    -   **Gather:** Upon completion, the main process scans the directory and performs a serial `pin_write` loop to ingest results into the board.

## 5. Pins Storage

### 5.1 Board Setup (Environment-Aware)

To support seamless transition between local development and server/cluster execution, the board path resolves in the following priority order:

1.  **Explicit Argument:** `cs_board(path = "custom/path")`
2.  **Environment Variable:** `Sys.getenv("R_CAUSALSTRESS_BOARD")`
3.  **Default:** `"causalstress_pins"` (relative to working directory)

``` r
cs_board <- function(
  path      = NULL,
  versioned = TRUE,
  type      = c("folder", "s3", "gcs", "azure", "local"),
  ...
) {
  type <- match.arg(type)
  
  if (is.null(path) && type == "folder") {
    path <- Sys.getenv("R_CAUSALSTRESS_BOARD", unset = "causalstress_pins")
  }
  
  board <- switch(type,
    folder = pins::board_folder(path, versioned = versioned),
    s3     = pins::board_s3(bucket = path, versioned = versioned, ...),
    gcs    = pins::board_gcs(bucket = path, versioned = versioned, ...),
    azure  = pins::board_azure(container = path, versioned = versioned, ...),
    local  = pins::board_local(versioned = versioned)
  )
  
  board
}
```

**Supported board types:**

| Type     | Backend                | Use Case                                  |
|------------------|--------------------------|----------------------------|
| `folder` | `pins::board_folder()` | Local filesystem, network drives, Dropbox |
| `local`  | `pins::board_local()`  | Standard R cache location (cross-session) |
| `s3`     | `pins::board_s3()`     | AWS S3, MinIO, DigitalOcean Spaces        |
| `gcs`    | `pins::board_gcs()`    | Google Cloud Storage                      |
| `azure`  | `pins::board_azure()`  | Azure Blob Storage                        |

**Versioning:**

By default, `versioned = TRUE`. This enables:

-   Automatic version creation on each `pin_write()`
-   Version history via `pins::pin_versions(board, name)`
-   Rollback capability for reproducibility audits

``` r
# List all versions of a pin
board %>% pins::pin_versions("results/synth_heavytail_v1.0.0/GenGC_v0.4.0/seed_42")

# Read a specific version
board %>% pins::pin_read("my_pin", version = "20250526T143000Z-abc123")
```

**Serialization format:**

CausalStress uses `type = "qs"` by default for `pin_write()` calls, which provides:

-   3-5× faster read/write than RDS
-   Better compression
-   Full R object support (including models)

``` r
# In cs_pin_write (internal)
pins::pin_write(board, object, name, type = "parquet", metadata = meta)
```

For interoperability with non-R tools, use `type = "parquet"` for tabular data.

**Environment configuration examples:**

``` bash
# In .Renviron (local development)
R_CAUSALSTRESS_BOARD=~/projects/causalstress_pins

# In .Renviron (server/cluster)
R_CAUSALSTRESS_BOARD=/mnt/shared/research/causalstress_pins

# For S3: pass type explicitly in R, use AWS env vars for auth
# board <- cs_board(path = "my-research-bucket", type = "s3")
AWS_ACCESS_KEY_ID=...
AWS_SECRET_ACCESS_KEY=...
```

**Cache location:**

pins automatically caches remote pins locally. Configure via:

-   `PINS_CACHE_DIR` — pins-specific cache
-   `R_USER_CACHE_DIR` — general R cache (used by rappdirs)

For read-only environments (e.g., AWS Lambda), set cache to a writable temp directory.

### 5.2 Pin Naming Convention

**Granular results (per DGP × estimator × seed):**

```         
results/{dgp_id}_v{dgp_version}/{estimator_id}_v{est_version}/seed_{seed}
```

**Campaign summaries (per DGP × estimator, aggregated over seeds):**

```         
campaigns/{dgp_id}_v{dgp_version}/{estimator_id}_v{est_version}/{timestamp}
```

**Suite summaries (per suite × estimator set, aggregated):**

```         
suites/{suite_id}/{timestamp}
```

**Examples:**

-   `results/synth_heavytail_v1.0.0/GenGC_v0.4.0/seed_42`
-   `results/synth_placebo_tau0_v1.0.0/GenGC_v0.4.0/seed_42`
-   `campaigns/synth_heavytail_v1.0.0/GenGC_v0.4.0/2025-05-26T14-30-00`
-   `suites/placebo/2025-05-26T14-30-00`
-   `data/synth_heavytail_v1.0.0/seed_42` (if `store_data = TRUE`)

### 5.3 Storage Strategy: Hybrid Approach

Suites span multiple DGPs, so we use a **hybrid strategy**:

1.  **Granular pins** for individual results (queryable by DGP)
2.  **Summary pins** for aggregated metrics (lightweight retrieval)

| Type | Content | Granularity | When |
|----------------|-----------------|-------------------------|----------------|
| `results` | Single run output | Per DGP × estimator × seed | During `cs_run_single()` |
| `campaigns` | Aggregated metrics | Per DGP × estimator | After `cs_run_campaign()` |
| `suites` | Suite summary | Per suite × estimator set | After `cs_run_suite()` |
| `boot_draws` | Raw bootstrap draws | Per DGP × estimator × seed | Optional (large) |
| `data` | Raw `dgp$df` | Per DGP × seed | If `store_data = TRUE` |

**Suite runs pin individual results automatically**, then write a lightweight summary:

``` r
cs_run_suite <- function(..., board = NULL) {
  # 1. Build design from suite
  # 2. Run cs_run_design() — each run pins to results/ if board provided
  # 3. Aggregate metrics
  # 4. Pin suite summary to suites/
}
```

### 5.4 Suite Summary Structure

The suite summary pin contains **aggregated metrics only** (not raw results):

``` r
list(
  suite_id      = "placebo",
  estimator_ids = c("GenGC", "GRF_DR", "AIPW_linear"),
  dgp_ids       = c("synth_placebo_tau0", "synth_placebo_nonlinear", ...),
  seeds         = 1:100,
  N             = 2000,
  B             = 200,
  timestamp     = Sys.time(),
  
 # ATT summary (one row per DGP × estimator)
  att_summary = tibble(
    dgp_id,
    estimator_id,
    mean_bias,
    rmse,
    coverage,
    mean_ci_width
  ),
  
  # QST summary (one row per DGP × estimator × tau)
  qst_summary = tibble(
    dgp_id,
    estimator_id,
    tau,
    mean_bias,
    mean_abs_bias,
    coverage
  ),
  
  # Run metadata
  meta = list(
    n_runs_total    = integer(),
    n_runs_success  = integer(),
    n_runs_failed   = integer(),
    git_hash        = character(),
    session_info    = sessionInfo()
  )
)
```

**Benefits:**

-   **Queryable:** Individual results accessible by DGP via `results/` pins
-   **Lightweight:** Suite summary is small (aggregated metrics, not raw data)
-   **Reproducible:** Full provenance in metadata
-   **Comparable:** Easy to compare suite runs across time/versions

### 5.5 Pin Writer Helpers

**General writer:**

``` r
cs_pin_write <- function(
  board,
  object,
  type,
  dgp_meta,
  est_meta,
  seed = NULL,
  format = "qs",
  extra_meta = list()
) {
  # Construct name
  name <- glue::glue(
    "{type}/",
    "{dgp_meta$dgp_id}_v{dgp_meta$version}/",
    "{est_meta$estimator_id}_v{est_meta$version}",
    if (!is.null(seed)) glue::glue("/seed_{seed}") else ""
  )
  
  # Assemble metadata
  meta <- c(
    extra_meta,
    list(
      dgp_id            = dgp_meta$dgp_id,
      dgp_version       = dgp_meta$version,
      estimator_id      = est_meta$estimator_id,
      estimator_version = est_meta$version,
      seed              = seed,
      timestamp         = Sys.time(),
      session_info      = utils::sessionInfo(),
      git_hash          = tryCatch(
        system("git rev-parse HEAD", intern = TRUE),
        error = function(e) NA_character_
      )
    )
  )
  
  pins::pin_write(board, object, name, type = format, metadata = meta)
}
```

**Suite summary writer:**

``` r
cs_pin_write_suite <- function(
  board,
  suite_summary,
  suite_id,
  format = "qs"
) {
  timestamp <- format(Sys.time(), "%Y-%m-%dT%H-%M-%S")
  name <- glue::glue("suites/{suite_id}/{timestamp}")
  
  meta <- list(
    suite_id      = suite_id,
    estimator_ids = suite_summary$estimator_ids,
    dgp_ids       = suite_summary$dgp_ids,
    n_seeds       = length(suite_summary$seeds),
    timestamp     = Sys.time(),
    git_hash      = tryCatch(
      system("git rev-parse HEAD", intern = TRUE),
      error = function(e) NA_character_
    )
  )
  
  pins::pin_write(board, suite_summary, name, type = format, metadata = meta)
}
```

**Format options:**

| Format      | Use Case                                                    |
|-------------|-------------------------------------------------------------|
| `"qs"`      | Default. Fast, compressed, R-native. Best for internal use. |
| `"rds"`     | Fallback if `qs` package unavailable.                       |
| `"parquet"` | Cross-language interoperability (Python, Spark).            |
| `"csv"`     | Human-readable, but loses type info.                        |

### 5.6 Querying Pins

**Find all results for a DGP:**

``` r
cs_list_results <- function(board, dgp_id = NULL, estimator_id = NULL) {
  pins::pin_list(board) %>%
    filter(str_starts(name, "results/")) %>%
    filter(if (!is.null(dgp_id)) str_detect(name, dgp_id) else TRUE) %>%
    filter(if (!is.null(estimator_id)) str_detect(name, estimator_id) else TRUE)
}
```

**Find all suite runs:**

``` r
cs_list_suite_pins <- function(board, suite_id = NULL) {
  pins::pin_list(board) %>%
    filter(str_starts(name, "suites/")) %>%
    filter(if (!is.null(suite_id)) str_detect(name, suite_id) else TRUE)
}
```

**Load latest suite summary:**

``` r
cs_read_suite <- function(board, suite_id) {
  suites <- cs_list_suite_pins(board, suite_id)
  latest <- suites %>% arrange(desc(created)) %>% slice(1)
  pins::pin_read(board, latest$name)
}
```

------------------------------------------------------------------------

### 5.7 Loading & Audit Helpers

CausalStress provides convenience functions for loading previous experiments, searching by metadata, and auditing the full pin history.

**Load a specific run:**

``` r
cs_read_result <- function(board, dgp_id, estimator_id, seed, version = NULL) {
  # Construct expected pin name pattern
  pattern <- glue::glue("results/{dgp_id}.*/{estimator_id}.*/seed_{seed}")
  
  matches <- pins::pin_list(board) %>%
    filter(str_detect(name, pattern))
  
 if (nrow(matches) == 0) {
    stop("No matching pin found for: ", dgp_id, "/", estimator_id, "/seed_", seed)
  }
  
  if (nrow(matches) > 1) {
    warning("Multiple versions found. Loading most recent. Use version arg to specify.")
    matches <- matches %>% arrange(desc(created)) %>% slice(1)
  }
  
  pins::pin_read(board, matches$name, version = version)
}
```

**Load all runs for a campaign (all seeds):**

``` r
cs_load_campaign <- function(board, dgp_id, estimator_id) {
  # Find all matching pins
  results <- cs_list_results(board, dgp_id, estimator_id)
  
  if (nrow(results) == 0) {
    stop("No results found for: ", dgp_id, "/", estimator_id)
  }
  
  # Load all and bind
  results %>%
    mutate(
      seed = str_extract(name, "seed_(\\d+)", group = 1) %>% as.integer(),
      result = map(name, ~ pins::pin_read(board, .x))
    ) %>%
    select(seed, result)
}
```

**Note:** `cs_load_campaign()` returns only seeds that have been pinned. If some seeds were never run or failed, they will not appear in the result. Users should inspect the `seed` column to identify any gaps.

**Search by metadata (without loading data):**

``` r
cs_search <- function(
  board,
  dgp_id       = NULL,
  estimator_id = NULL,
  seed         = NULL,
  after        = NULL,
  before       = NULL
) {
  # Get all pins with metadata
  all_pins <- pins::pin_list(board)
  
  # Filter by name patterns
  if (!is.null(dgp_id)) {
    all_pins <- all_pins %>% filter(str_detect(name, dgp_id))
  }
  if (!is.null(estimator_id)) {
    all_pins <- all_pins %>% filter(str_detect(name, estimator_id))
  }
  if (!is.null(seed)) {
    all_pins <- all_pins %>% filter(str_detect(name, glue::glue("seed_{seed}")))
  }
  
  # Filter by time
  if (!is.null(after)) {
    all_pins <- all_pins %>% filter(created >= after)
  }
  if (!is.null(before)) {
    all_pins <- all_pins %>% filter(created <= before)
  }
  
  # Enrich with metadata (without loading full objects)
  all_pins %>%
    mutate(
      meta = map(name, ~ pins::pin_meta(board, .x))
    )
}
```

**Check if a run exists (for resumable campaigns):**

``` r
cs_exists <- function(board, dgp_id, estimator_id, seed) {
  pattern <- glue::glue("results/{dgp_id}.*/{estimator_id}.*/seed_{seed}")
  
  matches <- pins::pin_list(board) %>%
    filter(str_detect(name, pattern))
  
  nrow(matches) > 0
}
```

**Full audit trail:**

``` r
cs_audit <- function(board, type = NULL) {
  # Get all pins
  all_pins <- pins::pin_list(board)
  
  # Filter by type if specified
  if (!is.null(type)) {
    all_pins <- all_pins %>% filter(str_starts(name, paste0(type, "/")))
  }
  
  # Extract full metadata for each pin
  all_pins %>%
    mutate(
      meta = map(name, ~ {
        m <- pins::pin_meta(board, .x)
        tibble(
          dgp_id       = m$user$dgp_id %||% NA_character_,
          estimator_id = m$user$estimator_id %||% NA_character_,
          seed         = m$user$seed %||% NA_integer_,
          git_hash     = m$user$git_hash %||% NA_character_,
          timestamp    = m$created
        )
      })
    ) %>%
    unnest(meta) %>%
    select(name, dgp_id, estimator_id, seed, timestamp, git_hash, everything())
}
```

**Usage examples:**

``` r
board <- cs_board()

# Quick audit: what have I run?
cs_audit(board) %>%
  count(dgp_id, estimator_id)

# Find all GenGC runs from last week
cs_search(board, estimator_id = "GenGC", after = Sys.Date() - 7)

# Resume an interrupted campaign
results <- cs_run_campaign(
  dgp_id       = "synth_heavytail",
  estimator_id = "GenGC",
  seeds        = 1:100,
  board        = board,
  skip_existing = TRUE
)

# Load previous campaign without re-running
old_results <- cs_load_campaign(board, "synth_heavytail", "GenGC")
```

**Version management:**

``` r
# List versions of a specific pin
board %>% pins::pin_versions("results/synth_heavytail_v1.0.0/GenGC_v0.4.0/seed_42")

# Read a specific version
board %>% pins::pin_read("my_pin", version = "20250526T143000Z-abc123")
  
# Prune old versions (keep last 5)
board %>% pins::pin_versions_prune("my_pin", n = 5)
```

**Inspect metadata without loading:**

``` r
# Quick look at what a pin contains
meta <- pins::pin_meta(board, "results/synth_heavytail_v1.0.0/GenGC_v0.4.0/seed_42")
meta$user$dgp_id
meta$user$estimator_id
meta$user$git_hash
meta$created
meta$file_size
```

------------------------------------------------------------------------

## 6. Metrics

### 6.1 ATT Metrics

**Per-run (single seed):**

| Metric   | Formula                         |
|----------|---------------------------------|
| Bias     | `estimate - true`               |
| Covered  | `true >= ci_lo & true <= ci_hi` |
| CI Width | `ci_hi - ci_lo`                 |

**Per-campaign (aggregated over seeds):**

| Metric        | Formula              |
|---------------|----------------------|
| Mean Bias     | `mean(bias)`         |
| RMSE          | `sqrt(mean(bias^2))` |
| Coverage      | `mean(covered)`      |
| Mean CI Width | `mean(ci_width)`     |

### 6.2 QST Metrics

**Per-run, per-τ:**

| Metric  | Formula                         |
|---------|---------------------------------|
| Bias    | `estimate - true`               |
| Covered | `true >= ci_lo & true <= ci_hi` |

**Per-run summary:**

| Metric             | Formula                  |
|--------------------|--------------------------|
| Mean Absolute Bias | `mean(abs(bias))` over τ |
| Sup Bias           | `max(abs(bias))` over τ  |
| Mean Coverage      | `mean(covered)` over τ   |

**Per-campaign (aggregated over seeds):**

| Metric             | Formula                          |
|--------------------|----------------------------------|
| Mean Absolute Bias | `mean(mean_abs_bias)` over seeds |
| Mean Sup Bias      | `mean(sup_bias)` over seeds      |
| Coverage           | `mean(mean_coverage)` over seeds |

------------------------------------------------------------------------

## 7. Visualization Helpers

``` r
cs_plot_att_bias(att_df)
cs_plot_att_coverage(att_df)
cs_plot_qst(qst_df, show_truth = TRUE, show_ci = TRUE)
cs_plot_qst_coverage(qst_df)
cs_plot_suite_summary(suite_summary)
```

All return ggplot objects. User can customize.

**Note:** All `cs_plot_*()` helpers operate on collected tibbles from `cs_collect_att()` / `cs_collect_qst()`, not on raw campaign results.

**Default parameters:**

-   `show_truth = TRUE`: Overlay ground truth (black dashed line)
-   `show_ci = TRUE`: Show confidence band (the "Blue Band" signature)

### Helper: Extract QST tibble

``` r
cs_collect_qst <- function(results) {
  # Input: campaign/suite results (tibble with result list-column)
  # Output: tibble with columns:
  #   dgp_id, estimator_id, seed, tau, estimate, ci_lo, ci_hi, true, bias, covered
  
  results %>%
    unnest_wider(result) %>%
    select(seed, qst, meta) %>%
    unnest(qst) %>%
    mutate(
      dgp_id       = map_chr(meta, ~ .x$dgp_id),
      estimator_id = map_chr(meta, ~ .x$estimator_id)
    ) %>%
    select(dgp_id, estimator_id, seed, tau, estimate, ci_lo, ci_hi, true, bias, covered)
}
```

### Helper: Extract ATT tibble

``` r
cs_collect_att <- function(results) {
  # Input: campaign/suite results
  # Output: tibble with columns:
  #   dgp_id, estimator_id, seed, estimate, ci_lo, ci_hi, true, bias, covered
  
  results %>%
    # Filter drafts unless requested
    filter(if (include_drafts) TRUE else map_chr(result, ~ .x$meta$certification %||% "draft") == "certified") %>%
    unnest_wider(result)
    unnest_wider(result) %>%
    unnest_wider(att) %>%
    mutate(
      dgp_id       = map_chr(meta, ~ .x$dgp_id),
      estimator_id = map_chr(meta, ~ .x$estimator_id)
    ) %>%
    select(dgp_id, estimator_id, seed, estimate, ci_lo, ci_hi, true, bias, covered)
}
```

### Helper: Summarise Suite Results

``` r
cs_summarise_suite <- function(results, suite_id) {
  # Input: raw suite results (tibble with result list-column)
  # Output: suite summary object for pinning
  
  att_df <- cs_collect_att(results)
  qst_df <- cs_collect_qst(results)
  
  att_summary <- att_df %>%
    group_by(dgp_id, estimator_id) %>%
    summarise(
      mean_bias     = mean(bias, na.rm = TRUE),
      rmse          = sqrt(mean(bias^2, na.rm = TRUE)),
      coverage      = mean(covered, na.rm = TRUE),
      mean_ci_width = mean(ci_hi - ci_lo, na.rm = TRUE),
      .groups       = "drop"
    )
  
  qst_summary <- qst_df %>%
    group_by(dgp_id, estimator_id, tau) %>%
    summarise(
      mean_bias     = mean(bias, na.rm = TRUE),
      mean_abs_bias = mean(abs(bias), na.rm = TRUE),
      coverage      = mean(covered, na.rm = TRUE),
      .groups       = "drop"
    )
  
  list(
    suite_id      = suite_id,
    estimator_ids = unique(att_df$estimator_id),
    dgp_ids       = unique(att_df$dgp_id),
    seeds         = unique(results$seed),
    timestamp     = Sys.time(),
    att_summary   = att_summary,
    qst_summary   = qst_summary,
    meta          = list(
      n_runs_total   = nrow(results),
      n_runs_success = sum(map_lgl(results$result, ~ .x$meta$success)),
      n_runs_failed  = sum(!map_lgl(results$result, ~ .x$meta$success))
    )
  )
}
```

**Expected input for `cs_plot_qst()`:**

> A tibble with columns: `dgp_id`, `estimator_id`, `tau`, `estimate`, `true`, and optionally `ci_lo`, `ci_hi`. Use `cs_collect_qst()` to extract from campaign/suite results.

**Implementation Note (Meta Accessors):**

The `cs_collect_*` helpers assume `meta$dgp_id` and `meta$estimator_id` are populated at the top level of the meta list (not only nested inside `meta$dgp` / `meta$estimator`). Implementations must ensure both flat and nested copies are set for consistency.

------------------------------------------------------------------------

## 8. Future Extensions (Deferred)

| Feature | Difficulty | Notes |
|-----------------------|-------------------------------|-------------------|
| **Sensitivity Analysis (Kill Curves)** | Medium | `cs_run_sensitivity()` — sweep DGP params (N, tilt, outlier_scale), plot breakdown points. Full spec in `DESIGN_SENSITIVITY.md`. |
| Multiple outcomes | Easy | Add `outcome` column to contracts |
| CATE(x) | Medium | Add `target_level = "unit"`, new output schema |
| Clustered bootstrap | Medium | Pass `cluster_id` to runner |
| Gatekeeper protocol | Medium | `cs_validate()` before interpreting QST |
| Tau grid interpolation | Easy | Interpolate when grids don't match exactly |
| YAML config | Easy | Load design tibble from YAML |
| S3 classes + broom | Easy | Polish, not structural |
| Reticulate estimators | Easy | Wrapper function returns contract |
| Vignettes | Easy | "Quickstart", "Add your estimator", "Add your DGP" |
| Custom suites | Easy | `cs_register_suite()` function |
| Leaderboard rendering | Easy | `cs_render_leaderboard()` → markdown/HTML |

------------------------------------------------------------------------



## 9. MVP Scope

**In scope:**

-   DGP registry (synthetic + placebo)
-   Estimator registry (GenGC, GRF-DR)
-   **Test suite registry (placebo, signal, stress, heavytail, all)**
-   Runner (single, campaign, design, **suite**)
-   Bootstrap CIs
-   **Concurrency management (Wide & Shallow)**
-   Pins storage with versioning
-   **Multi-backend boards (folder, s3, gcs, azure)**
-   **qs serialization format**
-   ATT + QST metrics (per-run and per-campaign)
-   Basic plots
-   `cs_collect_qst()` and `cs_collect_att()` helpers
-   **`cs_search()`, `cs_audit()`, `cs_load_campaign()` helpers**
-   **`skip_existing` for resumable campaigns**

**Out of scope:**

-   Multiple outcomes
-   CATE
-   Clustered bootstrap
-   Gatekeeper
-   Tau interpolation
-   YAML config
-   S3 classes
-   Vignettes (add shortly after MVP)
-   Custom suite registration

------------------------------------------------------------------------

## 10. File Structure

```         
causalstress/
├── R/
│   ├── dgp_registry.R
│   ├── dgp_synth_*.R
│   ├── estimator_registry.R
│   ├── estimator_gengc.R
│   ├── estimator_grf_dr.R
│   ├── suite_registry.R
│   ├── runner.R
│   ├── runner_suite.R
│   ├── metrics.R
│   ├── plots.R
│   ├── pins.R
│   └── helpers.R
├── inst/
│   └── dgps/
│       └── *.md
├── tests/
│   └── testthat/
├── vignettes/
│   └── (deferred)
├── DESIGN.md
└── README.md
```

**helpers.R contents:**

``` r
# Null-coalescing operator
`%||%` <- function(x, y) if (!is.null(x)) x else y

# Tau grid validator
cs_validate_tau <- function(dgp, tau) {

  if (is.null(dgp$true_qst)) return(invisible(TRUE))
  
  if (!all(tau %in% dgp$true_qst$tau)) {
    stop(
      "tau grid must be subset of true_qst$tau (0.01-0.99). ",
      "Missing: ", paste(setdiff(tau, dgp$true_qst$tau), collapse = ", ")
    )
  }
  invisible(TRUE)
}
```

------------------------------------------------------------------------

## 11. Open Questions (Resolved)

| Question | Decision |
|------------------------------------|------------------------------------|
| Store raw data in pins? | Optional via `store_data = FALSE` (default) |
| Parallel backend? | Optional via `parallel = FALSE` (default); uses `furrr` if TRUE |
| How to version DGPs/estimators? | Explicit semantic versions in `meta$version` + git hash in pin metadata |
| Tau grid mismatch? | MVP: tau must be subset of truth grid; error otherwise. Interpolation deferred. |
| CPU oversubscription? | "Wide & Shallow" model: single-threaded estimators when parallel |
| Named test suites? | Built-in suites (placebo, signal, stress, heavytail, all) |

------------------------------------------------------------------------

## Appendix A: Example Usage

``` r
library(causalstress)
library(future)

# === Single run ===
result <- cs_run_single(
  dgp_id       = "synth_heavytail",
  estimator_id = "GenGC",
  seed         = 42,
  tau          = seq(0.05, 0.95, 0.05),
  bootstrap    = TRUE,
  B            = 200
)

# === Campaign ===
campaign <- cs_run_campaign(
  dgp_id       = "synth_heavytail",
  estimator_id = "GenGC",
  seeds        = 1:100,
  bootstrap    = TRUE
)

# === Suite (The Power Feature) ===
# Set up parallel backend
plan(multisession, workers = 6)

# Run placebo validation for multiple estimators
placebo_results <- cs_run_suite(
  suite_id      = "placebo",
  estimator_ids = c("GenGC", "GRF_DR", "AIPW_linear"),
  seeds         = 1:100,
  bootstrap     = TRUE,
  parallel      = TRUE
)

plan(sequential)

# === Extract and visualize ===
qst_df <- cs_collect_qst(placebo_results)
att_df <- cs_collect_att(placebo_results)

cs_plot_qst(qst_df, show_truth = TRUE)
cs_plot_att_bias(att_df)

# === Store ===
board <- cs_board()

# Summarise suite results (aggregate metrics)
suite_summary <- cs_summarise_suite(placebo_results, suite_id = "placebo")

# Store suite summary
cs_pin_write_suite(board, suite_summary, suite_id = "placebo")
```

------------------------------------------------------------------------

## Appendix B: Estimator Wrapper Template

``` r
estimator_my_method <- function(df, tau = seq(0.05, 0.95, 0.05), config = list()) {
  
  # === Handle thread configuration ===
  n_threads <- config$num_threads %||% (parallel::detectCores() - 1)
  
  # === Your estimation code here ===
  # att_hat <- ...
  # qst_hat <- tibble(tau = tau, estimate = ...)
  # y0_draws <- matrix(...)
  
  list(
    att = att_hat,
    
    qst = qst_hat,  # or NULL if not supported
    
    cf = list(
      y0_draws = y0_draws  # or NULL
    ),
    
    meta = list(
      estimator_id  = "MyMethod",
      version       = "1.0.0",
      capabilities  = c("att", "qst"),  # or just c("att")
      target_level  = "population",
      config        = config,
      warnings      = character(),
      errors        = character()
    )
  )
}
```

------------------------------------------------------------------------

## Appendix C: DGP Template

``` r
dgp_my_dgp <- function(N = 2000, seed = NULL, ...) {
  if (!is.null(seed)) set.seed(seed)
  
  # === Generate data ===
  # x1 <- rnorm(N)
  # ...
  # y0 <- ...
  # y1 <- ...
  # w <- rbinom(N, 1, p)
  # y <- ifelse(w == 1, y1, y0)
  
  df <- tibble(
    y  = y,
    w  = w,
    y0 = y0,
    y1 = y1,
    x1 = x1,
    ...
  )
  
  # === Compute ground truth ===
  true_att <- mean(structural_te[w == 1])

  
  # QST: quantile shift for treated units
  tau_grid <- seq(0.01, 0.99, 0.01)
  true_qst <- tibble(
    tau   = tau_grid,
    value = quantile(y1[w == 1], tau_grid) - quantile(y0[w == 1], tau_grid)
  )
  
  list(
    df       = df,
    true_att = true_att,
    true_qst = true_qst,
    meta     = list(
      dgp_id  = "my_dgp",
      version = "1.0.0",
      type    = "synthetic",
      structural_te = structural_te,
      params  = list(N = N, ...)
    )
  )
}
```

------------------------------------------------------------------------

## Appendix D: Adding a Custom Suite

``` r
# Future feature (post-MVP)
cs_register_suite <- function(suite_id, dgp_ids, description = NULL) {
  # Validates dgp_ids exist in registry
  # Adds to suite registry
  # Persists to package state or user config
}

# Usage:
cs_register_suite(
  "my_validation",
  c("synth_heavytail", "synth_qte1", "my_custom_dgp"),
  description = "Custom validation suite for Paper 1"
)
```