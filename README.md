
<!-- README.md is generated from README.Rmd. Please edit that file -->

# CausalStress

> **⚠️ Early Access (v0.1.x)**  
> CausalStress is currently in **Alpha**.  
> The **architecture is stable and fully tested**, but the **DGP and
> Estimator libraries are minimal** in this release.  
> We encourage experimentation, but **do not use for production
> research** until v0.2.0.

## ⚠️ Important Disclaimer (DGP Library Status: Experimental)

CausalStress v0.1.x ships with an expanded *DGP zoo*, but **these DGPs
have not yet undergone full human validation**. All implementations
currently pass the Constitutional test suite (Airlock, truth separation,
determinism, reproducible contracts), but they have **not yet been
manually reviewed for scientific interpretation, historical correctness,
or citation-accurate replication**.

Accordingly:

- **Do not write papers or draw scientific conclusions based on the
  current DGP zoo.**
- The only DGPs considered *validated* in v0.1.x are:
  - `synth_baseline`
  - `synth_heavytail`
- All other DGPs are **experimental** and may change in minor ways
  during the validation process (vignettes, plots, theoretical
  motivations, and source citations will be added in upcoming versions).

A full DGP validation wave is planned for **v0.3.x**, after the entire
suite is complete and stable.

------------------------------------------------------------------------

## Note on LLM-Assisted Code Generation

CausalStress is developed at high velocity, and part of that
productivity comes from using LLMs **as drafting assistants** mainly for
generating boilerplate R code, templates, and scaffolding for DGPs and
internal helpers.

Two clarifications are important:

1.  **LLMs do not define or validate the scientific content.**  
    All generated code is rewritten, integrated, and then constrained by
    the CausalStress *Constitution*: deterministic RNG, strict truth
    separation, Airlock enforcement, reproducibility guarantees,
    versioned registries, and a comprehensive test suite.  
    The **framework**, not the LLM, is the source of correctness.

2.  **LLM-generated code is never accepted without human oversight.**  
    Until every DGP is manually validated, all LLM-assisted DGPs remain
    **experimental**. A dedicated vignette will accompany each one,
    providing plots, structural formulas, references, and a motivation
    for what the DGP is meant to stress-test.

Using modern tooling accelerates development, but the **governance,
scientific responsibility, and quality control remain human-driven**.
Once validation is complete, the “experimental” label will be removed.

For now, **no published research should rely on unvalidated DGPs.**

# What is CausalStress?

**CausalStress** is a **scientific instrument** for benchmarking causal
inference estimators.  
It enforces strict *Constitutional Guarantees* to ensure:

- reproducibility
- comparability
- fairness
- version safety
- crash resilience

even as estimators and DGPs grow increasingly complex.

------------------------------------------------------------------------

# Why This Exists

Causal inference simulations today are often fragile:

- **Truth Drift:** The “true effect” depends on the seed or sample
  instead of the structural model.  
- **Data Leakage:** Estimators accidentally see `y0`, `y1`, or true
  `p`.  
- **Silent Failure:** One crash = three days lost.  
- **Version Drift:** Results from last month can’t be reproduced because
  code changed silently.

These are **not** coding problems… They are **governance problems**.

CausalStress solves them by introducing a **Constitution**: a simple set
of rules that enforce scientific hygiene.

Not a manifesto.  
Not philosophy.  
Just **guardrails** for reliable science.

------------------------------------------------------------------------

# The Constitutional Guarantees

### **1. Two-Tier Truth (Article I)**

True effects are computed using a massive Oracle sample ($N=10^6$),
independent of the simulation sample.

### **2. The Airlock (Article III)**

Forbidden columns are physically removed before any estimator sees the
data:

    y0, y1, p, structural_te

Leakage is prevented for estimators that respect the CausalStress
interface: forbidden columns (`y0`, `y1`, `p`, `structural_te`) are
stripped before data reach the estimator. Bypassing this requires
*deliberate* violation of the framework’s conventions.

### **3. Atomic Persistence (Article VI)**

Each seed is immediately saved to a `pins` board.  
If your cluster dies on seed 999/1000 you’re safe.

### **4. Provenance & Time Travel (Article II)**

Every result includes:

- Git hash  
- Timestamp  
- Full R Session Info

So you can always reproduce or load old runs.

### Architecture

``` mermaid
flowchart LR
    %% Style Definitions
    classDef input fill:#D1E8E2,stroke:#333,stroke-width:1px;
    classDef safe fill:#FFD1D1,stroke:#333,stroke-width:1px;
    classDef store fill:#FFE4B5,stroke:#333,stroke-width:1px;
    classDef view fill:#E6E6FA,stroke:#333,stroke-width:1px;

    subgraph Registries ["1. Registries"]
        direction TB
        DGP["DGP<br/>(Formula + Truth)"]:::input
        Est["Estimator<br/>(Function)"]:::input
    end

    subgraph Runner ["2. Constitutional Runner"]
        direction TB
        Gen((Generate))
        Airlock{{"🔒 AIRLOCK<br/>(Strip y0, y1, p)"}}:::safe
        Boot((Bootstrap))
        
        Gen --> Airlock
        Airlock --> Boot
    end

    subgraph Output ["3. Output"]
        Pins[("💾 Persistence<br/>(Pins Board)")]:::store
        Tidy["📄 cs_tidy()<br/>(Analysis Table)"]:::view
    end

    %% Connections
    DGP --> Gen
    Est --> Boot
    Boot --> Pins
    Boot --> Tidy
```

------------------------------------------------------------------------

# Installation

``` r
# install.packages("pak")
pak::pak("blechturm/CausalStress")
```

------------------------------------------------------------------------

# The Workflow

**Run → Persist → Audit → Tidy**

We benchmark two estimators (`lm_att`, `ipw_att`) on two DGPs:

- `synth_baseline`  
- `synth_heavytail`

More are coming soon, including **theory-backed stress DGPs** and
**famous datasets** (IHDP, Lalonde, Kang–Schafer, ACIC-style
generators).

------------------------------------------------------------------------

## 1. Run a Campaign

``` r
library(CausalStress)
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
library(pins)
#> Warning: package 'pins' was built under R version 4.5.2

board <- pins::board_temp()

runs <- cs_run_grid(
  dgp_ids       = c("synth_baseline", "synth_heavytail"),
  estimator_ids = c("lm_att", "ipw_att"),
  n             = 500,
  seeds         = 1:5,
  bootstrap     = TRUE,
  B             = 100,
  board         = board,
  skip_existing = TRUE
)
#> Running batch: synth_baseline x lm_att
#> Running batch: synth_heavytail x lm_att
#> Running batch: synth_baseline x ipw_att
#> Running batch: synth_heavytail x ipw_att
```

------------------------------------------------------------------------

## 2. Tidy the Results

``` r
runs_tidy <- runs %>%
  cs_tidy()

runs_tidy %>%
  select(dgp_id, estimator_id, seed, est_att, att_ci_width, att_covered) %>%
  head(6)
#> # A tibble: 6 × 6
#>   dgp_id          estimator_id  seed est_att att_ci_width att_covered
#>   <chr>           <chr>        <int>   <dbl>        <dbl> <lgl>      
#> 1 synth_baseline  lm_att           1   1.04         0.238 TRUE       
#> 2 synth_baseline  lm_att           2   1.12         0.228 TRUE       
#> 3 synth_baseline  lm_att           3   1.26         0.245 TRUE       
#> 4 synth_baseline  lm_att           4   1.11         0.242 TRUE       
#> 5 synth_baseline  lm_att           5   1.18         0.214 TRUE       
#> 6 synth_heavytail lm_att           1   0.106        2.47  FALSE
```

------------------------------------------------------------------------

## 3. Scorecard Summary

``` r
summary <- cs_summarise_runs(runs_tidy)

summary %>% 
  select(dgp_id, estimator_id, RMSE = mean_error, Coverage = mean_att_covered)
#> # A tibble: 4 × 4
#>   dgp_id          estimator_id    RMSE Coverage
#>   <chr>           <chr>          <dbl>    <dbl>
#> 1 synth_baseline  ipw_att      -0.0116      0.8
#> 2 synth_baseline  lm_att        0.0178      1  
#> 3 synth_heavytail ipw_att       1.95        0.6
#> 4 synth_heavytail lm_att        2.17        0.4
```

------------------------------------------------------------------------

## 4. Audit and Time Travel

``` r
history <- cs_audit(board)

history %>%
  select(dgp_id, estimator_id, seed, git_hash, timestamp) %>%
  head(5)
#> # A tibble: 5 × 5
#>   dgp_id         estimator_id  seed git_hash                 timestamp          
#>   <chr>          <chr>        <int> <chr>                    <dttm>             
#> 1 synth_baseline ipw_att          1 5ee9e4a75c561411e22e568… 2025-11-30 22:55:34
#> 2 synth_baseline ipw_att          2 5ee9e4a75c561411e22e568… 2025-11-30 22:55:34
#> 3 synth_baseline ipw_att          3 5ee9e4a75c561411e22e568… 2025-11-30 22:55:35
#> 4 synth_baseline ipw_att          4 5ee9e4a75c561411e22e568… 2025-11-30 22:55:36
#> 5 synth_baseline ipw_att          5 5ee9e4a75c561411e22e568… 2025-11-30 22:55:36
```

You can retrieve any run from any git commit, ever.

------------------------------------------------------------------------

# Extensibility: Registries & Plugins

CausalStress maintains two central registries:

### DGP Registry

``` r
CausalStress:::cs_dgp_registry()
#> # A tibble: 12 × 9
#>    dgp_id                   type  generator version description status rationale
#>    <chr>                    <chr> <list>    <chr>   <chr>       <chr>  <chr>    
#>  1 synth_baseline           synt… <fn>      1.3.0   Baseline l… stable Validate…
#>  2 synth_heavytail          synt… <fn>      1.3.0   Same linea… stable Validate…
#>  3 synth_placebo_tau0       synt… <fn>      1.3.0   Sharp-null… exper… Pending …
#>  4 synth_qte1               synt… <fn>      1.3.0   Sign-flip … exper… Pending …
#>  5 synth_nonlinear_heteros… synt… <fn>      1.3.0   Nonlinear … exper… Pending …
#>  6 synth_overlap_stressed   synt… <fn>      1.3.0   Overlap-st… exper… Pending …
#>  7 synth_tilt_mild          synt… <fn>      1.3.0   Mildly til… exper… Pending …
#>  8 synth_placebo_nonlinear  synt… <fn>      1.3.0   Placebo no… exper… Pending …
#>  9 synth_placebo_heavytail  synt… <fn>      1.3.0   Placebo he… exper… Pending …
#> 10 synth_placebo_tilted     synt… <fn>      1.3.0   Placebo ti… exper… Pending …
#> 11 synth_placebo_kangschaf… synt… <fn>      1.4.0   Kang-Schaf… exper… Pending …
#> 12 synth_hd_sparse_plm      synt… <fn>      1.4.0   High-dim s… exper… Pending …
#> # ℹ 2 more variables: date_status_changed <chr>, design_spec <chr>
```

### Estimator Registry

``` r
CausalStress:::cs_estimator_registry()
#> # A tibble: 3 × 9
#>   estimator_id type   generator oracle supports_qst version description   source
#>   <chr>        <chr>  <list>    <lgl>  <lgl>        <chr>   <chr>         <chr> 
#> 1 oracle_att   oracle <fn>      TRUE   FALSE        0.1.2   Oracle ATT u… core  
#> 2 lm_att       gcomp  <fn>      FALSE  FALSE        0.1.2   Linear outco… core  
#> 3 ipw_att      ipw    <fn>      FALSE  FALSE        0.1.2   Inverse-prob… core  
#> # ℹ 1 more variable: requires_pkgs <list>
```

DGPs and Estimators can be added via:

- `cs_register_dgp()`  
- `cs_register_estimator()`

The Airlock guarantees that custom estimators receive **only legal
inputs**.

### Upcoming DGP Library (v0.2–v0.3)

We will add **theory-backed stress tests**:

- heteroskedastic confounding  
- heavy-tail outcomes  
- missingness mechanisms  
- weak instruments  
- near-violations of ignorability  
- overlap collapse scenarios

And later:

- Lalonde  
- IHDP  
- Kang & Schafer  
- ACIC generators  
- DoubleML benchmark datasets

------------------------------------------------------------------------

# Python Support (v0.4.0)

Many SOTA estimators are Python-only (EconML, DoWhy, DragonNet).  
We support them with a **Hub & Spoke** model:

### R = Hub

- Generates data  
- Enforces truth & airlock  
- Stores provenance  
- Persists results atomically

### Python = Spoke

- Receives Parquet  
- Trains estimator  
- Returns predictions in a strict schema

This ensures **fairness, reproducibility, and no leakage** across
languages.

------------------------------------------------------------------------

# Parallelization (Constitutional Requirement)

Parallel execution is explicitly part of the Constitution (Article V:
*Computational Safety*).

**Why not in the MVP?**  
Parallelism touches:

- RNG determinism  
- Progress bars  
- Atomic writes  
- Resume logic  
- Future cluster safety

Now that all foundations are stable, parallelization is coming in
**v0.2.0**.

Planned API:

``` r
library(future)
plan(multisession)

with_progress({
  cs_run_grid(..., parallel = TRUE)
})
```

Thanks to atomic seeds, this is **race-free, deterministic, and
resume-safe**.

------------------------------------------------------------------------

# Vignettes

See:

``` r

vignette("from-run-to-history", package = "CausalStress")
```

------------------------------------------------------------------------

# Citation

If you use CausalStress, please cite:

> Thomasberger, M. (2025). *CausalStress: A rigorous benchmarking
> framework built on a Constitutional architecture..* R package version
> 0.1.x.

\`\`\`
