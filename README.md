
<!-- README.md is generated from README.Rmd. Please edit that file -->

# CausalStress

> **⚠️ Early Access (v0.1.2)**  
> CausalStress is currently in **Alpha**.  
> The **architecture is stable and fully tested**, but the **DGP and
> Estimator libraries are minimal** in this release.  
> We encourage experimentation, but **do not use for production
> research** until v0.2.0.

**CausalStress** is a **scientific instrument** for benchmarking causal
inference estimators.  
It enforces strict *Constitutional Guarantees* to ensure:

- reproducibility  
- comparability  
- fairness  
- version safety  
- crash resilience

even as estimators and DGPs grow increasingly complex.

> **Note:** v0.1.x is **serial-only**.  
> Parallel execution is scheduled for **v0.2.0**.

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

------------------------------------------------------------------------

## 2. Tidy the Results

    #> # A tibble: 6 × 6
    #>   dgp_id          estimator_id  seed est_att att_ci_width att_covered
    #>   <chr>           <chr>        <int>   <dbl>        <dbl> <lgl>      
    #> 1 synth_baseline  lm_att           1   1.04         0.229 TRUE       
    #> 2 synth_baseline  lm_att           2   1.12         0.228 TRUE       
    #> 3 synth_baseline  lm_att           3   1.26         0.245 TRUE       
    #> 4 synth_baseline  lm_att           4   1.11         0.242 TRUE       
    #> 5 synth_baseline  lm_att           5   1.18         0.214 TRUE       
    #> 6 synth_heavytail lm_att           1   0.106        2.28  TRUE

------------------------------------------------------------------------

## 3. Scorecard Summary

    #> # A tibble: 4 × 4
    #>   dgp_id          estimator_id    RMSE Coverage
    #>   <chr>           <chr>          <dbl>    <dbl>
    #> 1 synth_baseline  ipw_att      -0.0116      0.8
    #> 2 synth_baseline  lm_att        0.0178      1  
    #> 3 synth_heavytail ipw_att       1.95        0.6
    #> 4 synth_heavytail lm_att        2.17        0.6

------------------------------------------------------------------------

## 4. Audit and Time Travel

    #> # A tibble: 5 × 5
    #>   dgp_id         estimator_id  seed git_hash                           timestamp
    #>   <chr>          <chr>        <int> <chr>                                  <dbl>
    #> 1 synth_baseline ipw_att          1 92705e6d100eb23c15bba39f7a97dd112…    1.76e9
    #> 2 synth_baseline ipw_att          2 92705e6d100eb23c15bba39f7a97dd112…    1.76e9
    #> 3 synth_baseline ipw_att          3 92705e6d100eb23c15bba39f7a97dd112…    1.76e9
    #> 4 synth_baseline ipw_att          4 92705e6d100eb23c15bba39f7a97dd112…    1.76e9
    #> 5 synth_baseline ipw_att          5 92705e6d100eb23c15bba39f7a97dd112…    1.76e9

You can retrieve any run from any git commit, ever.

------------------------------------------------------------------------

# Extensibility: Registries & Plugins

CausalStress maintains two central registries:

### DGP Registry

    #> # A tibble: 12 × 5
    #>    dgp_id                          type      generator version description      
    #>    <chr>                           <chr>     <list>    <chr>   <chr>            
    #>  1 synth_baseline                  synthetic <fn>      1.3.0   Baseline linear …
    #>  2 synth_heavytail                 synthetic <fn>      1.3.0   Same linear sign…
    #>  3 synth_placebo_tau0              synthetic <fn>      1.3.0   Sharp-null place…
    #>  4 synth_qte1                      synthetic <fn>      1.3.0   Sign-flip QTE DG…
    #>  5 synth_nonlinear_heteroskedastic synthetic <fn>      1.3.0   Nonlinear hetero…
    #>  6 synth_overlap_stressed          synthetic <fn>      1.3.0   Overlap-stressed…
    #>  7 synth_tilt_mild                 synthetic <fn>      1.3.0   Mildly tilted pr…
    #>  8 synth_placebo_nonlinear         synthetic <fn>      1.3.0   Placebo nonlinea…
    #>  9 synth_placebo_heavytail         synthetic <fn>      1.3.0   Placebo heavy-ta…
    #> 10 synth_placebo_tilted            synthetic <fn>      1.3.0   Placebo tilted: …
    #> 11 synth_placebo_kangschafer       synthetic <fn>      1.4.0   Kang–Schafer mis…
    #> 12 synth_hd_sparse_plm             synthetic <fn>      1.4.0   High-dim sparse …

### Estimator Registry

    #> # A tibble: 3 × 9
    #>   estimator_id type   generator oracle supports_qst version description   source
    #>   <chr>        <chr>  <list>    <lgl>  <lgl>        <chr>   <chr>         <chr> 
    #> 1 oracle_att   oracle <fn>      TRUE   FALSE        0.1.1   Oracle ATT u… core  
    #> 2 lm_att       gcomp  <fn>      FALSE  FALSE        0.1.1   Linear outco… core  
    #> 3 ipw_att      ipw    <fn>      FALSE  FALSE        0.1.1   Inverse-prob… core  
    #> # ℹ 1 more variable: requires_pkgs <list>

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

# Parallelization

CausalStress defines, in the Constitution (Article V), how **parallel
execution must behave once implemented**. These rules govern thread
safety, deterministic RNG behavior, and protection against race
conditions.

However:

> **v0.1.x is strictly serial-only.**  
> The parallel rules are *normative*, not currently active.

Parallel execution affects: - RNG determinism across processes  
- Atomic persistence and crash recovery  
- Progress signaling  
- Resume logic stability  
- Interactions with estimator thread settings

Because these pieces are foundational, parallelism will be introduced
carefully in **v0.2.0**, fully compliant with Article V.

### Planned API (v0.2.0)

Parallel execution will be **deterministic, race-free, and resume-safe**
once implemented.

------------------------------------------------------------------------

# Vignettes

See:

------------------------------------------------------------------------

# Citation

If you use CausalStress, please cite:

> Thomasberger, M. (2025). *CausalStress: A rigorous benchmarking
> framework built on a Constitutional architecture.* R package version
> 0.1.0.

\`\`\`
