# Design Specification: Parameterized DGP Families in CausalStress

**Version:** 3.2 (Final - Implementation Ready)\
**Status:** APPROVED FOR IMPLEMENTATION\
**Target Release:** v0.2.0 "Families MVP"\
**Date:** 2025-01-08

------------------------------------------------------------------------

## Executive Summary

This specification introduces **Parameterized DGP Families** as first-class objects in CausalStress, enabling scientifically defensible stress-testing along controlled pathology axes.

**Critical Fixes from v3.1:** - ✅ Campaign fingerprint with canonical n handling (single-n for MVP) - ✅ Loose consolidation uses run_fingerprint (prevents merging incompatible runs) - ✅ Corrected oracle mixture variance calculation - ✅ Split status into plan_status/run_status (unambiguous semantics) - ✅ Oracle tolerance metadata in regression tests - ✅ Explicit Z standardization with documented weights - ✅ dgp_config defaults serialized at planning time

**Review Verdicts:** - Gemini: "🟢 GO FOR LAUNCH" - ChatGPT: "Genuinely build-ready with these fixes"

**Status:** All critical architectural, mathematical, and operational issues resolved. Ready for immediate implementation.

------------------------------------------------------------------------

## Table of Contents

1.  [Motivation](#1-motivation)
2.  [Core Concepts](#2-core-concepts)
3.  [Constitutional Constraints](#3-constitutional-constraints)
4.  [Technical Specifications](#4-technical-specifications)
5.  [Estimand Definitions](#5-estimand-definitions)
6.  [Truth Precision Contract](#6-truth-precision-contract)
7.  [Fingerprint Taxonomy](#7-fingerprint-taxonomy)
8.  [Family Specifications](#8-family-specifications)
9.  [Architectural Implementation](#9-architectural-implementation)
10. [Campaign Planning](#10-campaign-planning)
11. [Status and Eligibility](#11-status-and-eligibility)
12. [Survivorship Bias Protocol](#12-survivorship-bias-protocol)
13. [Campaign Consolidation](#13-campaign-consolidation)
14. [Implementation Roadmap](#14-implementation-roadmap)
15. [Validation Plan](#15-validation-plan)
16. [Governance](#16-governance)
17. [Appendices](#17-appendices)

------------------------------------------------------------------------

## 1. Motivation

### 1.1 The Point DGP Problem

CausalStress v0.1.x operates on **fixed synthetic scenarios**:

``` r
synth_baseline      # df → ∞ (Gaussian)
synth_heavytail     # df ≈ 4 (moderate tails)
# Missing: df = 3, 2.5, 2.2, 2.05, ...
```

To study robustness **as a function of tail thickness**, authors must: - Add N nearly-identical DGPs to the registry (explosion) - Make arbitrary comparisons between discrete points - Invite reviewer skepticism ("why these values?")

### 1.2 The Solution

**Parameterized DGP Families**: factories indexed by scalar stress parameter λ that generate immutable DGP instances with deterministic oracle truth.

**Scientific value:** Kill-plots reveal where estimators **transition from statistical degradation to computational breakdown**.

------------------------------------------------------------------------

## 2. Core Concepts

### 2.1 DGP Family Definition

A **DGP Family** consists of:

1.  **Family ID** (`family_id`): Unique identifier (e.g., `"heavytail_noise_t"`)
2.  **Stress parameter** (`λ`): Scalar pathology dial
3.  **Parameter grid** (`lambda_grid`): Declared discrete values (character vector)
4.  **Factory function**: `(lambda, n, seed, scale_mode, dgp_config) → DGP instance`
5.  **Oracle function**: `(lambda, scale_mode, oracle_config) → {att_truth, qst_truth}`
6.  **DGP config defaults**: `dgp_config_defaults` (explicit parameter defaults)
7.  **Metadata**: See Section 2.2

### 2.2 Family Metadata

``` r
family_metadata <- list(
  lambda_name = "degrees_of_freedom",
  lambda_symbol = "ν",
  lambda_scale = "log",              # "linear" | "log"
  lambda_grid_default = c("30", "10", "5", "3", "2.5"),
  scale_modes = c("l2_var"),         # Valid modes for this family
  
  moment_regime = function(lambda, scale_mode) {
    df <- as.numeric(lambda)
    list(
      mean_exists = df > 1,
      variance_exists = df > 2,
      att_estimable = df > 1,
      rmse_valid = df > 2 && scale_mode == "l2_var",
      oracle_method = if (df > 2.5) "quadrature" else "not_implemented"
    )
  },
  
  # v3.2: DGP config defaults (empty for heavytail_noise_t)
  dgp_config_defaults = list(),
  
  recommended_range = c(2.5, 30),
  death_zone_expected = c(1.5, 2.5),
  
  interpretation = list(
    primary = "Tail thickness (lower ν = heavier tails)",
    confound = "L2 signal-to-noise fixed via theoretical variance",
    estimator_prediction = "L2 estimators degrade; L1-robust estimators stable"
  )
)
```

### 2.3 Hierarchy of Identity

```         
Family Level:
  ├─ family_id: "heavytail_noise_t"
  ├─ family_version: "1.0.0"
  └─ family_fingerprint: SHA256(family_id, version, metadata, source_hash)

Instance Level (λ-specific):
  ├─ instance_id: "heavytail_noise_t_df3_l2var"
  ├─ lambda_canonical: "3"
  ├─ scale_mode: "l2_var"
  ├─ dgp_config: {} (serialized defaults)
  └─ instance_fingerprint: SHA256(family_fp, lambda_canonical, scale_mode, dgp_config_fp)

Run Level (seed-specific):
  ├─ run_id: unique per execution
  ├─ n: 1000
  ├─ seed: 42
  ├─ estimator_id: "gengc"
  └─ run_fingerprint: SHA256(instance_fp, estimator_id, n, seed, est_config_fp)

Campaign Level:
  ├─ campaign_id: SHA256(grid, estimators, strategy_map, n, schema_version)
  └─ campaign_metadata: {timestamp, git_hash, hardware_id, ...}
```

**Key Principle:** Identity flows down (deterministic), provenance flows up (timestamped).

------------------------------------------------------------------------

## 3. Constitutional Constraints

### Article I: Estimand Preservation

The **definition** of ATT, QST, QTE is identical ∀λ. Only the data-generating regime varies.

### Article II: Separation of Concerns

**Science payload** (estimates, truth, errors) is separated from **provenance** (timestamps, git hash, hardware).

### Article III: Airlock Integrity

DGP generation and oracle truth MUST NOT depend on estimator behavior. Oracle columns stripped before estimator sees data.

**CRITICAL:** The definition of quantities (e.g., confounding index Z) must be **deterministic in population parameters**, not sample realizations. Using sample statistics (e.g., `sd(Z)`) violates airlock integrity.

### Article IV: Stable Oracle Contract

Oracle truth must be **deterministic within declared tolerances**, **precision-controlled**, and **auditable**.

**Revised Reproducibility Claim (v3.1):** \> "Oracle truth is numerically reproducible up to declared tolerance bounds (default: 1e-5 relative, 1e-6 absolute) across platforms, R versions, and BLAS implementations."

**NOT claimed:** Bitwise-identical results across all configurations.

**Justification:** Statistical functions (`dnorm`, `pt`, `qnorm`, `uniroot`) are not guaranteed bitwise-identical across platforms. Tolerance-level reproducibility is the appropriate standard for scientific computing.

Allowed implementations (in order of preference):

1.  **Closed-form** when available
2.  **Deterministic numerical integration** (quadrature, fixed domain)
3.  **Quasi-Monte Carlo** with fixed nodes, fixed N, deterministic transform
4.  **Monte Carlo** only if:
    -   Moment regime is safe (finite variance)
    -   Oracle SE ≤ 1% of signal magnitude
    -   Convergence diagnostics stored

**Truth Precision Contract** (mandatory): - Absolute tolerance: `oracle_tol_abs` (default: 1e-6) - Relative tolerance: `oracle_tol_rel` (default: 1e-5) - Domain specification: adaptive based on distribution (see Section 6.2.1) - Validation suite: limiting cases, monotonicity, regression tests - All oracle comparisons use `expect_equal(..., tolerance = tol)`, never exact equality - **v3.2:** Tolerance metadata stored with regression truths

### Article V: Frozen Grid for Citation

The λ-grid is **campaign-level citation metadata**. Instance-level resume is permitted when expanding grids (see Section 13 for consolidation).

### Article VI: Explicit Moment Declarations

Each family MUST declare `moment_regime(lambda, scale_mode)` returning existence flags.

### Article VII: Deterministic Planning

Campaign fingerprints MUST NOT include timestamps or random seeds. Planning is a pure function of inputs.

### Article VIII: Eligibility Transparency

Eligibility rules MUST NOT encode favoritism toward specific methods. If an estimator is marked "always eligible," this must be justified as a scientific claim about the estimator's theoretical properties, and regimes where the estimand is ill-posed must be explicitly labeled.

### Article IX: Status Clarity (NEW v3.2)

Planning status (`plan_status`) and execution status (`run_status`) MUST be separate. Survivorship calculations MUST be unambiguous.

------------------------------------------------------------------------

## 4. Technical Specifications

### 4.1 Canonical λ-Representation

**Rule:** λ values MUST be provided as character strings in plans.

``` r
# Correct
lambda_grid = c("30", "10", "5", "3", "2.5")

# Deprecated (allowed for backward compat, but canonicalized to string)
lambda_grid = c(30, 10, 5, 3, 2.5)
```

**Canonicalization (v3.1):**

``` r
cs_canonicalize_lambda <- function(lambda) {
  if (is.character(lambda)) {
    return(lambda)
  } else if (is.numeric(lambda)) {
    # Use sprintf for consistent formatting (avoids signif scientific notation)
    return(sprintf("%.15g", lambda))
  } else {
    stop("lambda must be character or numeric")
  }
}
```

**Rationale:** `sprintf("%.15g", lambda)` produces consistent decimal representation across platforms, unlike `signif()` which can produce scientific notation.

**Validation:** At planning time, check that provided λ is in declared grid (warn if not, unless `allow_off_grid = TRUE`).

### 4.2 Scale Mode Specification

**Problem:** Normalizing by sample SD creates dependent draws and removes variance fluctuation.

**Solution:** Normalize by **theoretical** SD.

``` r
scale_mode ∈ {"l1_core", "l2_var"}
```

#### 4.2.1 L2-Var (Theoretical Variance = 1)

**Uses theoretical SD, not sample SD:**

``` r
generate_noise_l2var <- function(n, lambda) {
  df <- as.numeric(lambda)
  
  if (df <= 2) {
    stop("L2-var scaling requires df > 2 (finite variance)")
  }
  
  # v3.2: Add warning for near-2 regimes
  if (df < 2.5) {
    warning(sprintf(
      "df = %.3f is close to 2. Theoretical SD = %.2f. ",
      "This creates high scaling factors. ",
      "Consider df >= 2.5 for genuine tail stress or use l1_core scaling.",
      df, sqrt(df / (df - 2))
    ))
  }
  
  # Theoretical standard deviation of t_df
  theoretical_sd <- sqrt(df / (df - 2))
  
  # Generate raw noise
  raw_noise <- rt(n, df = df)
  
  # Scale by theoretical SD (not sample SD!)
  noise <- raw_noise / theoretical_sd
  
  # Result: E[noise²] = 1, but sample variance fluctuates naturally
  return(noise)
}
```

**Rationale:** - Preserves **distributional stress** (variance of the variance) - Fixes **expected** L2 scale, not **realized** scale - Sample variance σ̂² \~ χ²/(n-1) still fluctuates - Heavy tails increase σ̂² volatility → genuine challenge for estimators

**Interpretation:** - `l2_var`: Tests estimators under fixed **expected** L2 SNR - L2-based estimators still degrade due to variance instability - Quantile-based estimators benefit from core scale stability

#### 4.2.2 L1-Core (Robust Scale = 1)

``` r
generate_noise_l1core <- function(n, lambda) {
  df <- as.numeric(lambda)
  
  # Generate raw noise
  raw_noise <- rt(n, df = df)
  
  # Scale by MAD (median absolute deviation)
  # MAD = median(|X - median(X)|)
  robust_scale <- mad(raw_noise, constant = 1)  # constant=1 for consistency
  
  noise <- raw_noise / robust_scale
  
  # Result: Robust scale ≈ 1, but L2 scale can explode
  return(noise)
}
```

**Use case:** For df ≤ 2 (infinite variance) or to isolate tail robustness from SNR.

### 4.3 Status Taxonomy (v3.2 REVISED)

**CRITICAL CHANGE:** Split status into two columns for clarity.

Every task has:

``` r
plan_status ∈ {"eligible", "ineligible", "skipped"}
run_status ∈ {"not_run", "success", "fail"}
failure_mode ∈ {
  NA_character_,          # run_status = "success" or "not_run"
  "estimator_crash",      # R error during estimation
  "estimator_na",         # returned NA estimate
  "ci_failure",           # CI computation failed
  "timeout",              # max_runtime exceeded
  "numerical_overflow",   # Inf weights/estimates
  "numerical_underflow",  # propensity → 0
  "convergence_failure",  # optimizer didn't converge
  "oracle_root_failure",  # QST quantile inversion failed
  "ineligible_regime"     # violates moment requirements
}
failure_detail = ""       # short diagnostic string
```

**Status Definitions:**

**Plan Status:** - **eligible**: Task can be executed (meets moment/capability requirements) - **ineligible**: Task cannot be defined in this regime (e.g., OLS when variance infinite) - **skipped**: Task excluded for other reasons (debugging, cost)

**Run Status:** - **not_run**: Task has not been executed yet (or ineligible) - **success**: Estimator returned finite estimate and (if requested) valid CI - **fail**: Estimator attempted execution but failed

**State Transitions:**

```         
Planning Stage:
  → plan_status = "eligible" (default)
  → plan_status = "ineligible" (violates requirements)
  → run_status = "not_run" (all tasks)

Execution Stage (only for plan_status == "eligible"):
  eligible + not_run → success (estimator completed)
  eligible + not_run → fail (estimator crashed)
  
Ineligible tasks:
  → run_status remains "not_run" (never executed)
```

**Contract:** - Estimators MUST return `estimate = NA` with explicit `failure_mode` (never silent drops) - Planning stage MUST set `plan_status` before execution - Oracle failures (e.g., quantile inversion) MUST be recorded in `failure_mode`

------------------------------------------------------------------------

## 5. Estimand Definitions

**CRITICAL:** Formal definitions required for scientific validity.

### 5.1 Average Treatment Effect on the Treated (ATT)

**Population Estimand:**

$$
\text{ATT} = \mathbb{E}[\tau(X) \mid W = 1] = \mathbb{E}[Y^1 - Y^0 \mid W = 1]
$$

**Computational Form (1D Index Model):**

Given: - Index: $Z = X\beta$, where $Z \sim \mathcal{N}(0, 1)$ **in population** - Selection: $e(Z) = \text{logit}^{-1}(\alpha Z)$ - Treatment effect: $\tau(Z) = \tau_0 + \tau_1 Z$

Then:

$$
\text{ATT} = \int \tau(z) \cdot f(z \mid W=1) \, dz
$$

where the conditional density is:

$$
f(z \mid W=1) = \frac{e(z) \phi(z)}{\int e(z') \phi(z') \, dz'}
$$

**Oracle Implementation:** See Section 6.3.

### 5.2 Quantile Shift Treatment Effect (QST)

**Population Estimand:**

For quantile index $\tau \in (0, 1)$:

$$
\text{QST}(\tau) = Q_\tau(Y^1 \mid W=1) - Q_\tau(Y^0 \mid W=1)
$$

where $Q_\tau(Y^w \mid W=1)$ is the $\tau$-th quantile of the counterfactual distribution $Y^w$ conditional on treatment.

**Why this definition:** - ATT-aligned (conditions on $W=1$) - Reveals effect heterogeneity across outcome distribution - GenGC target estimand

**Key Property:** Under heterogeneous effects, QST(0.5) ≠ ATT in general. The median shift is not equal to the mean shift for skewed conditional distributions.

**Computational Challenge:** - $Y^w \mid W=1$ is a **mixture distribution** over $Z \mid W=1$ - For each $z$, $Y^w = \mu_w(z) + \varepsilon$ where $\varepsilon \sim t_\nu$ - Mixture quantiles require: (1) compute mixture CDF, (2) invert numerically

**Oracle Implementation:** See Section 6.4.

### 5.3 Complete Structural Functions (for debugging)

CausalStress also computes:

-   **QTE** (Quantile Treatment Effect): $Q_\tau(Y^1) - Q_\tau(Y^0)$ (unconditional)
-   **Distribution shift**: Full CDF comparison under treatment/control

These are computed when `oracle = TRUE` for validation purposes.

------------------------------------------------------------------------

## 6. Truth Precision Contract

### 6.1 Determinism Requirements

All oracle computations MUST be:

1.  **Numerically reproducible** up to declared tolerances given (family_id, version, lambda, scale_mode, oracle_config)
2.  **Tolerance-level cross-platform** (not bitwise-identical)
3.  **Version-stable** (same oracle_config always produces same truth within tolerance)

**Explicit Non-Guarantee (v3.1):** \> "CausalStress does NOT guarantee bitwise-identical oracle truth across different platforms, R versions, or BLAS implementations. Statistical functions (`dnorm`, `pt`, `qnorm`, `uniroot`) may differ at the \~1e-12 level due to library implementations. All comparisons use relative tolerance of 1e-5 or absolute tolerance of 1e-6."

### 6.2 Numerical Tolerances

``` r
oracle_config <- list(
  # Absolute tolerance (for near-zero estimates)
  tol_abs = 1e-6,
  
  # Relative tolerance (for large estimates)
  tol_rel = 1e-5,
  
  # Domain truncation strategy
  domain_strategy = "adaptive",  # "fixed" | "adaptive"
  
  # For adaptive strategy (heavy tails)
  tail_prob_max = 1e-8,
  
  # Quadrature grid size (1D integration)
  n_quad = 10000,
  
  # Root-finding tolerance (for quantile inversion)
  root_tol = 1e-7,
  root_maxiter = 1000,
  
  # QMC node count (if used)
  n_qmc = 50000,
  qmc_seed = 20250108L  # Fixed for determinism
)
```

### 6.2.1 Adaptive Domain Selection

**Problem:** Heavy-tailed distributions require wider integration domains than Gaussian. Using `qnorm()` truncates tails prematurely.

**Solution:** Adapt domain to the distribution being integrated.

``` r
cs_determine_integration_domain <- function(dist_type, df, tail_prob_max) {
  # For Z ~ N(0,1) in the mixture
  # Use normal quantiles for the mixing variable
  L_z <- qnorm(1 - tail_prob_max / 2)
  
  # For t-distributed noise, determine separately if needed
  if (dist_type == "t" && !is.null(df)) {
    # For heavy tails, may need wider domain for outcome space
    # But Z is always N(0,1), so L_z is fine
  }
  
  list(lower = -L_z, upper = L_z)
}
```

**Note:** For the confounding index Z, we use `qnorm()` because Z is constructed to be N(0,1) in population. The t-distribution appears only in the outcome noise ε, which is integrated over Z.

### 6.3 ATT Oracle (1D Quadrature)

**Specification:**

``` r
heavytail_t_att_oracle <- function(lambda, 
                                   scale_mode,
                                   oracle_config = cs_oracle_config_default()) {
  df <- as.numeric(lambda)
  
  # Extract config
  n_quad <- oracle_config$n_quad
  tail_prob <- oracle_config$tail_prob_max
  
  # Determine integration domain [-L, L]
  # Z ~ N(0,1) by construction, so use qnorm
  L <- qnorm(1 - tail_prob / 2)
  
  # Quadrature grid (deterministic)
  z_grid <- seq(-L, L, length.out = n_quad)
  dz <- z_grid[2] - z_grid[1]
  
  # Treatment effect function
  tau_z <- 1 + 0.5 * z_grid  # τ(Z) = 1 + 0.5Z
  
  # Propensity function
  alpha <- 0.5  # Selection strength
  e_z <- plogis(alpha * z_grid)
  
  # Marginal density of Z
  phi_z <- dnorm(z_grid)
  
  # Conditional density f(z | W=1) (unnormalized)
  f_z_w1_unnorm <- e_z * phi_z
  
  # Normalization constant
  norm_const <- sum(f_z_w1_unnorm) * dz
  
  # Conditional density (normalized)
  f_z_w1 <- f_z_w1_unnorm / norm_const
  
  # ATT = ∫ τ(z) f(z|W=1) dz
  att_truth <- sum(tau_z * f_z_w1) * dz
  
  # Precision check: integration weights should sum to 1
  integral_check <- sum(f_z_w1) * dz
  if (abs(integral_check - 1.0) > oracle_config$tol_abs) {
    warning("ATT oracle: density integration error = ", 
            abs(integral_check - 1.0))
  }
  
  return(att_truth)
}
```

### 6.4 QST Oracle (Mixture Quantile Inversion - v3.2 CORRECTED)

**CRITICAL FIX:** Corrected mixture variance calculation.

**Specification:**

``` r
heavytail_t_qst_oracle <- function(lambda,
                                   scale_mode,
                                   tau_grid = cs_tau_default(),
                                   oracle_config = cs_oracle_config_default()) {
  df <- as.numeric(lambda)
  
  # Get conditional density of Z|W=1 (reuse ATT computation)
  z_setup <- compute_z_conditional_density(lambda, oracle_config)
  z_grid <- z_setup$z_grid
  f_z_w1 <- z_setup$f_z_w1
  dz <- z_setup$dz
  
  # For each z, Y^w = μ_w(z) + ε, where ε ~ scaled t_df
  # Get noise scale based on scale_mode
  if (scale_mode == "l2_var") {
    if (df <= 2) stop("QST oracle: L2-var requires df > 2")
    noise_scale <- sqrt(df / (df - 2))
  } else if (scale_mode == "l1_core") {
    # MAD of t_df (numerically computed once, cached)
    noise_scale <- get_t_mad_cached(df)
  }
  
  # Structural functions
  mu0_z <- 1 + z_grid + 0.5 * z_grid^2  # μ₀(Z)
  tau_z <- 1 + 0.5 * z_grid             # τ(Z)
  mu1_z <- mu0_z + tau_z                # μ₁(Z)
  
  # For each τ ∈ tau_grid, compute QST(τ)
  qst_values <- vapply(tau_grid, function(tau) {
    # Q_τ(Y^0 | W=1) = mixture quantile
    q0 <- compute_mixture_quantile(
      tau = tau,
      location = mu0_z,
      scale = noise_scale,
      df = df,
      mixture_weights = f_z_w1 * dz,
      config = oracle_config
    )
    
    # Q_τ(Y^1 | W=1)
    q1 <- compute_mixture_quantile(
      tau = tau,
      location = mu1_z,
      scale = noise_scale,
      df = df,
      mixture_weights = f_z_w1 * dz,
      config = oracle_config
    )
    
    # QST(τ) = Q_τ(Y^1|W=1) - Q_τ(Y^0|W=1)
    q1 - q0
  }, FUN.VALUE = numeric(1))
  
  # Return tibble
  tibble::tibble(
    tau_id = cs_tau_id(tau_grid),
    tau = tau_grid,
    value = qst_values
  )
}

#' Compute Mixture Quantile (v3.2 CORRECTED)
#'
#' @param tau Quantile index (0, 1)
#' @param location Vector of mixture component locations
#' @param scale Scale parameter for t-distribution
#' @param df Degrees of freedom
#' @param mixture_weights Weights (must sum to 1)
#' @param config Oracle configuration
compute_mixture_quantile <- function(tau, 
                                     location, 
                                     scale, 
                                     df,
                                     mixture_weights,
                                     config) {
  # Y | W=1 ~ Σ_i w_i · t_df(loc=location[i], scale=scale)
  # Find q such that P(Y ≤ q | W=1) = τ
  
  # Define CDF of mixture
  mixture_cdf <- function(y) {
    # P(Y ≤ y | W=1) = Σ_i w_i · P(t_df ≤ (y - location[i])/scale)
    sum(mixture_weights * pt((y - location) / scale, df = df))
  }
  
  # CORRECTED: Proper mixture variance calculation (v3.2)
  y_mean <- sum(mixture_weights * location)
  
  if (df > 2) {
    # Var(mixture) = Var(locations) + Var(noise)
    var_location <- sum(mixture_weights * (location - y_mean)^2)
    var_noise <- scale^2 * df / (df - 2)
    y_var <- var_location + var_noise
    y_sd <- sqrt(y_var)
  } else {
    # Use IQR-based pseudo-SD when variance doesn't exist
    # Use simple bisection for IQR (deterministic, no recursion)
    q25 <- compute_mixture_quantile_bisection(0.25, location, scale, df, 
                                               mixture_weights)
    q75 <- compute_mixture_quantile_bisection(0.75, location, scale, df, 
                                               mixture_weights)
    y_iqr <- q75 - q25
    y_sd <- y_iqr / 1.349  # Convert IQR to pseudo-SD
  }
  
  # Bracketing with corrected SD
  lower <- y_mean - 20 * y_sd
  upper <- y_mean + 20 * y_sd
  
  # CRITICAL CHECK: Verify bracket contains quantile
  cdf_lower <- mixture_cdf(lower)
  cdf_upper <- mixture_cdf(upper)
  
  if (cdf_lower > tau || cdf_upper < tau) {
    # Expand and retry once
    lower <- y_mean - 50 * y_sd
    upper <- y_mean + 50 * y_sd
    cdf_lower <- mixture_cdf(lower)
    cdf_upper <- mixture_cdf(upper)
    
    if (cdf_lower > tau || cdf_upper < tau) {
      stop(sprintf(
        "QST oracle: Quantile tau=%.3f not bracketed. CDF(lower)=%.6f, CDF(upper)=%.6f",
        tau, cdf_lower, cdf_upper
      ))
    }
    
    warning(sprintf(
      "QST oracle: Expanded bracket for tau=%.3f (df=%.1f, heavy tails)",
      tau, df
    ))
  }
  
  # Root finding (uniroot)
  result <- tryCatch(
    uniroot(
      f = function(y) mixture_cdf(y) - tau,
      lower = lower,
      upper = upper,
      tol = config$root_tol,
      maxiter = config$root_maxiter
    ),
    error = function(e) {
      stop(sprintf(
        "QST oracle: Root-finding failed for tau=%.3f: %s",
        tau, e$message
      ))
    }
  )
  
  # Check convergence quality
  residual <- abs(mixture_cdf(result$root) - tau)
  if (residual > config$tol_abs) {
    warning(sprintf(
      "QST oracle: Root residual %.2e exceeds tolerance for tau=%.3f",
      residual, tau
    ))
  }
  
  result$root
}

#' Helper: Bisection for IQR Computation (v3.2 NEW)
#' 
#' Deterministic, non-recursive helper for computing mixture quantiles
#' when variance doesn't exist (df <= 2). Used for IQR-based SD estimation.
#' 
#' Contract:
#' - Deterministic given inputs
#' - No recursion (doesn't call compute_mixture_quantile)
#' - Fixed max iterations
#' - Returns best estimate if not converged (no error)
#' - Monotone-safe for mixture CDF
compute_mixture_quantile_bisection <- function(tau, 
                                               location, 
                                               scale, 
                                               df,
                                               mixture_weights,
                                               maxiter = 100, 
                                               tol = 1e-6) {
  # Simple bisection for IQR computation
  mixture_cdf <- function(y) {
    sum(mixture_weights * pt((y - location) / scale, df = df))
  }
  
  # Wide initial bracket
  y_mean <- sum(mixture_weights * location)
  lower <- y_mean - 100 * scale
  upper <- y_mean + 100 * scale
  
  # Verify bracket
  if (mixture_cdf(lower) > tau || mixture_cdf(upper) < tau) {
    # Expand further
    lower <- y_mean - 500 * scale
    upper <- y_mean + 500 * scale
  }
  
  # Bisection
  for (i in 1:maxiter) {
    mid <- (lower + upper) / 2
    cdf_mid <- mixture_cdf(mid)
    
    if (abs(cdf_mid - tau) < tol) {
      return(mid)
    }
    
    if (cdf_mid < tau) {
      lower <- mid
    } else {
      upper <- mid
    }
  }
  
  # Return best estimate (no error on non-convergence)
  (lower + upper) / 2
}
```

### 6.5 Validation Suite (REVISED)

Every oracle MUST pass:

``` r
test_oracle_validity <- function(family_id, lambda, scale_mode) {
  oracle_result <- cs_oracle_truth(family_id, lambda, scale_mode)
  
  # V1: ATT is finite
  expect_true(is.finite(oracle_result$att))
  
  # V2: QST is weakly monotone in tau
  qst <- oracle_result$qst$value
  expect_true(all(diff(qst) >= -1e-6))  # Weak monotone
  
  # V3: REMOVED (was invalid for heterogeneous effects)
  # DO NOT test QST(0.5) ≈ ATT for heterogeneous τ(Z)
  
  # V4: Limiting case (df → ∞ approaches Gaussian)
  if (as.numeric(lambda) > 20) {
    oracle_gaussian <- cs_oracle_truth(family_id, "1000", scale_mode)
    expect_equal(oracle_result$att, oracle_gaussian$att, tolerance = 0.01)
  }
  
  # V5: Regression test (pinned truth value with tolerance)
  expected <- get_regression_truth(family_id, lambda, scale_mode)
  if (!is.null(expected)) {
    # v3.2: Use stored tolerance
    tol <- max(expected$att_tol_rel, expected$att_tol_abs)
    expect_equal(oracle_result$att, expected$att, tolerance = tol)
  }
  
  # V6 (IMPROVED v3.2): QST ordering for monotone τ(Z)
  # For τ(Z) = 1 + 0.5Z (monotone increasing):
  # Higher quantiles should have larger effects
  qst_10 <- qst[which.min(abs(oracle_result$qst$tau - 0.1))]
  qst_90 <- qst[which.min(abs(oracle_result$qst$tau - 0.9))]
  
  expect_true(qst_90 > qst_10,
              info = "QST should increase for monotone τ(Z)")
  
  # Effect size should be meaningful (not just numerical noise)
  effect_size <- qst_90 - qst_10
  expect_true(effect_size > 0.1,
              info = sprintf("QST range too small: %.4f", effect_size))
  
  # Test rank correlation
  cor_tau_qst <- cor(oracle_result$qst$tau, qst, method = "spearman")
  expect_true(cor_tau_qst > 0.8,
              info = sprintf("QST-tau correlation too low: %.3f", cor_tau_qst))
}

# V7: Homogeneous effects test (separate DGP)
test_oracle_homogeneous_case <- function() {
  # Define special DGP with constant treatment effect
  # τ(Z) = 1 (no heterogeneity)
  # For this case, QST(τ) ≈ ATT ∀τ should hold
  
  oracle_homo <- cs_oracle_truth("heavytail_noise_t_homogeneous", "10", "l2var")
  qst_median <- oracle_homo$qst$value[oracle_homo$qst$tau == 0.5]
  
  expect_equal(qst_median, oracle_homo$att, tolerance = 0.05)
}
```

**Regression Truth Storage (v3.2 ENHANCED):**

``` r
# Stored in inst/oracle_regression_tests.rds
# v3.2: Now includes tolerance metadata
regression_truths <- list(
  "heavytail_noise_t" = list(
    "30_l2var" = list(
      att = 1.0234567,
      att_tol_rel = 1e-5,       # NEW: relative tolerance
      att_tol_abs = 1e-6,       # NEW: absolute tolerance
      qst = c(...),
      qst_tol_rel = 1e-5,
      qst_tol_abs = 1e-6,
      oracle_config_fingerprint = "abc123...",  # NEW: config identity
      version = "1.0.0",
      r_version = "4.3.2",
      platform = "x86_64-pc-linux-gnu",
      updated = "2025-01-08",
      updated_by = "max",
      reason = "initial"
    ),
    "10_l2var" = list(...),
    "5_l2var" = list(...),
    "3_l2var" = list(...)
  )
)
```

------------------------------------------------------------------------

## 7. Fingerprint Taxonomy

**Three-tier system:** Family → Instance → Run

### 7.1 Family Fingerprint

Identifies the **conceptual family** and its implementation version.

``` r
cs_build_family_fingerprint <- function(
  family_id,
  family_version,
  family_metadata,
  factory_source_hash,   # digest(deparse(factory_function))
  oracle_source_hash     # digest(deparse(oracle_function))
) {
  digest::digest(
    list(
      schema_version = 3L,
      family_id = as.character(family_id),
      family_version = as.character(family_version),
      metadata = normalize_for_fingerprint(family_metadata),
      factory_hash = factory_source_hash,
      oracle_hash = oracle_source_hash
    ),
    algo = "sha256"
  )
}
```

**Usage:** Identifies family for citation, registry lookup, version control.

### 7.2 Instance Fingerprint

Identifies a **specific λ-configuration** (reusable across seeds and sample sizes).

``` r
cs_build_instance_fingerprint <- function(
  family_fingerprint,
  lambda_canonical,      # Character string: "3" or "2.5"
  scale_mode,            # "l1_core" or "l2_var"
  dgp_config             # v3.2: Serialized defaults
) {
  # CRITICAL: Do NOT include n or seed
  # Instance identity is independent of sample size and random seed
  
  digest::digest(
    list(
      schema_version = 3L,
      family_fingerprint = family_fingerprint,
      lambda_canonical = as.character(lambda_canonical),
      scale_mode = as.character(scale_mode),
      dgp_config_fingerprint = digest::digest(
        normalize_for_fingerprint(dgp_config)
      )
    ),
    algo = "sha256"
  )
}
```

**Usage:** Oracle truth caching, kill-plot aggregation, instance-level resume.

**Critical Property:** Same instance fingerprint → same oracle truth.

### 7.3 Run Fingerprint

Identifies a **single execution** (unique per seed and sample size).

``` r
cs_build_run_fingerprint <- function(
  instance_fingerprint,
  estimator_id,
  estimator_version,
  n,
  seed,
  estimator_config
) {
  digest::digest(
    list(
      schema_version = 3L,
      instance_fingerprint = instance_fingerprint,
      estimator_id = as.character(estimator_id),
      estimator_version = as.character(estimator_version),
      n = as.integer(n),
      seed = as.integer(seed),
      estimator_config_fingerprint = digest::digest(
        normalize_for_fingerprint(estimator_config)
      )
    ),
    algo = "sha256"
  )
}
```

**Usage:** Result pins, crash-recovery, exact run identification.

### 7.4 Campaign Fingerprint (v3.2 CORRECTED)

Identifies the **scientific plan** (timestamp-free for reproducibility).

**CRITICAL CHANGE (v3.2):** Single-n enforcement for MVP.

``` r
cs_build_campaign_fingerprint <- function(
  dgp_families,        # list(family_id = lambda_grid)
  estimator_ids,
  n,                   # v3.2: Must be scalar for v0.2.0 MVP
  strategy_map,
  schema_version = 3L
) {
  # CRITICAL: No timestamp, no random seed, no hardware ID
  # Campaign identity is a pure function of the plan
  
  # v3.2: Enforce single-n for MVP
  if (length(n) > 1) {
    stop("v0.2.0 MVP supports single-n campaigns only. ",
         "For multi-n studies, run separate campaigns. ",
         "Multi-n support planned for v0.3.0.")
  }
  
  # Sort for determinism
  dgp_families <- dgp_families[order(names(dgp_families))]
  dgp_families <- lapply(dgp_families, function(grid) {
    sort(as.character(grid))  # Canonical order
  })
  estimator_ids <- sort(estimator_ids)
  
  digest::digest(
    list(
      schema_version = as.integer(schema_version),
      dgp_families = dgp_families,
      estimator_ids = estimator_ids,
      n = as.integer(n),  # Scalar for v0.2.0
      strategy_map = normalize_for_fingerprint(strategy_map)
    ),
    algo = "sha256"
  )
}
```

**Rationale (v3.2):** Sample size is part of the experimental design. For MVP, we enforce single-n campaigns to avoid ambiguity in fingerprinting. Multi-n support will be added in v0.3.0 with proper n_grid canonicalization.

**Usage:** - Paper citations: "Campaign A (SHA256: abc123...)" - Kill-plot provenance - Detecting plan changes

**Separate:** Campaign *metadata* includes timestamp, git hash, etc. (provenance, not identity).

``` r
campaign_metadata <- list(
  campaign_id = "abc123...",  # From fingerprint (deterministic)
  timestamp = Sys.time(),      # Provenance
  git_hash = system("git rev-parse HEAD", intern = TRUE),
  hostname = Sys.info()["nodename"],
  r_version = R.version.string,
  ...
)
```

------------------------------------------------------------------------

## 8. Family Specifications

### 8.1 Heavy-Tailed Outcome Noise (Student-t)

**Family ID:** `heavytail_noise_t`\
**Version:** `1.0.0`

#### 8.1.1 Structural Model (v3.2 CORRECTED)

**CRITICAL FIX (v3.2):** Explicit Z weights with theoretical SD.

``` r
heavytail_t_factory <- function(lambda, n, seed, scale_mode = "l2_var",
                                dgp_config = list()) {
  set.seed(seed)
  df <- as.numeric(lambda)
  
  # Covariates (p=5)
  p <- 5
  X <- matrix(rnorm(n * p), n, p)
  # ASSUMES: Covariates are independent N(0,1)
  # If this assumption changes, update theoretical_var_Z calculation below
  
  # Index construction (v3.2: explicit weights)
  # Z = X₁ + 0.5 X₂
  z_weights <- c(1, 0.5, 0, 0, 0)  # Only X₁ and X₂ contribute
  
  # Theoretical variance: Var(Z) = Σ w²ᵢ (under independence)
  theoretical_var_Z <- sum(z_weights^2)  # = 1 + 0.25 = 1.25
  theoretical_sd_Z <- sqrt(theoretical_var_Z)  # = sqrt(1.25) ≈ 1.118
  
  # Construct index
  raw_Z <- X %*% z_weights
  
  # CRITICAL: Use theoretical SD, not sample SD
  Z <- as.vector(raw_Z / theoretical_sd_Z)
  
  # Now: E[Z] = 0, Var(Z) = 1 (in population)
  # Sample variance of Z will fluctuate naturally around 1
  
  # Propensity (logistic selection on index)
  alpha <- 0.5  # Selection strength
  e_X <- plogis(alpha * Z)
  W <- rbinom(n, 1, e_X)
  
  # Outcome structural functions
  mu0 <- 1 + Z + 0.5 * Z^2    # Nonlinear baseline
  tau <- 1 + 0.5 * Z          # Heterogeneous treatment effect
  
  # Noise (scale according to mode)
  if (scale_mode == "l2_var") {
    if (df <= 2) {
      stop("L2-var scaling requires df > 2")
    }
    
    # v3.2: Add warning for near-2 regimes
    if (df < 2.5) {
      warning(sprintf(
        "df = %.3f is close to 2. Theoretical SD = %.2f. ",
        "This creates high scaling factors. ",
        "Consider df >= 2.5 for genuine tail stress or use l1_core scaling.",
        df, sqrt(df / (df - 2))
      ))
    }
    
    theoretical_sd <- sqrt(df / (df - 2))
    eps <- rt(n, df = df) / theoretical_sd
  } else if (scale_mode == "l1_core") {
    raw_eps <- rt(n, df = df)
    eps <- raw_eps / mad(raw_eps, constant = 1)
  } else {
    stop("Unknown scale_mode: ", scale_mode)
  }
  
  # Potential outcomes
  Y0 <- mu0 + eps
  Y1 <- mu0 + tau + eps  # Same noise realization (SUTVA)
  
  # Observed outcome
  Y <- ifelse(W == 1, Y1, Y0)
  
  # Oracle columns (stripped by airlock before estimator sees data)
  oracle_cols <- list(
    y0 = Y0,
    y1 = Y1,
    p = e_X,
    structural_te = tau,
    z_index = Z  # For debugging/validation
  )
  
  # Return DGP
  list(
    df = data.frame(
      y = Y,
      w = W,
      X
    ),
    oracle = oracle_cols,
    meta = list(
      family_id = "heavytail_noise_t",
      lambda = lambda,
      scale_mode = scale_mode,
      n = n,
      seed = seed,
      z_weights = z_weights,               # v3.2: For validation
      theoretical_sd_Z = theoretical_sd_Z, # v3.2: For validation
      covariate_assumption = "independent N(0,1)"  # v3.2: Document assumption
    )
  )
}
```

**Validation check (v3.2 NEW):**

``` r
test_that("Z distribution matches oracle assumption", {
  dgp <- heavytail_t_factory(lambda = "10", n = 10000, seed = 42, 
                              scale_mode = "l2_var")
  Z <- dgp$oracle$z_index
  
  # Sample mean should be near 0
  expect_equal(mean(Z), 0, tolerance = 0.05,
               info = "Z mean should be near 0")
  
  # Sample variance should be near 1 (but not exactly 1)
  expect_equal(var(Z), 1, tolerance = 0.1,
               info = "Z variance should be near 1")
  
  # CRITICAL: Z variance should NOT be exactly 1
  # (that would indicate sample SD bug)
  expect_true(abs(var(Z) - 1) > 0.005,
              info = "Z variance should fluctuate (not forced to 1)")
  
  # Verify theoretical SD calculation
  z_weights <- dgp$meta$z_weights
  theoretical_var <- sum(z_weights^2)
  expect_equal(dgp$meta$theoretical_sd_Z, sqrt(theoretical_var), 
               tolerance = 1e-10,
               info = "Theoretical SD calculation correct")
  
  # Verify covariate assumption documented
  expect_equal(dgp$meta$covariate_assumption, "independent N(0,1)")
})
```

#### 8.1.2 Oracle Function

See Section 6.3 and 6.4 for full specification.

``` r
heavytail_t_oracle <- function(lambda, 
                               scale_mode,
                               oracle_config = cs_oracle_config_default()) {
  att_truth <- heavytail_t_att_oracle(lambda, scale_mode, oracle_config)
  qst_truth <- heavytail_t_qst_oracle(lambda, scale_mode, 
                                      cs_tau_default(), oracle_config)
  
  list(
    att = att_truth,
    qst = qst_truth,
    config = oracle_config
  )
}
```

#### 8.1.3 Family Metadata (v3.2 ENHANCED)

``` r
heavytail_t_metadata <- list(
  lambda_name = "degrees_of_freedom",
  lambda_symbol = "ν",
  lambda_scale = "log",
  lambda_grid_default = c("30", "10", "5", "3", "2.5"),
  scale_modes = c("l2_var"),  # v0.2.0: only finite-variance regime
  
  # v3.2: DGP config defaults (empty for this family)
  dgp_config_defaults = list(),
  
  moment_regime = function(lambda, scale_mode) {
    df <- as.numeric(lambda)
    list(
      mean_exists = df > 1,
      variance_exists = df > 2,
      att_estimable = df > 1,
      rmse_valid = (df > 2 && scale_mode == "l2_var"),
      oracle_method = "quadrature"
    )
  },
  
  recommended_range = c(2.5, 30),  # v3.2: Note lower bound
  death_zone_expected = c(1.5, 2.5),
  
  interpretation = list(
    primary = "Tail thickness (lower ν = heavier tails)",
    confound = "L2 SNR fixed via theoretical variance (not sample variance)",
    estimator_prediction = "L2 estimators degrade due to variance instability",
    near_2_caveat = "df < 2.5 creates high scaling factors; use with caution"  # v3.2
  ),
  
  reference = "Student (1908). The probable error of a mean."
)
```

------------------------------------------------------------------------

## 9. Architectural Implementation

### 9.1 Dual Registry Pattern (v3.2 ENHANCED)

``` r
# Point DGP Registry (unchanged from v0.1.x)
cs_dgp_registry() %>%
  select(dgp_id, version, generator, oracle, meta, status)

# Family Registry (v3.2: added dgp_config_defaults)
cs_dgp_family_registry() %>%
  select(
    family_id, 
    family_version, 
    family_fingerprint,
    lambda_name,
    lambda_symbol,
    lambda_grid_default,
    scale_modes,
    moment_regime,
    factory_function,
    oracle_function,
    dgp_config_defaults,  # v3.2: NEW
    metadata,
    status
  )
```

### 9.2 Dispatcher

``` r
cs_get_dgp <- function(id, 
                       lambda = NULL, 
                       scale_mode = NULL,
                       version = NULL,
                       status = "stable") {
  # Try point registry first
  if (id %in% cs_dgp_registry()$dgp_id) {
    if (!is.null(lambda)) {
      warning("Point DGP ignores lambda parameter")
    }
    return(cs_resolve_dgp_point(id, version, status))
  }
  
  # Try family registry
  if (id %in% cs_dgp_family_registry()$family_id) {
    if (is.null(lambda)) {
      # Use default
      family <- cs_get_family_record(id, version, status)
      lambda <- family$lambda_grid_default[1]
      message("Using default lambda = ", lambda)
    }
    if (is.null(scale_mode)) {
      family <- cs_get_family_record(id, version, status)
      scale_mode <- family$scale_modes[1]
      message("Using default scale_mode = ", scale_mode)
    }
    return(cs_resolve_dgp_family(id, lambda, scale_mode, version))
  }
  
  stop("Unknown DGP or family: ", id)
}
```

------------------------------------------------------------------------

## 10. Campaign Planning

### 10.1 Planning Interface (v3.2 REVISED)

**CRITICAL CHANGES:** - Enforce single-n for MVP - Serialize dgp_config defaults at planning time

``` r
cs_plan_stress_campaign <- function(
  dgp_families,          # list(family_id = lambda_grid)
  estimator_ids,         # character vector
  seeds,                 # integer vector
  n = 1000,              # v3.2: Must be scalar for MVP
  strategy_map = list(),
  eligibility = NULL,
  campaign_seed = NULL,
  batch_size = 50,
  ...
) {
  # v3.2: Enforce single-n for MVP
  if (length(n) > 1) {
    stop("v0.2.0 MVP supports single-n campaigns only. ",
         "For multi-n studies, run separate campaigns and merge manually. ",
         "Multi-n support planned for v0.3.0.")
  }
  
  # Validate inputs
  stopifnot(is.list(dgp_families))
  stopifnot(all(names(dgp_families) %in% cs_dgp_family_registry()$family_id))
  
  # v3.2: Expand families to instances (with dgp_config serialization)
  instances <- expand_families_to_instances(dgp_families, strategy_map)
  
  # Build full task grid
  tasks <- tidyr::expand_grid(
    instance_id = instances$instance_id,
    estimator_id = estimator_ids,
    n = n,
    seed = seeds
  ) %>%
    left_join(instances, by = "instance_id")
  
  # v3.2: Initialize plan_status and run_status
  tasks$plan_status <- "eligible"  # Default
  tasks$run_status <- "not_run"    # Not executed yet
  
  # v3.2: Apply eligibility filters (sets plan_status)
  tasks <- apply_eligibility_filters(tasks, eligibility)
  
  # Resolve per-task configs
  tasks$config <- pmap(tasks, resolve_task_config, strategy_map)
  
  # Assign fingerprints
  tasks$run_fingerprint <- pmap_chr(
    list(tasks$instance_fingerprint, tasks$estimator_id, 
         tasks$n, tasks$seed, tasks$config),
    cs_build_run_fingerprint
  )
  
  # Shuffle deterministically
  if (!is.null(campaign_seed)) {
    set.seed(campaign_seed)
    tasks <- tasks[sample(nrow(tasks)), ]
  }
  
  # Assign batches
  tasks$batch_id <- rep(seq_len(ceiling(nrow(tasks) / batch_size)),
                        each = batch_size, length.out = nrow(tasks))
  
  # Build campaign fingerprint (deterministic, no timestamp)
  # v3.2: Single-n enforced
  campaign_id <- cs_build_campaign_fingerprint(
    dgp_families = dgp_families,
    estimator_ids = estimator_ids,
    n = n,  # Scalar
    strategy_map = strategy_map,
    schema_version = 3L
  )
  
  # Attach campaign metadata (includes timestamp for provenance)
  attr(tasks, "campaign_id") <- campaign_id
  attr(tasks, "campaign_metadata") <- list(
    campaign_id = campaign_id,
    timestamp_created = Sys.time(),
    git_hash = tryCatch(
      system("git rev-parse HEAD", intern = TRUE),
      error = function(e) NA_character_
    ),
    r_version = R.version.string,
    schema_version = 3L,
    dgp_families = dgp_families,
    estimator_ids = estimator_ids,
    n = n,
    n_tasks = nrow(tasks),
    n_eligible = sum(tasks$plan_status == "eligible"),
    n_ineligible = sum(tasks$plan_status == "ineligible"),
    n_skipped = sum(tasks$plan_status == "skipped")
  )
  
  class(tasks) <- c("cs_campaign_plan", class(tasks))
  tasks
}
```

### 10.2 Instance Expansion (v3.2 NEW)

**CRITICAL:** Serialize dgp_config defaults.

``` r
expand_families_to_instances <- function(dgp_families, strategy_map) {
  instances <- list()
  
  for (family_id in names(dgp_families)) {
    lambda_grid <- dgp_families[[family_id]]
    family <- cs_get_family_record(family_id)
    
    for (lambda in lambda_grid) {
      for (scale_mode in family$scale_modes) {
        # v3.2: Serialize defaults into dgp_config
        dgp_config <- family$dgp_config_defaults  # Start with family defaults
        
        # Override with any family-specific strategy_map configs
        # (extract only DGP-related params, not estimator params)
        if (family_id %in% names(strategy_map$family)) {
          family_overrides <- strategy_map$family[[family_id]]
          
          # Only override params that exist in defaults
          dgp_params <- intersect(names(family_overrides), 
                                  names(family$dgp_config_defaults))
          if (length(dgp_params) > 0) {
            dgp_config <- modifyList(dgp_config, family_overrides[dgp_params])
          }
        }
        
        # Build instance
        instance <- list(
          instance_id = sprintf("%s_%s_%s", family_id, lambda, scale_mode),
          family_id = family_id,
          family_version = family$family_version,
          lambda = lambda,
          scale_mode = scale_mode,
          dgp_config = dgp_config,  # v3.2: Explicit, serialized config
          moment_regime = family$moment_regime(lambda, scale_mode),
          instance_fingerprint = cs_build_instance_fingerprint(
            family$family_fingerprint,
            lambda,
            scale_mode,
            dgp_config  # v3.2: Now complete and explicit
          )
        )
        
        instances[[length(instances) + 1]] <- instance
      }
    }
  }
  
  bind_rows(instances)
}
```

### 10.3 Strategy Map Syntax (Structured)

**No string parsing!** Use nested lists with explicit types.

``` r
strategy_map <- list(
  # Global defaults (all tasks)
  defaults = list(
    ci_method = "bootstrap",
    n_boot = 200,
    max_runtime = 300
  ),
  
  # Per-estimator overrides
  estimator = list(
    gengc = list(
      num_trees = 1200,
      n_draws = 2000
    ),
    ipw_att = list(
      trim = 0.01
    )
  ),
  
  # Per-family overrides (applies to all estimators on this family)
  family = list(
    heavytail_noise_t = list(
      max_runtime = 600  # More time for heavy tails
    )
  ),
  
  # Per-(estimator, family) overrides (most specific)
  # Use cs_override helper to avoid string parsing
  specific = list(
    cs_override(
      estimator = "gengc",
      family = "heavytail_noise_t",
      config = list(
        num_trees = function(lambda) {
          # Lambda-dependent tuning
          df <- as.numeric(lambda)
          if (df < 3) 2000 else 1200
        }
      )
    ),
    cs_override(
      estimator = "lm_att",
      family = "heavytail_overlap",
      config = list(
        enabled = FALSE
      )
    )
  )
)

# Helper function
cs_override <- function(estimator, family, config) {
  structure(
    list(
      estimator = estimator,
      family = family,
      config = config
    ),
    class = "cs_strategy_override"
  )
}
```

**Resolution logic:**

``` r
resolve_task_config <- function(instance, estimator_id, strategy_map) {
  # Start with defaults
  config <- strategy_map$defaults %||% list()
  
  # Apply estimator overrides
  if (estimator_id %in% names(strategy_map$estimator)) {
    config <- modifyList(config, strategy_map$estimator[[estimator_id]])
  }
  
  # Apply family overrides
  family_id <- instance$family_id
  if (family_id %in% names(strategy_map$family)) {
    config <- modifyList(config, strategy_map$family[[family_id]])
  }
  
  # Apply specific overrides
  for (override in strategy_map$specific) {
    if (inherits(override, "cs_strategy_override")) {
      if (override$estimator == estimator_id && 
          override$family == family_id) {
        config <- modifyList(config, override$config)
      }
    }
  }
  
  # Resolve lambda-dependent functions
  lambda <- instance$lambda
  config <- rapply(config, function(x) {
    if (is.function(x)) x(lambda) else x
  }, how = "replace")
  
  config
}
```

------------------------------------------------------------------------

## 11. Status and Eligibility

### 11.1 Two-Column Status System (v3.2 REVISED)

**CRITICAL CHANGE:** Separate planning and execution status.

``` r
# Planning stage:
plan_status ∈ {"eligible", "ineligible", "skipped"}

# Execution stage:
run_status ∈ {"not_run", "success", "fail"}

# Failure details (when run_status == "fail"):
failure_mode ∈ {
  "estimator_crash",
  "estimator_na",
  "ci_failure",
  "timeout",
  "numerical_overflow",
  "numerical_underflow",
  "convergence_failure",
  "oracle_root_failure"
}
```

**State Transitions:**

```         
Planning:
  → plan_status = "eligible" (default)
  → plan_status = "ineligible" (violates moment_regime)
  → plan_status = "skipped" (user exclusion)
  → run_status = "not_run" (all tasks initially)

Execution (only for plan_status == "eligible"):
  eligible + not_run → success (estimator completed)
  eligible + not_run → fail (estimator crashed)

Ineligible/Skipped:
  → run_status remains "not_run" (never executed)
```

### 11.2 Eligibility Filters (v3.2 REVISED)

**CRITICAL PRINCIPLE (Article VIII):** Eligibility rules MUST NOT encode favoritism. Always-eligible claims must be justified.

``` r
eligibility <- list(
  # OLS requires finite variance (clear theoretical requirement)
  lm_att = function(instance_meta) {
    moment <- instance_meta$moment_regime
    moment$variance_exists
  },
  
  # IPW requires overlap (for v0.2.0, no overlap families, so always eligible)
  ipw_att = function(instance_meta) {
    TRUE  # Will add overlap checks in v0.3.0
  },
  
  # GenGC eligibility (v3.2: justified, family-aware)
  gengc = function(instance_meta) {
    # Theoretical justification: GenGC uses quantile regression, which:
    # 1. Remains well-defined under infinite variance
    # 2. Converges to conditional quantiles regardless of tail thickness
    # 3. Does not require moment existence beyond E[|Y|] < ∞
    
    # For v0.2.0 heavytail_noise_t (df > 2.5), E[|Y|] exists
    moment <- instance_meta$moment_regime
    moment$att_estimable  # Equivalent to E[τ(X)|W=1] exists
    
    # v0.3.0 will add family-specific checks for overlap stress
  }
)
```

**Documenting Eligibility Claims:**

Each estimator should have documented eligibility policy in metadata:

``` r
cs_register_estimator(
  estimator_id = "gengc",
  ...,
  eligibility_policy = list(
    requirements = c("att_estimable"),
    justification = "Quantile regression remains well-defined under infinite variance",
    reference = "Koenker (2005). Quantile Regression.",
    family_specific = list(
      heavytail_noise_t = "Eligible for df > 1 (E[τ(X)|W=1] exists)",
      heavytail_overlap = "May require additional overlap checks (v0.3.0)"
    )
  )
)
```

### 11.3 Eligibility Application (v3.2 REVISED)

Sets `plan_status` column:

``` r
apply_eligibility_filters <- function(tasks, eligibility) {
  if (is.null(eligibility)) {
    tasks$plan_status <- "eligible"
    return(tasks)
  }
  
  tasks$plan_status <- pmap_chr(tasks, function(estimator_id, instance_meta, ...) {
    if (estimator_id %in% names(eligibility)) {
      filter_fn <- eligibility[[estimator_id]]
      is_eligible <- tryCatch(
        filter_fn(instance_meta),
        error = function(e) {
          warning("Eligibility check failed for ", estimator_id, ": ", e$message)
          FALSE
        }
      )
      if (is_eligible) "eligible" else "ineligible"
    } else {
      "eligible"  # No filter specified = eligible
    }
  })
  
  tasks
}
```

### 11.4 Result Status (v3.2 REVISED)

After execution:

``` r
result <- list(
  # v3.2: Two-column status
  plan_status = "eligible",     # Preserved from planning
  run_status = "success",       # or "fail" or "not_run"
  failure_mode = NA_character_, # Only if run_status == "fail"
  failure_detail = "",
  
  # Science payload
  est_att = 1.234,
  ci_att_lower = 1.100,
  ci_att_upper = 1.368,
  
  # Errors (computed if oracle available)
  error_att = est_att - truth$att,
  
  ...
)
```

**Key distinction (v3.2):** - `plan_status = "ineligible"` → not executed (planning decision) - `run_status = "fail"` → executed but crashed (runtime failure)

------------------------------------------------------------------------

## 12. Survivorship Bias Protocol

### 12.1 Success Rate Computation (v3.2 REVISED)

**Uses two-column status for clarity.**

For each (family, lambda, estimator):

``` r
cs_summarise_stress <- function(results, 
                                lambda,
                                estimator_id,
                                moment_regime) {
  # Filter to this lambda + estimator
  subset <- results %>%
    filter(lambda == !!lambda, estimator_id == !!estimator_id)
  
  # v3.2: Count by plan_status and run_status
  n_total <- nrow(subset)
  n_eligible <- sum(subset$plan_status == "eligible")
  n_ineligible <- sum(subset$plan_status == "ineligible")
  n_skipped <- sum(subset$plan_status == "skipped")
  
  # Among eligible tasks, count run outcomes
  eligible_subset <- subset %>% filter(plan_status == "eligible")
  n_executed <- sum(eligible_subset$run_status %in% c("success", "fail"))
  n_success <- sum(eligible_subset$run_status == "success", na.rm = TRUE)
  n_fail <- sum(eligible_subset$run_status == "fail", na.rm = TRUE)
  n_not_run <- sum(eligible_subset$run_status == "not_run", na.rm = TRUE)
  
  # Success rate (conditional on eligible and executed)
  success_rate <- if (n_executed > 0) {
    n_success / n_executed
  } else {
    NA_real_
  }
  
  # Conditional metrics (only on successes)
  successes <- subset %>% filter(run_status == "success")
  
  list(
    lambda = lambda,
    estimator_id = estimator_id,
    
    # Sample size breakdown
    n_total = n_total,
    n_eligible = n_eligible,
    n_executed = n_executed,
    n_success = n_success,
    n_fail = n_fail,
    n_ineligible = n_ineligible,
    n_skipped = n_skipped,
    n_not_run = n_not_run,
    
    # Success rate (key metric)
    success_rate = success_rate,
    
    # Conditional metrics (ONLY on successes)
    bias_conditional = if (nrow(successes) > 0) {
      median(successes$error_att, na.rm = TRUE)
    } else {
      NA_real_
    },
    
    mae_conditional = if (nrow(successes) > 0) {
      median(abs(successes$error_att), na.rm = TRUE)
    } else {
      NA_real_
    },
    
    rmse_conditional = if (nrow(successes) > 0 && moment_regime$rmse_valid) {
      sqrt(mean(successes$error_att^2, na.rm = TRUE))
    } else {
      NA_real_
    },
    
    # Coverage (if CI available)
    coverage_conditional = if (nrow(successes) > 0) {
      mean(successes$ci_covers_truth, na.rm = TRUE)
    } else {
      NA_real_
    },
    
    # Survivorship flags
    survivorship_risk = success_rate < 0.90,
    death_zone = success_rate < 0.50,
    transition_zone = success_rate >= 0.50 && success_rate < 0.90,
    
    # v3.2: Flag for plotting enforcement
    requires_success_rate_context = success_rate < 0.90
  )
}
```

### 12.2 Death Zone Definition (with Sensitivity Note)

``` r
death_zone_threshold <- 0.50  # User-configurable
transition_zone_threshold <- 0.90  # User-configurable

# Classification:
# - success_rate ≥ 0.90: "stable" (trustworthy conditional metrics)
# - 0.50 ≤ success_rate < 0.90: "transition" (flag survivorship bias)
# - success_rate < 0.50: "death zone" (do not plot curves)
```

**Sensitivity Note:** \> "Death zone and transition thresholds (0.50, 0.90) are heuristic defaults based on simulation experience. Sensitivity analyses with alternative thresholds (e.g., 0.40/0.85 or 0.60/0.95) are recommended for publication-quality results."

### 12.3 Visualization Rules

**Kill-Plot Semantics:**

``` r
cs_plot_kill_curve <- function(stress_summary, 
                               family,
                               metric = "mae",
                               death_zone_threshold = 0.50,
                               transition_zone_threshold = 0.90,
                               show_success_rate = TRUE) {
  # v3.2: Check input class
  if (!inherits(stress_summary, "cs_stress_summary")) {
    stop("Input must be from cs_summarise_stress()")
  }
  
  # v3.2: MANDATORY enforcement
  needs_context <- any(stress_summary$requires_success_rate_context, na.rm = TRUE)
  if (needs_context && !show_success_rate) {
    stop("Cannot hide success rate when any region has success_rate < 0.90. ",
         "Set show_success_rate = TRUE or filter to stable regions only.")
  }
  
  # For each lambda:
  # - If death_zone: gray shaded region, no curve, show success rate
  # - If transition: plot curve with dashed line + warning annotation
  # - If stable: plot curve normally
  
  # Curves are CONDITIONAL metrics (success-only)
  # Success rate shown as secondary axis or facet (REQUIRED)
  
  # Never interpolate across death zones
  # Never show RMSE where variance is infinite
}
```

**Hard Rule (v3.2):** The plotting function MUST error if `show_success_rate = FALSE` when any region has `success_rate < 0.90`.

**Table Output:**

``` r
# Always report success rate alongside conditional metrics
stress_summary %>%
  select(lambda, estimator, success_rate, 
         bias_conditional, mae_conditional, 
         survivorship_risk, death_zone) %>%
  mutate(
    # Add flag column for easy spotting
    flag = case_when(
      death_zone ~ "††",
      survivorship_risk ~ "†",
      TRUE ~ ""
    )
  )
```

**Interpretation Guidelines:**

> "Conditional bias and MAE are computed only on successful runs. Results marked with † (success_rate \< 0.90) may not be representative due to survivorship bias. Results marked with †† (success_rate \< 0.50) indicate computational breakdown; these are excluded from performance curves but success rates are reported."

------------------------------------------------------------------------

## 13. Campaign Consolidation

### 13.1 The Fragmentation Problem

When expanding λ-grids:

``` r
# Day 1: Initial campaign
plan1 <- cs_plan_stress_campaign(
  list(heavytail_noise_t = c("30", "10")),
  ...
)
# Campaign ID: A123...

# Day 2: Expand grid after seeing results
plan2 <- cs_plan_stress_campaign(
  list(heavytail_noise_t = c("30", "10", "5")),
  ...
)
# Campaign ID: B456... (different due to different grid)
```

**Result:** Staging directory contains runs from two campaigns. How to merge?

### 13.2 Consolidation Modes

``` r
cs_consolidate <- function(
  staging_dir,
  board,
  campaign_id = NULL,
  mode = c("strict", "loose", "manual"),
  ...
) {
  mode <- match.arg(mode)
  
  if (mode == "strict") {
    # Default: Only consolidate runs matching campaign_id
    # Recommended for publication-quality results
    ...
  } else if (mode == "loose") {
    # Consolidate by run_fingerprint (v3.2 CORRECTED)
    # Allows merging across grid expansions
    # WARNING: User must verify compatibility manually
    ...
  } else if (mode == "manual") {
    # User provides explicit list of campaign_ids to merge
    # Most flexible, requires careful provenance tracking
    ...
  }
}
```

### 13.3 Loose Mode Specification (v3.2 CORRECTED)

**CRITICAL FIX:** Dedupe on run_fingerprint, not partial identity.

``` r
cs_consolidate_loose <- function(staging_dir, board, 
                                 family_filter = NULL,
                                 schema_version = 3L) {
  # Find all completed runs
  runs <- list_staging_runs(staging_dir)
  
  # Filter by schema version (MUST match)
  runs <- runs %>%
    filter(schema_version == !!schema_version)
  
  # Optionally filter by family
  if (!is.null(family_filter)) {
    runs <- runs %>%
      filter(family_id %in% family_filter)
  }
  
  # v3.2: CRITICAL FIX - Dedupe on run_fingerprint
  # Only "most recent" within EXACT run_fingerprint (true duplicates from retries)
  consolidated <- runs %>%
    group_by(run_fingerprint) %>%
    slice_max(timestamp, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    # v3.2: CRITICAL - Preserve original_campaign_id per row
    mutate(
      original_campaign_id = campaign_id,
      consolidation_mode = "loose",
      consolidation_timestamp = Sys.time()
    )
  
  # v3.2: Detect conflicts (same instance/estimator/n/seed but different run_fp)
  # This should NOT happen if run_fingerprints are correct
  conflicts <- consolidated %>%
    group_by(instance_fingerprint, estimator_id, n, seed) %>%
    filter(n() > 1) %>%
    ungroup()
  
  if (nrow(conflicts) > 0) {
    warning("Found ", nrow(conflicts), " runs with same (instance, estimator, n, seed) ",
            "but different run_fingerprints. ",
            "This indicates config/version changes between campaigns.")
    
    # Write conflict report
    conflict_report <- conflicts %>%
      select(instance_id, estimator_id, n, seed, run_fingerprint, 
             original_campaign_id, estimator_version, timestamp) %>%
      arrange(instance_id, estimator_id, n, seed, timestamp)
    
    write_csv(conflict_report, 
              file.path(staging_dir, "consolidation_conflicts.csv"))
    
    stop("Consolidation aborted due to conflicts. ",
         "Multiple runs exist for same (instance, estimator, n, seed) ",
         "with different configurations. ",
         "See ", file.path(staging_dir, "consolidation_conflicts.csv"), "\n",
         "Use manual mode to resolve conflicts explicitly.")
  }
  
  # Build merged campaign_ids list
  campaign_ids_merged <- unique(consolidated$original_campaign_id)
  
  # Write to board
  pin_meta <- list(
    consolidation_mode = "loose",
    campaign_ids_merged = campaign_ids_merged,
    n_campaigns = length(campaign_ids_merged),
    n_runs = nrow(consolidated),
    consolidation_timestamp = Sys.time(),
    schema_version = schema_version
  )
  
  pins::pin_write(
    board = board,
    x = consolidated,
    name = "stress_campaign_loose_merged",
    metadata = pin_meta
  )
  
  message("Consolidated ", nrow(consolidated), " runs from ", 
          length(campaign_ids_merged), " campaigns")
  message("WARNING: Loose mode merges campaigns. Verify compatibility manually.")
  message("Original campaign IDs preserved in 'original_campaign_id' column.")
  
  consolidated
}
```

**Key Changes (v3.2):** 1. Dedupe on `run_fingerprint` (not partial identity) 2. Preserve `original_campaign_id` per row (not just in metadata) 3. Detect and abort on conflicts (different run_fp for same task) 4. Write conflict report for user investigation

### 13.4 Best Practices

**For iterative development:** - Use loose mode for exploratory analysis - Document all merged campaign IDs - Re-run strict mode for final results

**For publication:** - Plan complete grid up front - Use strict mode only - If grid must expand, create new campaign with explicit provenance

**Provenance tracking:**

``` r
# Record in paper/documentation
merged_provenance <- list(
  campaigns_merged = c("A123...", "B456..."),
  merge_mode = "loose",
  merge_timestamp = Sys.time(),
  merge_rationale = "Added lambda=5 after initial analysis",
  validated_by = "researcher_name",
  validation_checks = c("config_compatibility", "version_consistency")
)
```

------------------------------------------------------------------------

## 14. Implementation Roadmap

### 14.1 v0.2.0 "Families MVP" (Target: 3 weeks)

**Week 1: Core Infrastructure** - \[ \] Three-tier fingerprint system (family/instance/run) - \[ \] Dual registry (point + family) - \[ \] `cs_dgp_family()` constructor with dgp_config_defaults - \[ \] Family dispatcher (`cs_get_dgp`) - \[ \] Single-n enforcement in campaign planning

**Week 2: Oracle & Validation** - \[ \] ATT oracle (1D quadrature) - \[ \] QST oracle (v3.2 corrected mixture variance) - \[ \] `compute_mixture_quantile_bisection()` helper - \[ \] Truth precision contract with tolerance metadata - \[ \] Validation suite (revised V6, added V7) - \[ \] One family: `heavytail_noise_t` (df ∈ \[2.5, 30\], scale_mode = "l2_var") - \[ \] Z standardization with explicit weights

**Week 3: Planning & Execution** - \[ \] `cs_plan_stress_campaign()` with dgp_config serialization - \[ \] Structured strategy_map (no string parsing) - \[ \] Two-column status (plan_status/run_status) - \[ \] Eligibility filters with justifications - \[ \] Extended result schema - \[ \] `cs_summarise_stress()` with survivorship handling - \[ \] `cs_consolidate()` with corrected loose mode

**Deliverables:** - Runnable end-to-end example - Unit tests (fingerprints, Z standardization, oracle, status) - Integration tests (resume, grid expansion, loose consolidation) - Vignette: "Your First Stress Campaign"

### 14.2 v0.2.1 "Kill-Plots" (Target: 1-2 weeks)

-   [ ] `cs_plot_kill_curve()` with death zones
-   [ ] Mandatory success rate display enforcement
-   [ ] Transition zone dashed lines
-   [ ] Multi-estimator comparison
-   [ ] Export tables for papers (with flags)

### 14.3 v0.3.0 "Hard Regimes" (Target: 3-4 weeks)

-   [ ] L1-core scaling (df ≤ 2)
-   [ ] Multi-n campaign support (with n_grid canonicalization)
-   [ ] Contaminated noise family
-   [ ] Overlap stress family
-   [ ] Family-specific eligibility for GenGC
-   [ ] User-defined family registration

### 14.4 v0.4.0 "Methods Paper" (Post-dissertation)

-   [ ] Multi-dimensional stress (overlap × tails)
-   [ ] Real-data families (LaLonde w/ propensity truncation)
-   [ ] Adaptive grid suggestions
-   [ ] Interactive dashboard
-   [ ] JSS/R Journal submission

------------------------------------------------------------------------

## 15. Validation Plan

### 15.1 Unit Tests (v3.2 ENHANCED)

``` r
# Fingerprint stability
test_that("lambda canonicalization is stable", {
  fp1 <- cs_build_instance_fingerprint(..., lambda = "3", ...)
  fp2 <- cs_build_instance_fingerprint(..., lambda = 3.0, ...)
  expect_equal(fp1, fp2)
})

test_that("campaign fingerprint is timestamp-free", {
  plan1 <- cs_plan_stress_campaign(...)
  Sys.sleep(1)
  plan2 <- cs_plan_stress_campaign(...)  # Same inputs
  expect_equal(attr(plan1, "campaign_id"), attr(plan2, "campaign_id"))
})

# v3.2: Single-n enforcement
test_that("campaign planning rejects multi-n for MVP", {
  expect_error(
    cs_plan_stress_campaign(..., n = c(500, 1000)),
    "v0.2.0 MVP supports single-n"
  )
})

# Oracle validation
test_that("ATT oracle passes validation suite", {
  expect_true(test_oracle_validity("heavytail_noise_t", "10", "l2_var"))
})

test_that("QST oracle is monotone", {
  qst <- heavytail_t_qst_oracle("5", "l2_var")
  expect_true(all(diff(qst$value) >= -1e-6))
})

# v3.2: Oracle uses tolerance-level comparisons
test_that("oracle regression tests use stored tolerance", {
  oracle <- cs_oracle_truth("heavytail_noise_t", "30", "l2var")
  expected <- get_regression_truth("heavytail_noise_t", "30", "l2var")
  
  tol <- max(expected$att_tol_rel, expected$att_tol_abs)
  expect_equal(oracle$att, expected$att, tolerance = tol)
})

# v3.2: Z standardization validation
test_that("Z uses theoretical SD not sample SD", {
  dgp <- heavytail_t_factory(lambda = "10", n = 10000, seed = 42, 
                              scale_mode = "l2_var")
  Z <- dgp$oracle$z_index
  
  # Sample mean near 0
  expect_equal(mean(Z), 0, tolerance = 0.05)
  
  # Sample variance near 1 BUT NOT EXACTLY 1
  expect_equal(var(Z), 1, tolerance = 0.1)
  expect_true(abs(var(Z) - 1) > 0.005,
              info = "Z variance should fluctuate (not forced to 1)")
  
  # Verify theoretical SD calculation
  z_weights <- dgp$meta$z_weights
  theoretical_var <- sum(z_weights^2)
  expect_equal(dgp$meta$theoretical_sd_Z, sqrt(theoretical_var), 
               tolerance = 1e-10)
})

# v3.2: Status system
test_that("plan_status and run_status are separate", {
  plan <- cs_plan_stress_campaign(
    list(heavytail_noise_t = c("3")),
    estimator_ids = "lm_att",
    seeds = 1:2
  )
  
  expect_true("plan_status" %in% names(plan))
  expect_true("run_status" %in% names(plan))
  expect_true(all(plan$run_status == "not_run"))
})

test_that("ineligible tasks are not executed", {
  plan <- cs_plan_stress_campaign(
    list(heavytail_noise_t = c("1.5")),  # df=1.5 → no variance
    estimator_ids = "lm_att",
    seeds = 1:2,
    eligibility = list(lm_att = function(m) m$moment_regime$variance_exists)
  )
  
  expect_true(all(plan$plan_status == "ineligible"))
  expect_true(all(plan$run_status == "not_run"))
})

# Failure recording
test_that("failures are recorded with mode", {
  result <- cs_run_single(..., max_runtime = 0.001)
  expect_equal(result$run_status, "fail")
  expect_equal(result$failure_mode, "timeout")
})
```

### 15.2 Integration Tests (v3.2 ENHANCED)

``` r
test_that("grid expansion with resume works", {
  # Run coarse grid
  plan1 <- cs_plan_stress_campaign(
    list(heavytail_noise_t = c("10", "5")),
    estimator_ids = "lm_att",
    seeds = 1:2,
    n = 1000
  )
  res1 <- cs_run_campaign(plan1, board = board)
  
  # Expand grid
  plan2 <- cs_plan_stress_campaign(
    list(heavytail_noise_t = c("10", "5", "3")),
    estimator_ids = "lm_att",
    seeds = 1:2,
    n = 1000
  )
  res2 <- cs_run_campaign(plan2, board = board, skip_existing = TRUE)
  
  # Only new lambda=3 should compute (2 seeds)
  expect_equal(nrow(res2) - nrow(res1), 2)
  
  # Campaign IDs are different (different grids)
  expect_false(attr(plan1, "campaign_id") == attr(plan2, "campaign_id"))
})

# v3.2: Loose consolidation tests
test_that("loose consolidation merges by run_fingerprint", {
  # Assume two campaigns run with overlapping instances
  cons <- cs_consolidate_loose(staging_dir, board)
  
  # Should have merged runs
  expect_true(length(unique(cons$original_campaign_id)) > 1)
  expect_equal(cons$consolidation_mode[1], "loose")
  
  # original_campaign_id should be preserved per row
  expect_true("original_campaign_id" %in% names(cons))
})

test_that("loose consolidation detects conflicts", {
  # If same (instance, estimator, n, seed) has different run_fingerprints
  # Should abort with conflict report
  expect_error(
    cs_consolidate_loose(staging_dir_with_conflicts, board),
    "Consolidation aborted due to conflicts"
  )
  
  # Conflict report should exist
  expect_true(file.exists(
    file.path(staging_dir_with_conflicts, "consolidation_conflicts.csv")
  ))
})

test_that("v0.1.8 point DGPs still work", {
  result <- cs_run_seeds("synth_baseline", 
                         estimator_ids = "lm_att",
                         seeds = 1:5)
  expect_true(all(c("est_att", "error_att") %in% names(result)))
})
```

### 15.3 Smoke Tests

``` r
# Oracle determinism (tolerance-level)
test_that("oracle is reproducible within tolerance", {
  oracle1 <- cs_oracle_truth("heavytail_noise_t", "3", "l2_var")
  oracle2 <- cs_oracle_truth("heavytail_noise_t", "3", "l2_var")
  expect_equal(oracle1$att, oracle2$att, tolerance = 1e-5)
  expect_equal(oracle1$qst$value, oracle2$qst$value, tolerance = 1e-5)
})

# Death zone detection
test_that("death zones are detected in extreme regimes", {
  results <- cs_run_campaign(
    list(heavytail_noise_t = c("30", "10", "5", "3", "2.5")),
    estimator_ids = c("lm_att", "gengc"),
    seeds = 1:50,
    n = 1000
  )
  summary <- cs_summarise_stress(results)
  
  # OLS should degrade at low df
  ols_summary <- summary %>% filter(estimator_id == "lm_att")
  expect_true(any(ols_summary$success_rate < 0.90))
  
  # GenGC should be more stable
  gengc_summary <- summary %>% filter(estimator_id == "gengc")
  expect_true(all(gengc_summary$success_rate > 0.80))
})
```

------------------------------------------------------------------------

## 16. Governance

### 16.1 Oracle Regression Test Governance (v3.2 ENHANCED)

**Problem:** Regression tests pin oracle truth. When are updates allowed?

**Enhanced Schema (v3.2):**

``` r
# File: inst/oracle_regression_tests.rds
# v3.2: Now includes tolerance metadata
regression_truths <- list(
  "heavytail_noise_t" = list(
    "30_l2var" = list(
      att = 1.0234567,
      att_tol_rel = 1e-5,               # NEW: tolerance metadata
      att_tol_abs = 1e-6,
      qst = c(...),
      qst_tol_rel = 1e-5,
      qst_tol_abs = 1e-6,
      oracle_config_fingerprint = "abc123...",  # NEW: config identity
      version = "1.0.0",
      r_version = "4.3.2",
      platform = "x86_64-pc-linux-gnu",
      updated = "2025-01-08",
      updated_by = "max",
      reason = "initial"
    )
  )
)
```

**When updates are allowed:**

1.  **Initial registration**: First oracle implementation for a family
2.  **Bug fix**: Oracle had demonstrable error (requires documentation)
3.  **Precision improvement**: Algorithm change improves convergence (old value must be within 10× old tolerance)
4.  **Family version bump**: Breaking change to DGP (requires new family_version)

**When updates are PROHIBITED:**

-   To make tests pass after unrelated code changes
-   To "tune" oracle to match estimator outputs
-   Without explicit `reason` field
-   Without verifying oracle_config_fingerprint matches (unless bugfix)

**Review process (v3.2 ENHANCED):**

``` r
cs_update_regression_truth <- function(
  family_id, 
  lambda, 
  scale_mode,
  new_att,
  new_qst,
  oracle_config,  # NEW: Must provide config
  reason,
  updated_by
) {
  # Load existing
  truths <- readRDS("inst/oracle_regression_tests.rds")
  
  # Check if exists
  key <- paste0(lambda, "_", scale_mode)
  old <- truths[[family_id]][[key]]
  
  if (!is.null(old)) {
    # v3.2: Check if change is within 10× old tolerance
    old_tol <- max(old$att_tol_rel, old$att_tol_abs)
    change <- abs(new_att - old$att)
    
    cat("OLD ATT:", old$att, "\n")
    cat("NEW ATT:", new_att, "\n")
    cat("Difference:", change, "\n")
    cat("Old tolerance:", old_tol, "\n")
    cat("Threshold (10× tol):", 10 * old_tol, "\n")
    
    if (change > 10 * old_tol) {
      stop(sprintf(
        "Change (%.2e) exceeds 10× old tolerance (%.2e). ",
        "Require explicit approval and reason='family_version_bump'.",
        change, 10 * old_tol
      ))
    }
    
    # v3.2: Verify oracle_config_fingerprint matches (if not bugfix)
    new_config_fp <- digest::digest(oracle_config)
    if (reason != "bugfix" && new_config_fp != old$oracle_config_fingerprint) {
      warning(
        "Oracle config fingerprint changed from ",
        old$oracle_config_fingerprint, " to ", new_config_fp, ". ",
        "This may invalidate comparison. Ensure this is intentional."
      )
    }
    
    if (!reason %in% c("bugfix", "precision_improvement", "family_version_bump")) {
      stop("Invalid reason. Must be: bugfix, precision_improvement, or family_version_bump.")
    }
  }
  
  # Store new value with v3.2 enhanced schema
  truths[[family_id]][[key]] <- list(
    att = new_att,
    att_tol_rel = oracle_config$tol_rel,       # Store tolerance
    att_tol_abs = oracle_config$tol_abs,
    qst = new_qst,
    qst_tol_rel = oracle_config$tol_rel,
    qst_tol_abs = oracle_config$tol_abs,
    oracle_config_fingerprint = digest::digest(oracle_config),
    version = cs_get_family_version(family_id),
    r_version = R.version.string,
    platform = Sys.info()["machine"],
    updated = as.character(Sys.Date()),
    updated_by = updated_by,
    reason = reason,
    old_value = if (!is.null(old)) old$att else NA,
    old_tolerance = if (!is.null(old)) max(old$att_tol_rel, old$att_tol_abs) else NA
  )
  
  # Save
  saveRDS(truths, "inst/oracle_regression_tests.rds")
  
  message("Regression truth updated. Commit with detailed message:")
  message("  git add inst/oracle_regression_tests.rds")
  message("  git commit -m 'Update oracle truth: ", family_id, " ", key, " (", reason, ")'")
}
```

### 16.2 Design Evolution Policy

**Constitutional amendments** (Articles I-IX) require: - Written rationale - Backward compatibility analysis - Schema version bump if breaking

**Family specifications** can evolve via: - Bug fixes (patch version bump) - New families (no version bump to existing) - Deprecated families (status change, not deletion)

**Breaking changes** (requiring schema version bump): - Fingerprint algorithm changes - Oracle computation method changes - Status system changes - Estimand definition changes

### 16.3 Deprecation Protocol

When a family has issues:

``` r
cs_deprecate_family <- function(family_id, reason) {
  # Change status to "deprecated"
  # Do NOT delete from registry
  # Users can still access with explicit version
  
  registry <- cs_dgp_family_registry()
  registry$status[registry$family_id == family_id] <- "deprecated"
  registry$deprecation_reason[registry$family_id == family_id] <- reason
  registry$deprecated_date[registry$family_id == family_id] <- Sys.Date()
  
  message("Family ", family_id, " deprecated: ", reason)
  message("Existing results remain valid. New campaigns will warn.")
}
```

------------------------------------------------------------------------

## 17. Appendices

### Appendix A: Full Lifecycle Example

``` r
library(CausalStress)
library(pins)
library(dplyr)
library(ggplot2)

# 1. Setup
board <- board_folder("~/causalstress_results")

# 2. Define eligibility
eligibility <- list(
  lm_att = function(instance_meta) {
    instance_meta$moment_regime$variance_exists
  },
  gengc = function(instance_meta) {
    # Justified: QR remains well-defined under infinite variance
    instance_meta$moment_regime$att_estimable
  }
)

# 3. Plan stress campaign
plan <- cs_plan_stress_campaign(
  dgp_families = list(
    heavytail_noise_t = c("30", "10", "5", "3", "2.5")
  ),
  estimator_ids = c("lm_att", "ipw_att", "gengc"),
  seeds = 1:100,
  n = 1000,  # v3.2: Single-n for MVP
  strategy_map = list(
    defaults = list(
      ci_method = "bootstrap",
      n_boot = 200,
      max_runtime = 300
    ),
    estimator = list(
      gengc = list(
        num_trees = 1200,
        n_draws = 2000
      )
    ),
    family = list(
      heavytail_noise_t = list(
        max_runtime = 600
      )
    )
  ),
  eligibility = eligibility,
  campaign_seed = 42,
  batch_size = 50
)

# Check plan
print(plan)
cat("Campaign ID:", attr(plan, "campaign_id"), "\n")
cat("Eligible tasks:", sum(plan$plan_status == "eligible"), "\n")
cat("Ineligible tasks:", sum(plan$plan_status == "ineligible"), "\n")

# 4. Run campaign (with resume)
results <- cs_run_campaign(
  plan = plan,
  board = board,
  staging_dir = "~/causalstress_staging",
  workers = 20,
  show_progress = TRUE,
  skip_existing = TRUE
)

# 5. Consolidate (strict mode for publication)
cs_consolidate(
  staging_dir = "~/causalstress_staging",
  board = board,
  campaign_id = attr(plan, "campaign_id"),
  mode = "strict"
)

# 6. Analyze
tidy <- cs_tidy(board, campaign_id = attr(plan, "campaign_id"))

stress_summary <- tidy %>%
  group_by(family_id, lambda, estimator_id) %>%
  summarise(
    cs_summarise_stress(cur_data(), lambda, estimator_id, moment_regime),
    .groups = "drop"
  )

# Mark as cs_stress_summary class for enforcement
class(stress_summary) <- c("cs_stress_summary", class(stress_summary))

# 7. Visualize (with mandatory success rate)
kill_plot <- cs_plot_kill_curve(
  stress_summary,
  family = "heavytail_noise_t",
  metric = "mae",
  show_death_zones = TRUE,
  show_success_rate = TRUE  # REQUIRED when success_rate < 0.9
)

ggsave("heavytail_killplot.png", kill_plot, width = 12, height = 6)

# 8. Table for paper (with flags)
stress_summary %>%
  filter(lambda %in% c("30", "10", "5", "3", "2.5")) %>%
  select(lambda, estimator_id, success_rate, 
         bias_conditional, mae_conditional, 
         survivorship_risk, death_zone) %>%
  mutate(
    flag = case_when(
      death_zone ~ "††",
      survivorship_risk ~ "†",
      TRUE ~ ""
    )
  ) %>%
  arrange(lambda, estimator_id) %>%
  write_csv("table_stress_results.csv")

# 9. Export provenance
campaign_meta <- attr(plan, "campaign_metadata")
cat("Campaign fingerprint:", campaign_meta$campaign_id, "\n")
cat("Git hash:", campaign_meta$git_hash, "\n")
cat("Timestamp:", format(campaign_meta$timestamp_created), "\n")
cat("Eligible tasks:", campaign_meta$n_eligible, "\n")
```

### Appendix B: cs_override() Helper

``` r
#' Create Structured Strategy Override
#' 
#' Helper function to avoid string parsing in strategy_map.
#' 
#' @param estimator Estimator ID (character)
#' @param family Family ID (character)
#' @param config Configuration list
#' @return S3 object of class "cs_strategy_override"
#' @export
cs_override <- function(estimator, family, config) {
  stopifnot(is.character(estimator), length(estimator) == 1)
  stopifnot(is.character(family), length(family) == 1)
  stopifnot(is.list(config))
  
  structure(
    list(
      estimator = estimator,
      family = family,
      config = config
    ),
    class = "cs_strategy_override"
  )
}

#' @export
print.cs_strategy_override <- function(x, ...) {
  cat("Strategy override:\n")
  cat("  Estimator:", x$estimator, "\n")
  cat("  Family:", x$family, "\n")
  cat("  Config:\n")
  str(x$config, max.level = 1)
}
```

### Appendix C: normalize_for_fingerprint()

``` r
normalize_for_fingerprint <- function(obj) {
  # Convert to canonical form for fingerprinting
  # - Sort lists by name
  # - Convert factors to character
  # - Remove attributes except names
  # - Ensure consistent precision for numerics
  
  if (is.list(obj)) {
    # Sort by names (if named)
    if (!is.null(names(obj))) {
      obj <- obj[order(names(obj))]
    }
    # Recursively normalize
    obj <- lapply(obj, normalize_for_fingerprint)
  } else if (is.factor(obj)) {
    obj <- as.character(obj)
  } else if (is.numeric(obj)) {
    # Round to consistent precision
    obj <- signif(obj, digits = 15)
  }
  
  # Strip all attributes except names, class, dim
  keep_attrs <- c("names", "class", "dim")
  curr_attrs <- attributes(obj)
  if (!is.null(curr_attrs)) {
    attributes(obj) <- curr_attrs[intersect(names(curr_attrs), keep_attrs)]
  }
  
  obj
}
```

### Appendix D: Changes from v3.1

**Critical Fixes:** 1. Campaign fingerprint enforces single-n for MVP (Section 7.4) 2. Loose consolidation dedupes on run_fingerprint (Section 13.3) 3. Oracle mixture variance corrected (Section 6.4) 4. Status split into plan_status/run_status (Section 4.3, 11.1, 12.1)

**High Priority:** 5. Oracle tolerance metadata in regression tests (Section 6.5, 16.1) 6. Z standardization with explicit weights (Section 8.1.1) 7. dgp_config defaults serialization (Section 9.1, 10.2)

**Medium Priority:** 8. Warning for df \< 2.5 in L2-var (Section 4.2.1) 9. Improved QST V6 test (Section 6.5) 10. Family-specific GenGC eligibility noted (Section 11.2)

**Documentation:** 11. `compute_mixture_quantile_bisection` contract (Section 6.4) 12. Near-2 numerical instability note (Section 8.1.3) 13. Plot guardrail enforcement (Section 12.3)

------------------------------------------------------------------------

## Conclusion

This specification provides a **complete, implementation-ready, and reviewer-proof design** for parameterized DGP families that:

1.  ✅ Enforces single-n campaigns (MVP scope control)
2.  ✅ Deduplicates consolidation by run_fingerprint (prevents merging incompatible runs)
3.  ✅ Calculates oracle mixture variance correctly
4.  ✅ Splits status into plan_status/run_status (unambiguous semantics)
5.  ✅ Stores tolerance metadata with regression truths
6.  ✅ Uses explicit Z weights with theoretical SD
7.  ✅ Serializes dgp_config defaults at planning time

**All critical issues from v3.1 adversarial reviews are resolved.**

**Status: APPROVED FOR IMPLEMENTATION**

**Next Steps:** 1. Begin Week 1: Core infrastructure (fingerprints, registry, dispatcher) 2. Week 2: Oracle implementation with corrected variance calculation 3. Week 3: Planning with two-column status and loose consolidation 4. Validation: Comprehensive unit + integration tests

------------------------------------------------------------------------

**Document Version:** 3.2 (Final)\
**Schema Version:** 3\
**Last Updated:** 2025-01-08\
**Status:** IMPLEMENTATION-READY\
**Authors:** Max (with comprehensive adversarial review synthesis)\
**License:** CC-BY-4.0 (design spec), MIT (code examples)

--