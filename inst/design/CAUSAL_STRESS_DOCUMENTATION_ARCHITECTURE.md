# CausalStress Documentation Architecture: The Sidecar Pattern

**Version:** 0.2.0  
**Status:** Accepted  
**Date:** 2025-12-08  
**Revision:** 2 (Math moved to Layer 3)

---

## 1. The Problem

The `CausalStress` Constitution (Article VII) mandates that DGP implementations are **Immutable**. Once `v1.3.0` is released, the code in `R/dgp-*.R` cannot change without a version bump.

However, scientific documentation (mathematical specification, validation evidence, stress test rationale) is **Mutable**. We need to refine explanations, add validation plots, and improve clarity *without* triggering a semantic version change of the DGP itself.

**Conflict:** If documentation lives inside the immutable R file (via Roxygen), we cannot improve the docs without "touching" the immutable file.

**MVP Goal:** Enable specialists and reviewers to validate that "this DGP does what it claims" through:
1. Mathematical transparency
2. Visual evidence
3. **Empirical validation** - Run naive estimators to verify the stress actually breaks them

Pedagogy is a future enhancement, not an MVP blocker.

---

## 2. The Solution: Three-Layer Architecture

We decouple the **Execution (Law)** from the **Classification (Metadata)** and the **Explanation (Narrative)** using a "Sidecar" pattern.

### Layer 1: The Immutable Core ("The Law")
* **Role:** Executable truth.
* **Location:** `R/dgp-<id>.R`
* **Content:** Pure R code defining the generator function.
* **Roxygen:** Minimal. Only `@export` and a brief one-line `@description`.
* **Mutability:** **FROZEN.** Changing this file requires a DGP version bump per Constitution Article VII.
* **Version Authority:** The `version` field in `cs_dgp_registry()` is the **single source of truth** for code version. YAML metadata must match this version; mismatches trigger validation errors. YAML can never override code version.
* **Example:**
  ```r
  #' @title Baseline Synthetic DGP
  #' @description Linear baseline with Gaussian noise.
  #' @export
  dgp_synth_baseline <- function(n, seed = NULL) {
    # ... implementation ...
  }
  ```

### Layer 2: The Metadata Sidecar ("The Tags")
* **Role:** Machine-readable classification and discovery.
* **Location:** `inst/dgp_meta/<id>.yml`
* **Content:**
    * Identity: `dgp_id`, `version`, `status`, `type`
    * Classification: Stress profile tags (overlap, noise, effect structure)
    * Difficulty: Rating (1-5 stars) for pedagogy
    * Literature: Primary citations (author, year, DOI)
    * **NO MATH** (math lives in Layer 3)
* **Mutability:** **MUTABLE.** Can be updated to refine tags, add citations, adjust difficulty ratings.
* **Purpose:** Powers programmatic DGP discovery, filtering, and comparison matrices.

### Layer 3: The Narrative Sidecar ("The Validation")
* **Role:** Scientific validation and mathematical specification for specialists.
* **Location:** `inst/dgp_meta/<id>.Rmd` (RMarkdown Fragment)
* **MVP Content:**
    * **Mathematical Specification:** LaTeX formulas for $\mu_0(X)$, $\tau(X)$, $p(X)$, noise distributions
    * **What It Stresses:** Clear statement of what estimator property this tests
    * **Expected Behavior:** How estimators should perform (pass/fail criteria)
    * **Visual Evidence:** Plots showing the stress actually exists (overlap, heterogeneity, etc.)
* **Future Enhancements (Post-MVP):**
    * Literature context and citations
    * Pedagogical interpretation
    * Code examples and failure demonstrations
    * Common pitfalls documentation
* **Mutability:** **MUTABLE.** Can be refined as validation deepens.
* **MVP Purpose:** Convince a reviewer: "Yes, this DGP is correctly implemented and tests what it claims."
* **Math Role:** Mathematical formulas here are **descriptive** (explaining what the code does), not **normative** (defining truth). The R code in Layer 1 + oracle truth system define actual behavior; Layer 3 math is your best explanation for humans.

---

## 3. Design Rationale: Why Math Goes in Layer 3

### Original Proposal Issue
The initial design placed LaTeX formulas in YAML (Layer 2):
```yaml
math:
  mu0: "1 + X_1 + 0.5 X_2"
  tau: "1 + 0.5 X_1"
```

**Problems with this approach:**
1. **Redundancy:** Duplicates R code, risking divergence
2. **Validation Burden:** Requires automated tests to ensure YAML matches R implementation
3. **Limited Expressiveness:** YAML strings can't render complex multi-line derivations
4. **Wrong Abstraction:** Math is *narrative* (educational), not *metadata* (classification)

### Revised Design Benefits

**Math in RMarkdown (Layer 3) is superior because:**

1. **Natural Medium:** RMarkdown is designed for math exposition via LaTeX
   ```markdown
   The baseline outcome follows:
   $$\mu_0(X) = 1 + X_1 + 0.5 X_2$$
   
   The structural treatment effect is:
   $$\tau(X) = 1 + 0.5 X_1$$
   ```

2. **Rich Context:** Math can be explained, not just stated:
   ```markdown
   We model propensity as a logistic function of two covariates:
   $$p(X) = \text{expit}(0.5 X_1 - 0.5 X_2)$$
   
   This creates moderate imbalance: $X_1$ increases treatment probability
   while $X_2$ decreases it, producing overlap violations primarily in
   the tails of the covariate distribution.
   ```

3. **Derivations & Intuition:** Can show *why* a formula matters:
   ```markdown
   The structural ATT for treated units is:
   $$\text{ATT}_{\text{true}} = \mathbb{E}[\tau(X) \mid W=1]$$
   
   Under our specification, this simplifies to:
   $$\text{ATT}_{\text{true}} = 1 + 0.5 \cdot \mathbb{E}[X_1 \mid W=1]$$
   
   Since $W$ depends on $X_1$, the treated population is shifted toward
   positive $X_1$ values, making $\mathbb{E}[X_1 \mid W=1] > 0$.
   ```

4. **No Redundancy:** Math is *pedagogical documentation*, not *specification*
   - R code is the executable truth
   - RMarkdown math explains what the code does
   - No need for validation tests (no claim to be source of truth)

5. **Evolution-Friendly:** As understanding deepens, we can add:
   - Theoretical properties (identification conditions)
   - Asymptotic behavior
   - Connections to other DGPs
   - Visual intuition (inline plots)

**YAML stays pure metadata:** Just tags for filtering and classification.

---

## 4. Folder Structure

```text
CausalStress/
├── R/
│   ├── dgp-synth-baseline.R       # LAYER 1: The Code (Immutable)
│   ├── dgp-synth-heavytail.R
│   └── ...
│
├── inst/
│   ├── dgp_meta/
│   │   ├── schema.yml             # Controlled vocabulary definitions
│   │   │
│   │   ├── synth_baseline.yml     # LAYER 2: Metadata (Tags only)
│   │   ├── synth_baseline.Rmd     # LAYER 3: Narrative (Math + pedagogy)
│   │   │
│   │   ├── synth_heavytail.yml
│   │   ├── synth_heavytail.Rmd
│   │   │
│   │   └── ...
│   │
│   └── dossiers/                  # Generated HTML outputs
│       ├── synth_baseline.html
│       ├── synth_heavytail.html
│       └── ...
│
├── dev/
│   ├── render_dossiers.R          # The Factory Script
│   └── validate_yaml_schema.R     # Schema validation
│
└── vignettes/
    └── dgp_catalog.Rmd             # Master catalog (optional)
```

---

## 5. YAML Schema Definition (Layer 2)

### 5.1 Controlled Vocabulary (`inst/dgp_meta/schema.yml`)

```yaml
# Schema defining valid values for stress profile tags
# This file is the authoritative reference for classification vocabulary

stress_profile_schema:
  overlap:
    type: enum
    values: [none, mild, moderate, severe, extreme]
    description: "Propensity score overlap stress level"
    
  noise:
    type: enum
    values: [gaussian, heavy, heteroskedastic, bounded]
    description: "Noise distribution characteristics"
    
  linearity:
    type: enum
    values: [linear, smooth, discontinuous]
    description: "Outcome surface smoothness"
    
  effect:
    type: enum
    values: [constant, linear, nonlinear, heterogeneous]
    description: "Treatment effect structure"
    
  target:
    type: enum
    values: [att, qst, both]
    description: "Primary estimand focus"

difficulty_schema:
  stars:
    type: integer
    range: [1, 5]
    description: "Estimator challenge difficulty (how hard for methods to get right)"
    labels:
      1: "Sanity check (all methods should pass)"
      2: "Basic stress (exposes simple failures)"
      3: "Moderate stress (requires proper implementation)"
      4: "Hard stress (only robust methods succeed)"
      5: "Extreme stress (frontier/research challenge)"

status_schema:
  type: enum
  values: [stable, experimental, deprecated]
  description: "DGP lifecycle status per Constitution Article VII"
```

### 5.2 Example DGP Metadata (`inst/dgp_meta/synth_baseline.yml`)

**MVP Fields (Required):**

```yaml
dgp_id: "synth_baseline"
version: "1.3.0"
status: "stable"
type: "synthetic"

meta:
  title: "Linear Baseline (Sanity Check)"
  description: "Linear signal with Gaussian noise and moderate overlap"
  challenge: "Baseline reference / Sanity check"
  tags: [linear, gaussian, baseline, tutorial]

# Machine-Readable Classification (NO MATH)
stress_profile:
  overlap: "moderate"
  linearity: "linear" 
  noise: "gaussian"
  effect: "linear"
  target: "both"

difficulty:
  stars: 1
  label: "sanity check (all methods should pass)"
```

**Optional Fields (Post-MVP Enhancement):**

```yaml
# Primary Literature (optional for MVP - add during Phase 4)
literature:
  primary_citation:
    author: "CausalStress Team"
    year: 2025
    title: "Baseline reference implementation"
  
  # related_work:
  #   - "Imbens & Rubin (2015): Causal Inference - Ch. 3"

# Governance (optional - useful for tracking but not required for validation)
governance:
  date_status_changed: "2025-11-28"
  rationale: "Validated against analytical expectations"
  superseded_by: null
  validation_commit: "a0c3d96"

# Computational Properties (optional - can drift, not enforced)
properties:
  typical_n: [500, 1000, 2000]
  runtime_seconds_n1000: 0.05
  memory_mb_n1000: 2
  supports_oracle: true
```

**Design Note:** Literature, governance, and properties are defined in the schema but **not required for MVP**. They can be added incrementally during Phase 4 (polish). Missing optional fields default gracefully in the registry.

### 5.3 Example Experimental DGP (`inst/dgp_meta/synth_heavytail.yml`)

```yaml
dgp_id: "synth_heavytail"
version: "1.3.0"
status: "stable"
type: "synthetic"

meta:
  title: "Heavy-Tailed Noise (Cauchy Contamination)"
  description: "Linear signal with Gaussian-Cauchy mixture noise"
  challenge: "Robustness to infinite variance"
  tags: [linear, heavy-tail, robustness, outliers]

stress_profile:
  overlap: "moderate"
  linearity: "linear"
  noise: "heavy"
  effect: "linear"
  target: "att"

difficulty:
  stars: 3
  label: "intermediate"

literature:
  primary_citation:
    author: "CausalStress Team"
    year: 2025
    title: "Heavy-tail stress test"
    note: "Tests L2-loss estimator breakdown"
  
  related_work:
    - "Huber (1964): Robust estimation of location parameter"
    - "Hampel et al. (1986): Robust Statistics"

governance:
  date_status_changed: "2025-11-28"
  rationale: "Validated - correctly breaks non-robust estimators"
  superseded_by: null
  validation_commit: "a0c3d96"

properties:
  typical_n: [500, 1000, 2000]
  runtime_seconds_n1000: 0.06
  memory_mb_n1000: 2
  supports_oracle: true
  warnings: 
    - "Variance is undefined due to Cauchy component"
    - "Bootstrap may be unstable with small B"
```

---

## 6. RMarkdown Narrative Template (Layer 3)

### 6.1 MVP Template Structure (`inst/dgp_meta/synth_baseline.Rmd`)

**MVP Focus:** Scientific validation for specialists. Convince reviewers the DGP is correct.

```markdown
---
title: "DGP: Synth Baseline"
dgp_id: "synth_baseline"
version: "1.3.0"
---

## What This DGP Stresses

**Purpose:** Sanity check. If an estimator fails here, it's fundamentally broken.

**Tests:**
- Correct implementation under ideal conditions
- Bootstrap stability with well-behaved noise
- CI coverage when all assumptions hold

**Expected Behavior:**
- ✓ OLS/G-computation: unbiased, ~95% coverage
- ✓ IPW: unbiased if propensity correct
- ✓ DR: unbiased, efficient

---

## Mathematical Specification

### Covariates
$$X_1, X_2, X_3, X_4, X_5 \sim \mathcal{N}(0, 1) \text{ (independent)}$$

Only $X_1$, $X_2$ affect outcomes/treatment (sparse).

### Propensity Score
$$p(X) = \text{expit}(0.5 X_1 - 0.5 X_2)$$

Creates moderate overlap (typical range $[0.2, 0.8]$).

### Outcome Model
$$Y_0 = \mu_0(X) + \varepsilon_0$$
$$\mu_0(X) = 1 + X_1 + 0.5 X_2$$
$$\varepsilon_0, \varepsilon_1 \sim \mathcal{N}(0, 0.5)$$

### Treatment Effect
$$\tau(X) = 1 + 0.5 X_1$$

Linear heterogeneity. Treated outcome:
$$Y_1 = Y_0 + \tau(X)$$

### ATT Truth
$$\text{ATT} = \mathbb{E}[\tau(X) \mid W = 1] = 1 + 0.5 \cdot \mathbb{E}[X_1 \mid W = 1]$$

Typical value: $\approx 1.15$ (depends on realized sample).

---

## Validation Evidence

### Visual Diagnostics

```{r validation-plots, echo=FALSE, message=FALSE, warning=FALSE, fig.width=10, fig.height=6}
library(CausalStress)
library(ggplot2)
library(patchwork)

set.seed(123)
dgp <- dgp_synth_baseline(n = 2000, seed = 123)
df <- dgp$df

# Plot 1: Propensity overlap
p1 <- ggplot(df, aes(x = p, fill = factor(w))) +
  geom_histogram(alpha = 0.6, bins = 30, position = "identity") +
  labs(title = "Propensity Distribution (Moderate Overlap)",
       x = "p(X)", y = "Count") +
  theme_minimal()

# Plot 2: Treatment effect heterogeneity
p2 <- ggplot(df, aes(x = X1, y = structural_te)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "Treatment Effect: τ(X) = 1 + 0.5·X₁",
       x = "X₁", y = "τ(X)") +
  theme_minimal()

# Plot 3: Balance check
p3 <- ggplot(df, aes(x = X1, y = X2, color = factor(w))) +
  geom_point(alpha = 0.4) +
  labs(title = "Covariate Balance",
       x = "X₁", y = "X₂") +
  theme_minimal()

# Plot 4: Outcome distributions
p4 <- ggplot(df, aes(x = y, fill = factor(w))) +
  geom_density(alpha = 0.6) +
  labs(title = "Observed Outcomes",
       x = "Y", y = "Density") +
  theme_minimal()

(p1 + p2) / (p3 + p4)
```

**Validation Checklist:**
- ✓ Propensity stays in $[0.2, 0.8]$ (no extreme weights)
- ✓ Treatment effect linear in $X_1$ (correct specification testable)
- ✓ Treated group shifted toward positive $X_1$ (selection bias present)
- ✓ Outcome distributions overlapping (common support satisfied)

---

## Empirical Validation: Naive Estimator Benchmark

**Goal:** Verify that estimators behave as expected under this DGP.

**Implementation Note:** Validation chunks use relatively small samples and seed counts to keep dossier rendering fast (`n=500-1000`, `seeds=10-20`). Heavy-duty benchmarks with `n=2000` and `seeds=50+` should be run offline and results optionally pinned/linked. This keeps pkgdown builds and CRAN checks manageable.

### Test 1: Naive Difference-in-Means (Should Fail)

```{r naive-dim, cache=TRUE, message=FALSE, warning=FALSE}
library(CausalStress)

# Lightweight validation run (for dossier)
results_naive <- cs_run_seeds(
  dgp_id = "synth_baseline",
  estimator_id = "naive_dim",  # Simple mean(Y|W=1) - mean(Y|W=0)
  n = 500,    # Smaller n for fast rendering
  seeds = 1:20,  # Fewer seeds than production runs
  bootstrap = TRUE,
  B = 200
)

# Summarize
summary_naive <- results_naive %>%
  cs_tidy() %>%
  summarise(
    mean_bias = mean(att_error),
    rmse = sqrt(mean(att_error^2)),
    coverage = mean(att_covered)
  )

knitr::kable(summary_naive, digits = 3, caption = "Naive DIM Performance")
```

**Expected Result (typical, not invariant):**
- Bias: approximately $-0.1$ to $-0.2$ (negative due to selection on $X_1$)
- Coverage: typically $< 0.80$ (systematically misses truth)

**Interpretation:** Selection bias is real. Naive methods fail as designed.

---

### Test 2: Oracle Estimator (Should Pass)

```{r oracle-check, cache=TRUE, message=FALSE, warning=FALSE}
# Run oracle (uses true structural_te)
results_oracle <- cs_run_seeds(
  dgp_id = "synth_baseline",
  estimator_id = "oracle_att",
  n = 500,
  seeds = 1:20,
  bootstrap = FALSE
)

summary_oracle <- results_oracle %>%
  cs_tidy() %>%
  summarise(
    mean_bias = mean(att_error),
    rmse = sqrt(mean(att_error^2))
  )

knitr::kable(summary_oracle, digits = 3, caption = "Oracle Performance")
```

**Expected Result (typical):**
- Bias: $< 0.001$ (essentially zero, within Monte Carlo noise)
- RMSE: $< 0.001$ (Monte Carlo noise only)

**Interpretation:** DGP truth system is working correctly.

---

### Test 3: Linear Regression (Should Pass)

```{r lm-check, cache=TRUE, message=FALSE, warning=FALSE}
# Run OLS G-computation
results_lm <- cs_run_seeds(
  dgp_id = "synth_baseline",
  estimator_id = "lm_att",
  n = 500,
  seeds = 1:20,
  bootstrap = TRUE,
  B = 200
)

summary_lm <- results_lm %>%
  cs_tidy() %>%
  summarise(
    mean_bias = mean(att_error),
    rmse = sqrt(mean(att_error^2)),
    coverage = mean(att_covered)
  )

knitr::kable(summary_lm, digits = 3, caption = "Linear Regression Performance")
```

**Expected Result (typical):**
- Bias: $< 0.05$ (unbiased under correct specification)
- Coverage: typically $\geq 0.90$ (proper CIs)

**Interpretation:** When model is correctly specified, OLS works.

---

### Validation Summary

```{r validation-summary, echo=FALSE}
bind_rows(
  summary_naive %>% mutate(estimator = "Naive DIM"),
  summary_oracle %>% mutate(estimator = "Oracle", coverage = NA),
  summary_lm %>% mutate(estimator = "Linear Reg")
) %>%
  select(estimator, mean_bias, rmse, coverage) %>%
  knitr::kable(digits = 3, caption = "DGP Validation Results")
```

**Conclusion:**
- ✅ Naive methods fail as expected (selection bias detected)
- ✅ Oracle is unbiased (truth system correct)
- ✅ Proper adjustment recovers truth (DGP is valid)

**This DGP does what it claims.**

---

## Implementation Verification

```{r verify-implementation, eval=FALSE}
library(CausalStress)

# Generate and check
dgp <- dgp_synth_baseline(n = 1000, seed = 42)

# Verify structure
stopifnot(all(c("y0", "y1", "p", "structural_te") %in% names(dgp$df)))

# Verify ATT computation
manual_att <- mean(dgp$df$structural_te[dgp$df$w == 1])
stopifnot(abs(dgp$true_att - manual_att) < 1e-10)

# Verify reproducibility
dgp2 <- dgp_synth_baseline(n = 1000, seed = 42)
stopifnot(identical(dgp$df, dgp2$df))
```

---

## Changelog

**v1.3.0** (2025-11-28)
- Initial stable release
- Validated: oracle convergence, reproducibility, contract compliance
- Empirical validation: naive DIM fails, proper adjustment succeeds
```

### 6.2 Alternative Template for Stress DGPs (e.g., `synth_heavytail.Rmd`)

**Implementation Note:** This template assumes core benchmarking helpers like `cs_run_seeds()` and `cs_tidy()` exist. If not yet implemented, they should be added as part of the benchmarking core infrastructure.

For DGPs designed to **break** certain estimators:

```markdown
## Empirical Validation: Breaking L2-Based Estimators

**Goal:** Verify that heavy-tail noise actually breaks non-robust methods.

### Test 1: OLS (Should Fail)

```{r ols-heavytail, cache=TRUE}
results_ols <- cs_run_seeds(
  dgp_id = "synth_heavytail",
  estimator_id = "lm_att",
  n = 500,
  seeds = 1:20,
  bootstrap = TRUE,
  B = 200
)

summary_ols <- results_ols %>%
  cs_tidy() %>%
  summarise(
    mean_bias = mean(att_error),
    rmse = sqrt(mean(att_error^2)),
    coverage = mean(att_covered)
  )
```

**Expected Result (typical, order-of-magnitude):**
- RMSE: typically $> 2.0$ (explodes due to Cauchy outliers)
- Coverage: typically $< 0.50$ (CIs are unreliable)

**Interpretation:** L2 loss breaks under infinite variance. This is correct behavior for this stress test.

---

### Test 2: Robust Estimator (Should Pass)

[Test with quantile regression or trimmed estimator]

**Expected Result (typical):**
- RMSE: typically $< 0.5$ (handles outliers)
- Coverage: typically $\geq 0.85$ (robust CIs)

**Interpretation:** Robust methods handle heavy tails. Stress test is valid.

**This DGP does what it claims.**
```

### 6.3 Post-MVP Enhancements (Future)

Once validation is complete, **optionally** add:

**Literature Context Section:**
```markdown
## Literature & Historical Context

This design follows [Author Year], adapted for...
```

**Pedagogical Interpretation:**
```markdown
## For Students / Practitioners

**Learning objectives:**
- Understand difference between μ₀(X) and τ(X)
- ...

**Common mistakes:**
- Confusing ATE with ATT
- ...
```

**Code Examples:**
```markdown
## Demonstration: When Estimators Fail

[Show specific failure modes]
```

**But these are NOT required for MVP validation.**

---

## 7. The Dossier Factory (Rendering Pipeline)

### 7.1 Factory Script (`dev/render_dossiers.R`)

```r
#' Render DGP Dossiers
#' 
#' Generates HTML documentation by combining:
#' - Layer 1: R code (from registry)
#' - Layer 2: YAML metadata (from inst/dgp_meta/*.yml)
#' - Layer 3: RMarkdown narrative (from inst/dgp_meta/*.Rmd)

library(CausalStress)
library(rmarkdown)
library(yaml)
library(glue)
library(purrr)
library(fs)

#' Helper for dev/installed package paths
#' Works both in development repo and after installation
cs_inst_path <- function(...) {
  p <- system.file(..., package = "CausalStress")
  if (p == "") fs::path("inst", ...) else p
}

render_dossier <- function(dgp_id, force = FALSE, output_dir = "inst/dossiers") {
  
  # Setup paths (works in dev and installed)
  yaml_path <- cs_inst_path("dgp_meta", paste0(dgp_id, ".yml"))
  rmd_path <- cs_inst_path("dgp_meta", paste0(dgp_id, ".Rmd"))
  output_file <- file.path(output_dir, paste0(dgp_id, ".html"))
  
  # Check if files exist
  if (!file.exists(yaml_path)) {
    warning(glue("No YAML metadata found for {dgp_id} - skipping"))
    return(invisible(NULL))
  }
  
  if (!file.exists(rmd_path)) {
    warning(glue("No RMarkdown narrative found for {dgp_id} - skipping"))
    return(invisible(NULL))
  }
  
  # Check if rendering needed (incremental)
  if (!force && file.exists(output_file)) {
    yaml_time <- file.mtime(yaml_path)
    rmd_time <- file.mtime(rmd_path)
    output_time <- file.mtime(output_file)
    
    if (output_time > max(yaml_time, rmd_time, na.rm = TRUE)) {
      message(glue("✓ Skipping {dgp_id} (up to date)"))
      return(invisible(output_file))
    }
  }
  
  # Load metadata
  meta <- read_yaml(yaml_path)
  
  # Validate version consistency (hard error for mismatch)
  registry_version <- cs_dgp_registry() %>% 
    filter(dgp_id == !!dgp_id) %>% 
    pull(version)
  
  if (meta$version != registry_version) {
    stop(glue(
      "Version mismatch for {dgp_id}: ",
      "code={registry_version}, yaml={meta$version}. ",
      "YAML version must match registry."
    ))
  }
  
  # Render RMarkdown with metadata injected
  message(glue("🔨 Rendering {dgp_id}..."))
  
  rmarkdown::render(
    input = rmd_path,
    output_file = basename(output_file),
    output_dir = output_dir,
    params = list(
      dgp_id = dgp_id,
      meta = meta
    ),
    quiet = TRUE
  )
  
  message(glue("✓ Generated {output_file}"))
  invisible(output_file)
}

render_all_dossiers <- function(force = FALSE, parallel = FALSE) {
  
  # Ensure output directory exists
  dir_create("inst/dossiers")
  
  # Get all DGPs from registry
  registry <- cs_dgp_registry()
  dgp_ids <- registry$dgp_id
  
  message(glue("Rendering {length(dgp_ids)} dossiers..."))
  
  if (parallel) {
    library(furrr)
    plan(multisession)
    results <- future_map(dgp_ids, render_dossier, force = force)
  } else {
    results <- map(dgp_ids, render_dossier, force = force)
  }
  
  # Summary
  n_rendered <- sum(map_lgl(results, ~ !is.null(.x)))
  message(glue("\n✓ Complete: {n_rendered}/{length(dgp_ids)} dossiers rendered"))
  
  invisible(results)
}

# Run if called directly
if (!interactive()) {
  render_all_dossiers(force = FALSE, parallel = TRUE)
}
```

### 7.2 Schema Validation (`dev/validate_yaml_schema.R`)

```r
#' Validate YAML Metadata Against Schema
#' 
#' Ensures all DGP YAML files conform to controlled vocabulary

library(yaml)
library(purrr)
library(glue)
library(rlang)

validate_stress_profile <- function(dgp_id, stress_profile, schema) {
  
  errors <- character()
  
  # Check each field
  for (field in names(schema$stress_profile_schema)) {
    
    value <- stress_profile[[field]]
    allowed <- schema$stress_profile_schema[[field]]$values
    
    if (is.null(value)) {
      errors <- c(errors, glue("{field} is missing"))
    } else if (!value %in% allowed) {
      errors <- c(errors, glue(
        "{field}='{value}' not in allowed values: [{paste(allowed, collapse=', ')}]"
      ))
    }
  }
  
  if (length(errors) > 0) {
    abort(glue(
      "Validation failed for {dgp_id}:\n",
      paste("  -", errors, collapse = "\n")
    ))
  }
  
  invisible(TRUE)
}

validate_all_yaml <- function() {
  
  # Load schema
  schema_path <- system.file("dgp_meta", "schema.yml", package = "CausalStress")
  schema <- read_yaml(schema_path)
  
  # Get all DGP YAML files
  registry <- cs_dgp_registry()
  
  message("Validating YAML metadata against schema...")
  
  walk(registry$dgp_id, function(dgp_id) {
    yaml_path <- system.file("dgp_meta", paste0(dgp_id, ".yml"), 
                             package = "CausalStress")
    
    if (!file.exists(yaml_path)) {
      warning(glue("Missing YAML for {dgp_id}"))
      return(invisible(NULL))
    }
    
    meta <- read_yaml(yaml_path)
    
    # Validate stress profile
    validate_stress_profile(dgp_id, meta$stress_profile, schema)
    
    # Validate difficulty stars
    stars <- meta$difficulty$stars
    if (!is.numeric(stars) || stars < 1 || stars > 5) {
      abort(glue("{dgp_id}: difficulty$stars must be 1-5, got {stars}"))
    }
    
    # Validate status
    allowed_status <- schema$status_schema$values
    if (!meta$status %in% allowed_status) {
      abort(glue(
        "{dgp_id}: status='{meta$status}' not in [{paste(allowed_status, collapse=', ')}]"
      ))
    }
    
    message(glue("  ✓ {dgp_id}"))
  })
  
  message("\n✓ All YAML files valid")
  invisible(TRUE)
}

# Run if called directly
if (!interactive()) {
  validate_all_yaml()
}
```

---

## 8. Integration with CausalStress Core

### 8.1 Registry Enhancement

Update `cs_dgp_registry()` to merge YAML metadata with graceful fallbacks:

```r
cs_dgp_registry <- function() {
  
  # Base registry (immutable, authoritative for version)
  registry <- tibble::tribble(
    ~dgp_id,              ~type,       ~generator,                ~version,
    "synth_baseline",     "synthetic", dgp_synth_baseline,        "1.3.0",
    "synth_heavytail",    "synthetic", dgp_synth_heavytail,       "1.3.0",
    # ... more entries
  )
  
  # Enrich with YAML metadata (with graceful fallbacks)
  registry <- registry %>%
    mutate(
      yaml_meta = map(dgp_id, ~ {
        yaml_path <- system.file("dgp_meta", paste0(.x, ".yml"), 
                                 package = "CausalStress")
        if (file.exists(yaml_path)) {
          yaml::read_yaml(yaml_path)
        } else {
          NULL  # Missing YAML is OK - use defaults
        }
      })
    ) %>%
    mutate(
      # Unpack key fields with sensible defaults
      status = map_chr(yaml_meta, "status", .default = "experimental"),
      difficulty_stars = map_int(yaml_meta, c("difficulty", "stars"), .default = NA_integer_),
      stress_profile = map(yaml_meta, "stress_profile"),
      title = map_chr(yaml_meta, c("meta", "title"), .default = NA_character_)
    )
  
  # Validate version consistency (HARD ERROR for mismatch)
  # Missing YAML → warning only
  walk(seq_len(nrow(registry)), function(i) {
    if (!is.null(registry$yaml_meta[[i]])) {
      yaml_version <- registry$yaml_meta[[i]]$version
      code_version <- registry$version[[i]]
      
      if (yaml_version != code_version) {
        rlang::abort(sprintf(
          "Version mismatch for %s: code=%s, yaml=%s. YAML must match registry.",
          registry$dgp_id[[i]], code_version, yaml_version
        ))
      }
    } else {
      # Missing YAML is OK for experimental DGPs
      rlang::warn(sprintf(
        "No YAML metadata for %s. Defaulting to status='experimental'.",
        registry$dgp_id[[i]]
      ))
    }
  })
  
  registry
}
```

**Design Principle:** Missing YAML file → warning + `status="experimental"`. Only version **mismatch** (YAML exists but wrong version) triggers hard error.

### 8.2 Discovery Helpers

```r
#' Find DGPs by Stress Profile
#' 
#' @param noise Noise type (gaussian, heavy, heteroskedastic)
#' @param overlap Overlap level (none, mild, moderate, severe, extreme)
#' @param effect Effect structure (constant, linear, nonlinear, heterogeneous)
#' @param min_difficulty Minimum difficulty stars (1-5)
#' @param max_difficulty Maximum difficulty stars (1-5)
#' 
#' @export
cs_find_dgps <- function(
  noise = NULL,
  overlap = NULL,
  effect = NULL,
  min_difficulty = NULL,
  max_difficulty = NULL
) {
  
  registry <- cs_dgp_registry()
  
  # Filter by stress profile
  if (!is.null(noise)) {
    registry <- registry %>%
      filter(map_chr(stress_profile, "noise") == noise)
  }
  
  if (!is.null(overlap)) {
    registry <- registry %>%
      filter(map_chr(stress_profile, "overlap") == overlap)
  }
  
  if (!is.null(effect)) {
    registry <- registry %>%
      filter(map_chr(stress_profile, "effect") == effect)
  }
  
  # Filter by difficulty
  if (!is.null(min_difficulty)) {
    registry <- registry %>%
      filter(difficulty_stars >= min_difficulty)
  }
  
  if (!is.null(max_difficulty)) {
    registry <- registry %>%
      filter(difficulty_stars <= max_difficulty)
  }
  
  registry %>%
    select(dgp_id, title, status, difficulty_stars, stress_profile)
}

#' Browse DGP Dossier
#' 
#' @param dgp_id DGP identifier
#' @export
cs_browse_dgp <- function(dgp_id) {
  dossier_path <- system.file("dossiers", paste0(dgp_id, ".html"),
                              package = "CausalStress")
  
  if (!file.exists(dossier_path)) {
    rlang::abort(glue::glue(
      "No dossier found for {dgp_id}. ",
      "Run render_dossiers.R to generate."
    ))
  }
  
  utils::browseURL(dossier_path)
  invisible(dossier_path)
}

#' Generate DGP Comparison Matrix
#' 
#' @export
cs_dgp_comparison_matrix <- function() {
  registry <- cs_dgp_registry()
  
  registry %>%
    mutate(
      overlap = map_chr(stress_profile, "overlap", .default = NA_character_),
      noise = map_chr(stress_profile, "noise", .default = NA_character_),
      linearity = map_chr(stress_profile, "linearity", .default = NA_character_),
      effect = map_chr(stress_profile, "effect", .default = NA_character_)
    ) %>%
    select(dgp_id, title, status, difficulty_stars, 
           overlap, noise, linearity, effect) %>%
    arrange(difficulty_stars, dgp_id)
}
```

---

## 9. Implementation Roadmap

### Phase 1: MVP Foundation (Week 1)
**Goal:** Get 2 validated DGPs documented with empirical evidence

- [ ] Define schema.yml with controlled vocabulary
- [ ] Create validation script (validate_yaml_schema.R)
- [ ] Implement version-checking in cs_dgp_registry()
- [ ] Add naive estimators to registry:
  - [ ] `naive_dim`: Simple difference-in-means (no adjustment)
  - [ ] `naive_trim`: Trimmed mean (for heavy-tail tests)
- [ ] Migrate synth_baseline to Sidecar format
  - [ ] Create synth_baseline.yml (metadata only)
  - [ ] Create synth_baseline.Rmd (math + validation plots + empirical tests)
  - [ ] Run empirical validation: naive_dim, oracle_att, lm_att
  - [ ] Verify expected failures/successes
- [ ] Migrate synth_heavytail to Sidecar format
  - [ ] Run empirical validation: lm_att (should fail), oracle_att (should pass)
  - [ ] Document RMSE explosion under Cauchy noise
- [ ] Test rendering pipeline
- [ ] **Validation gate:** Review both dossiers - do they convince you?

**Deliverables:**
- 2 complete dossiers with empirical proof of stress behavior
- Naive estimators in registry for testing
- Confidence that DGPs do what you designed them to do

**Key Success Metric:** You can show a reviewer the empirical validation section and say "See? The naive method fails exactly as expected."

### Phase 2: Scale to Full Library (Week 2)
**Goal:** Document and empirically validate all 12 DGPs

- [ ] For each DGP, create:
  - [ ] Minimal YAML (stress profile, difficulty)
  - [ ] RMarkdown with math + validation plots
  - [ ] **Empirical validation section** with appropriate naive tests
- [ ] Define per-DGP validation protocols:
  - **Placebo DGPs:** naive methods should find zero effect (or close to zero)
  - **Overlap stress:** naive IPW should fail (infinite weights)
  - **Heavy-tail:** OLS should fail (RMSE explosion)
  - **Nonlinear:** Linear methods should be biased
  - **QTE:** Constant-effect estimators should miss heterogeneity
- [ ] Run all empirical validations (cached in dossiers)
- [ ] Render all dossiers
- [ ] **Validation gate:** Review each DGP's empirical results

**Deliverables:**
- 12 validated dossiers
- Empirical proof that each DGP stresses what it claims
- Personal confidence in your DGP library

**Time estimate (rough ballpark):** 
On the order of a few hours per DGP for full YAML, math, plots, and empirical validation. Total effort likely spans 2-3 full days of focused work, though individual DGPs vary in complexity.

### Phase 3: Tooling & Discovery (Week 3)
**Goal:** Make DGPs programmatically discoverable

- [ ] Implement cs_find_dgps()
- [ ] Implement cs_browse_dgp()
- [ ] Implement cs_dgp_comparison_matrix()
- [ ] Add pkgdown integration
- [ ] Create helper: cs_validate_dgp(dgp_id) - runs empirical tests on demand

**Deliverables:**
- Programmatic DGP filtering
- On-demand validation runner
- Easy access to documentation

### Phase 4: Polish (Optional - Post-PhD if time)
**Goal:** Transform into teaching materials

- [ ] Add literature context sections
- [ ] Add pedagogical interpretations
- [ ] Add more code examples
- [ ] Create comprehensive catalog vignette

**Deliverables:**
- University-grade teaching materials

**Decision Point:** Only pursue Phase 4 if:
1. PhD defense is secure
2. There's clear demand (teaching opportunity)
3. Not blocking other priorities

---

## 10. Usage Examples

### Example 1: Validating a DGP (Your Use Case)

```r
library(CausalStress)

# Quick validation: does synth_heavytail break OLS as expected?
cs_validate_dgp("synth_heavytail", 
                estimator_ids = c("oracle_att", "lm_att"),
                seeds = 1:20)

# Expected output:
# ✓ oracle_att: RMSE = 0.001 (correct truth system)
# ✗ lm_att: RMSE = 2.45 (L2 loss breaks under Cauchy)
# Conclusion: DGP correctly stresses non-robust estimators
```

### Example 2: Finding DGPs by Stress Type

```r
library(CausalStress)

# Find all DGPs with heavy-tailed noise
heavy_dgps <- cs_find_dgps(noise = "heavy")

# Find DGPs suitable for sanity checks (1 star difficulty)
sanity_checks <- cs_find_dgps(max_difficulty = 1)

# Find severe overlap stress tests
overlap_dgps <- cs_find_dgps(overlap = "severe")
```

### Example 3: Browsing Documentation

```r
# Open dossier in browser (includes empirical validation results)
cs_browse_dgp("synth_baseline")

# View comparison matrix
cs_dgp_comparison_matrix()
```

### Example 4: Building a Custom Suite

```r
# Create a "robustness" suite from metadata
robustness_suite <- cs_find_dgps(noise = "heavy") %>%
  pull(dgp_id)

# Run benchmarks
results <- cs_run_grid(
  dgp_ids = robustness_suite,
  estimator_ids = c("lm_att", "ipw_att", "grf_dr_att"),
  n = 1000,
  seeds = 1:50
)
```
  n = 1000,
  seeds = 1:50
)
```

---

## 11. Benefits Summary

### For PhD Defense (Primary Goal)
✅ **Specialist validation** - Reviewers can verify DGPs are correctly implemented  
✅ **Mathematical transparency** - LaTeX specs show exactly what you're testing  
✅ **Visual evidence** - Plots prove stress conditions exist  
✅ **Reproducible** - Version-locked, immutable implementations  

### For Researchers (MVP Scope)
✅ **Programmatic discovery** - Find DGPs by stress type  
✅ **Clear specifications** - Know exactly what's being tested  
✅ **Validation evidence** - Trust the DGP does what it claims  

### For Students (Post-MVP Enhancement)
🔲 **Pedagogical structure** - To be added after validation complete  
🔲 **Narrative explanations** - Future enhancement  
🔲 **Code examples** - Future enhancement  

### For the Project (MVP Focus)
✅ **Constitutional compliance** - Immutability preserved  
✅ **Validation-ready** - Specialists can review and approve  
✅ **Maintainable** - Docs evolve without code changes  
✅ **Differentiated** - No other framework has this rigor  

---

## 12. Conclusion

The Sidecar Pattern solves the documentation-immutability conflict by:

1. **Separating concerns** - Law (code) vs. Tags (metadata) vs. Validation (math/plots)
2. **Moving math to narrative** - Where it belongs (scientific specification, not classification)
3. **Enabling validation** - Specialists can verify DGPs are correctly implemented
4. **Scaling gracefully** - Incremental rendering supports large libraries
5. **Respecting the Constitution** - No code changes for documentation improvements

**MVP Success Criteria:**
- A reviewer reads a dossier and says: "Yes, this DGP is correct and tests what it claims"
- All 12 DGPs have validated math specifications and evidence plots
- Specialists can programmatically discover DGPs by stress type

**Post-MVP Enhancements (when time permits):**
- Literature citations and historical context
- Pedagogical interpretations for teaching
- Code examples and common pitfalls
- Interactive features

This transforms CausalStress from "simulation code" into **"validated scientific infrastructure"** suitable for PhD defense and future research.

**Status:** Ready for MVP implementation.  
**Risk level:** Low (well-scoped, clear validation gates).  
**Strategic value:** High (enables thesis defense, differentiates framework).

---

## Appendix A: Migration Checklist (MVP Version)

For each DGP being migrated to Sidecar format:

**Minimum viable documentation:**

- [ ] Create `inst/dgp_meta/{dgp_id}.yml`
  - [ ] Add identity: dgp_id, version, status, type
  - [ ] Add stress_profile tags (use controlled vocabulary)
  - [ ] Add difficulty stars (estimator challenge level)
  - [ ] Validate against schema
  
- [ ] Create `inst/dgp_meta/{dgp_id}.Rmd`
  - [ ] State what this stresses (1-2 sentences)
  - [ ] Add mathematical specification (LaTeX)
  - [ ] Generate validation plots (overlap, heterogeneity, etc.)
  - [ ] Add verification code chunk
  
- [ ] Validate
  - [ ] Run `validate_yaml_schema.R`
  - [ ] Render dossier
  - [ ] Check version consistency
  - [ ] **Have specialist review and approve**

**Optional enhancements (post-MVP):**
- [ ] Add literature citations
- [ ] Write pedagogical interpretation
- [ ] Add code examples
- [ ] Document common pitfalls

---

## Appendix B: Template Files

Templates will be provided in `inst/templates/`:
- `dgp_metadata_template.yml` - Minimal viable YAML
- `dgp_narrative_mvp_template.Rmd` - Math + validation plots only
- `dgp_narrative_full_template.Rmd` - With pedagogy (future use)

These ensure consistency and make migration fast. 