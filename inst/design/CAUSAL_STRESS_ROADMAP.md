# CausalStress v1.0.0 Roadmap: The "Wide & Shallow" Path

**Current Version:** v0.1.8
**Target:** v1.0.0 (JSS Submission + 14M Fit Campaign)
**Timeline:** 4-Week Sprint
**Philosophy:** Scientific Rigor > Architectural Complexity.

---

## 1. The Challenge (Context)

We are running a massive benchmarking campaign:
* **Scale:** 14 DGPs × 10 Estimators × 500 Seeds × 200 Bootstraps ≈ **14,000,000 model fits**.
* **Estimand:** **ATT** (Average Treatment Effect on the Treated) is the sole target unless explicitly stated.
* **Compute Requirement:** Benchmarks indicate high CPU cost per run. Target hardware is a **64-thread workstation** (or equivalent cluster).
* **Constraint 1 (Scientific):** Results must be bitwise reproducible across platforms (Constitution Article II).
* **Constraint 2 (Operational):** Managing 70,000 individual result files causes I/O latency; databases add unnecessary complexity.
* **Solution:** **Randomized Batching**. We will aggregate results into ~350 batch files (200 runs each) using a deterministic shuffle.

---

## Phase 1: Hardening (v0.1.6)
**Goal:** Ensure the instrument is scientifically valid before scaling.
**Effort:** ~1 Day

### 1.1 Strict RNG Locking (Critical)
`future` workers can drift in RNG implementation across OSs, violating the "Frozen Logic" clause.
* **Action:** Add `cs_set_rng(seed)` as the very first line of `cs_run_one_seed_internal`.
* **Verification:** Bitwise identity test between `plan(sequential)` and `plan(multisession)`.

### 1.2 Robust Bootstrap
One singular matrix in bootstrap replicate #199 must not crash the whole seed.
* **Action:** Wrap estimator calls inside the bootstrap loop with `tryCatch`.
* **Logic:** If a replicate fails, decrement `n_boot_ok` and continue. Only fail the run if `n_boot_ok < B * 0.9`.

### 1.3 Unambiguous Noise Notation
* **Action:** Update all documentation and the Constitution to explicitly specify "Standard Deviation $\sigma = 0.5$" (vs Variance = 0.25).

---

## Phase 2: Validation (v0.1.7)
**Goal:** Verify the 14 existing DGPs. (No new DGPs).
**Effort:** ~2 Days

### 2.1 The DGP Validator Tool
Create `cs_validate_dgp(dgp_fn)` to automate quality control.
* **Check 1 (Stability):** Coefficient of Variation of `true_att` across 100 seeds < 5%.
* **Check 2 (Determinism):** Seed $X$ always produces identical dataframe $Y$.
* **Check 3 (Contract):** Output format matches Constitution (correct columns, metadata).
* **Check 4 (Adversarial Sanity):** Run a naive estimator (Difference-in-Means).
    * *Baseline:* Bias must be reasonable (not massive).
    * *Placebo:* Estimate must be $\approx 0$.
    * *Rationale:* Catches "hallucination DGPs" where logic is valid but signal is broken.

### 2.2 Manual Metadata Review
* **Decision:** Do not write an automated schema validator.
* **Action:** Manually review the 14 YAML files in `inst/dgp_meta/`. Ensure tags are consistent (e.g., `heavy_tail` not `heavytail`) and difficulty stars match empirical reality.

---

## Phase 3: Governance & Integrity (v0.1.8)
**Goal:** Close governance gaps before scaling.
**Effort:** ~2 Days

### 3.1 Persistent Oracle Cache (Disk-based)
Cache oracle truth to disk using `tools::R_user_dir(...)` to avoid redundant
re-simulation across parallel workers.

### 3.2 Registry Fail-Closed Logic
Abort if a DGP sidecar YAML is missing to prevent silent ?fail-open? behavior.

### 3.3 Strict CI Intent (No Silent Bootstraps)
When `bootstrap = FALSE`, enforce `ci_method = "none"` unless explicitly set.

### 3.4 Parallel Governance (Experimental Gating)
Gate parallel execution behind `experimental_parallel = TRUE` with strict thread caps.

---

## Phase 4: Scale & Architecture (v0.1.9)
**Goal:** Solve the I/O bottleneck using Deterministic Randomized Batching.
**Effort:** ~2 Hours

### 4.1 The "Batch ID" Strategy
We replace the "One Pin Per Run" model with "One File Per Batch".

* **Refactor `cs_run_campaign`:**
    1.  **Expand:** Generate the full task grid (DGP ? Est ? Seed = 70,000 rows).
    2.  **Deterministic Shuffle:** `set.seed(campaign_seed); tasks <- tasks[sample(nrow(tasks)), ]`.
        * *Why:* Ensures Batch 1 always contains the same tasks, enabling resume logic.
        * *Benefit:* Perfect load balancing (slow/fast tasks mixed).
    3.  **Chunk:** Assign `batch_id` (1 to 350).
    4.  **Resume Logic:** If `results/batches/batch_101.qs` exists, skip it.

### 4.2 The Batch Runner (`cs_run_batch_internal`)
Create an internal worker function to handle the chunk.
* **Input:** Dataframe of 200 tasks.
* **Process:**
    * Loop through tasks in memory.
    * Regenerate DGP for each task (proven cost: <10ms, negligible).
    * Run Estimator.
    * `bind_rows()` results into one tibble.
* **Output:** Atomic write to `results/batches/batch_{id}.qs`.

### 4.3 Filesystem Hygiene
* **Outcome:** Campaign produces **~350 files** (approx 200-500MB each).
* **Justification:** Large files are optimized for sequential reads and avoid filesystem fragmentation/inode exhaustion.
* **Benefit:** Trivial to copy, backup, and list.

### 4.4 The Simple Aggregator
We do not need DuckDB. We have plenty of RAM for the summary statistics.

* **Script:** `R/cs-campaign-aggregate.R`
* **Logic:**
    1.  List all `*.qs` files in `results/batches/`.
    2.  Use `purrr::map_dfr` to read and bind them.
    3.  Select summary columns (Bias, Coverage, RMSE, `n_boot_ok`). Drop raw vectors.
    4.  Save to `analysis/campaign_results.rds` (~20-50MB).

### 4.5 JSS Figure Generation
* **Action:** Point `ggplot2` scripts directly at `campaign_results.rds`.

---

## Production: The v1.0.0 Freeze

### 5.1 The Freeze Protocol
Before the campaign starts:
1.  **Tag:** `git tag v1.0.0`
2.  **Lock:** Add "FROZEN" comments to `cs_dgp_registry.R`.
3.  **Docs:** Ensure `NEWS.md` and `README.md` reflect the frozen state.

### 5.2 Campaign Execution
1.  **Hardware:** 64-thread workstation (Threadripper/Epyc) or equivalent HPC node.
    * *Benchmark:* GRF takes ~1-5s per fit. 14M fits on 16 cores ≈ weeks. 64 threads reduces this to days.
2.  **Config:** `batch_size = 200`.
3.  **Command:** `cs_run_campaign(..., skip_existing = TRUE)`.
4.  **Monitoring:** Count files: `ls results/batches | wc -l`. (Target: ~350).

---

## The "Anti-Roadmap" (What we cut)
* **No New DGPs:** Pushed to v1.1.0.
* **No Dashboard:** HTML/Shiny is unnecessary overhead.
* **No Status YAML:** Avoids race conditions. Filesystem existence is the only truth.
* **No DuckDB:** `readRDS` is faster and simpler for this data volume.
* **No Seed Bundling:** Decouples estimators to prevent one crash from killing the whole seed.



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
