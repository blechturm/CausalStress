# CausalStress DGP Registry Specification

**Status:** Final (Version 1.3.0) **Date:** 2025-11-28

------------------------------------------------------------------------

## Quick Reference Matrix (Synthetic)

| Registry ID | $\mu_0(X)$ | $\tau(X)$ | Noise | Challenge |
|:--------------|:--------------|:--------------|:--------------|:--------------|
| **`synth_baseline`** | Linear | Linear | Gaussian | Sanity Check |
| **`synth_heavytail`** | Linear | Linear | Gauss+Cauchy | Robustness (L2 break) |
| **`synth_qte1`** | Linear | Sign Flip | Student-t ($df=4$) | Heterogeneity / QST |
| **`synth_nonlinear_heteroskedastic`** | Nonlinear | Constant ($1.0$) | Heteroskedastic | Curve Fitting |
| **`synth_overlap_stressed`** | Linear | Linear | Gaussian | Severe Overlap ($p \to 0/1$) |
| **`synth_tilt_mild`** | Linear | Linear | Gaussian | Covariate Shift |
| **`synth_placebo_*`** | Varies | Constant ($0$) | Varies | Gatekeeper (False Positives) |

*Note: Real Data Registry is defined in Section 4.*

------------------------------------------------------------------------

## 1. Simulation Philosophy

### 1.1 The "Two-Tier" Truth Contract

We distinguish between the causal *signal* (structural) and the causal *distribution* (realized).

1.  **ATT Truth (Structural):** Defined on the **noise-free** structural treatment effect $\tau(X)$.
    -   $\tau(X_i) = \mathbb{E}[Y_1 - Y_0 \mid X_i]$.
    -   $ATT_{true} = \frac{1}{N_{treated}} \sum_{i: W_i=1} \tau(X_i)$.
2.  **QST Truth (Distributional):** Defined on the **full data-generating distribution** (signal + noise).
    -   Let $u \in (0,1)$ denote the quantile level.
    -   $QST(u) = Q_u(Y_1 \mid W=1) - Q_u(Y_0 \mid W=1)$.
    -   *Canonical Grid:* Truth is defined for $u \in \{0.01, 0.02, \dots, 0.99\}$.
    -   *Oracle Definition:* Computed via Monte Carlo ($N_{oracle}=10^6$) by simulating full noisy outcomes $(Y_1, Y_0)$ conditioned on the treated population distribution $X \mid W=1$.
    -   *Performance Note:* Implementations SHOULD cache the oracle truth table per DGP version.

### 1.2 Generation Order & Structure

1.  **Covariates:** Draw $X$. (Dimensionality depends on DGP).
2.  **Propensity:** Compute $p(X)$.
3.  **Treatment:** Draw $W \sim \text{Bernoulli}(p(X))$.
4.  **Outcomes:** For all synthetic (non-placebo) DGPs, potential outcomes are generated via the additive model: $$Y_0 = \mu_0(X) + \varepsilon_0$$ $$Y_1 = \mu_0(X) + \tau(X) + \varepsilon_1$$
    -   **Independence:** $\varepsilon_0, \varepsilon_1$ are independent draws from the specified noise law.
    -   **Placebo Exception:** For Placebos, we set $\tau(X) \equiv 0$ and enforce the Sharp Null pathwise by setting $Y_1 \equiv Y_0$ (identical realization).
5.  **Observation:** $Y = W Y_1 + (1-W) Y_0$.

### 1.3 Global Defaults

-   **RNG:** Locked to `RNGkind("Mersenne-Twister", "Inversion", "Rounding")` to prevent sample rejection drift in newer R versions."
-   **Covariates:** $X \sim \mathcal{N}(0, I_5)$ unless specified.
-   **Noise Scale:** $\epsilon \sim \mathcal{N}(0, 0.5)$ unless specified. Note that $0.5$ is the Standard Deviation ($\sigma$), not variance.

------------------------------------------------------------------------

## 2. Synthetic Registry (Canonical Parameters)

### 2.1 `synth_baseline`

**Goal:**

Sanity check. The "Happy Path."

-   **Covariates:** $X \in \mathbb{R}^5$. Active: $X_1, X_2$.

-   **Baseline Outcome:** $\mu_0(X) = 1 + X_1 + 0.5 X_2$.

-   **Treatment Effect:** $\tau(X) = 1 + 0.5 X_1$.

-    **Propensity:** $p(X) = \text{plogis}(0.5 X_1 - 0.5 X_2)$.

-   **Noise:** $\epsilon \sim \mathcal{N}(0, 0.5)$.

### 2.2 `synth_heavytail`

**Goal:** Penalize non-robust loss functions (L2/MSE).

**Structure:** Same $\mu_0, \tau, p$ as `synth_baseline`.

-   **Noise Mixture:** $\epsilon \sim 0.8 \cdot \mathcal{N}(0, 0.5) + 0.2 \cdot \text{Cauchy}(0, 1)$.

-   **Note:** Variance undefined. Winsorization is forbidden *within the DGP generation code*.

### 2.3 `synth_qte1`

**Goal:** Verify detection of distributional heterogeneity.

**Structure:** Same $X, \mu_0, p$ as `synth_baseline`.

-   **Mechanism:** $\tau(X)$ flips sign based on $X_1$.

    -    $\tau(X) = +1.0$ if $X_1 > 0$.

    -    $\tau(X) = -1.0$ if $X_1 \le 0$.

-   **Noise:** Student-t ($df=4$) scaled by $\sigma=0.5$.

### 2.4 `synth_nonlinear_heteroskedastic`

**Goal:** Test curve fitting and variance adaptation.

-   **Covariates:** Defined on $\mathbb{R}^4$. (Note: No $X_5$).

    -    $X_1, X_2 \sim \mathcal{N}(0,1)$.

    -    $X_3 \sim \mathcal{U}[-2,2]$.

    -   $X_4 \sim \text{Bernoulli}(0.4)$

-   **Outcome:** $Y_0 = 1 + 0.8\sin(X_1) + 0.5 X_2^2 - 0.3 X_4$.

-   **Treatment Effect:** $\tau(X) = 1.0$ (Constant).

-   **Noise:** Gaussian with heteroskedastic scale: $\varepsilon \sim \mathcal{N}(0, \sigma(X)^2)$ where $\sigma(X) = 0.3 + 0.2|X_3|$.

-   **Propensity:** $p(X) = \text{plogis}(0.5 X_1 - 0.5 X_2)$.

### 2.5 `synth_tilt_mild`

**Goal:** Test basic propensity reweighting (covariate shift).

**Structure:** Same outcomes/noise as `synth_baseline`.

-   **Covariates:** $X \sim \mathcal{N}(0, I_5)$ (Standard Global Default).

-   **Propensity:** $p(X) = \text{plogis}(0.45 X_1 - 0.3 X_2 - 0.25 X_4)$.

-   **Clipping:** None in DGP. Typical estimators clip to $[0.05, 0.95]$.

### 2.6 `synth_overlap_stressed`

**Goal:** Stress-test weighting stability.

**Structure:** Same outcomes/noise as `synth_baseline`.

-   **Propensity:** $p(X) = \text{plogis}(3.0 X_1 + 3.0 X_2)$.

-   **Intention:** Pushes density to $0$ and $1$.

------------------------------------------------------------------------

## 3. Placebo Suite (The Gatekeeper)

Strictly enforces **Sharp Null**: $Y_1 \equiv Y_0$. *Implementation:* $\tau(X) \equiv 0$ and $Y_1$ is a pathwise copy of $Y_0$. No independent noise draw for treatment.

### 3.1 Truth Definition (All Placebos)

-   `structural_te` = vector of zeros.
-   `true_att` = 0.
-   `true_qst` = Tibble where `value` column is 0 for all $u \in \{0.01 \dots 0.99\}$.

### 3.2 Registry

| ID | Nuance | Parameterization |
|----------------|-----------------|----------------------------------------|
| `synth_placebo_tau0` | Linear Baseline | Outcome/Noise/Propensity same as `synth_baseline`. |
| `synth_placebo_nonlinear` | Sine/Cosine $Y_0$ | $\mu_0 = \sin(X_1) + \cos(X_2)$. Propensity/Noise same as `synth_baseline`. |
| `synth_placebo_heavytail` | Cauchy Noise | Outcome/Propensity same as `synth_baseline`. Noise same as `synth_heavytail`. |
| `synth_placebo_tilted` | Strong Selection | Outcome/Noise same as `synth_baseline`. $p(X) = \text{plogis}(1.0 X_1 + 1.2 X_2)$. |

------------------------------------------------------------------------

## 4. Real Data Registry

Real datasets conform to `type="real"`. In this spec, "real" means "truth is externally fixed and not generated by a CausalStress function," which includes semi-synthetic benchmarks like IHDP.

### 4.1 Truth Definitions (Benchmarking Convention)

The field `true_att` **MUST** be present in the return object.

Note for v0.2: Ensure strict separation between DW subset (N=445) and Full NSW (N=722). Truth differs by \~\$900.

-   **`lalonde_nsw`:** $1794$ (Experimental).
-   **`lalonde_dw`:** $1794$ (Target).
-   **`lalonde_cps`:** $1794$ (Target).
-   **`lalonde_psid`:** $1794$ (Target).
-   **`ihdp`:** $4.0$ (Hill 2011 SATT).

For these IDs, `true_att` may be `NA`: \* **`twins` / `401k`:** Used for relative comparison only.

-   **QST:** Always `NULL` for real data.

------------------------------------------------------------------------

## 5. Implementation Guidelines

### 5.1 Return Object Contract

**A. Synthetic DGPs (`type="synthetic"`)**

Includes all `synth_*` and `synth_placebo_*`.

MUST return full counterfactuals and truth.

The set of covariates in df MUST match the DGP

definition df = tibble(y, w, y0, y1, p, x1, ...),

y0, y1, p REQUIRED true_att = <numeric>,

REQUIRED true_qst = <tibble>

Columns: 'tau' (numeric u), 'value' (numeric QST) meta = list( type = "synthetic",

REQUIRED structural_te = <numeric vector>