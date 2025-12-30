# CAUSALSTRESS CONSTITUTION

**Version:** 1.8.1\
**Date:** 2025-12-01\
**Status:** Ratified (Stabilized Protocol)

------------------------------------------------------------------------

## Preamble

`CausalStress` is a scientific instrument, not merely a software library.\
This Constitution codifies the *normative, versioned rules* that ensure benchmarks remain comparable across time, machines, and estimator implementations.

The Constitution is **stable but not frozen**:\
patch-level revisions MAY clarify intent, tighten definitions, or correct ambiguities,\
but MUST NOT change the semantic meaning of any article without a *major* version bump.

All contributors must treat this document as the supreme authority.\
Any code proposal that violates it must be rejected.

------------------------------------------------------------------------

## Article I: The Definition of Truth

To prevent ambiguity between "Signal" and "Noise," all Synthetic DGPs must adhere to the **Two-Tier Truth Contract**.

### Section 1.1: Interpretation

Where ambiguity exists, **the strictest interpretation prevails**, favoring reproducibility, transparency, and estimator safety.

### Section 1.2: Authority to Define DGPs

Only the core maintainers may define new DGP IDs.\
Community contributions MUST pass constitutional validation and MUST be versioned according to Article II.

### Section 1.3: Structural ATT

The Average Treatment Effect on the Treated (ATT) is defined strictly on the **noise-free structural component**. $$ATT_{true} = \frac{1}{N_{treated}} \sum_{i: W_i=1} \mathbb{E}[Y_1 - Y_0 \mid X_i]$$

-   **Constraint:** For Synthetic DGPs, the structural treatment effect $\tau(X)$ MUST be a deterministic measurable function of covariates $X$ only. It MUST NOT depend on treatment assignment $W$, propensity $p(X)$, or realized sample noise.

-   **Prohibition:** Truth must never be calculated as the sample mean of realized differences ($y_1 - y_0$) in heavy-tailed settings.

-   **Real Data:** For Real DGPs, Truth must be defined externally (e.g., Experimental Benchmark) and never regenerated. **This truth is considered the Stabilized Experimental Estimate and is permitted to include inherent sampling noise.**

### Section 1.4: Distributional QST

The Quantile Shift (QST) is defined on the **full realized distribution** (Signal + Noise). $$QST(u) = Q_u(Y_1 \mid W=1) - Q_u(Y_0 \mid W=1)$$

-   **Grid:** The canonical truth grid is invariant: $u \in \{0.01, 0.02, \dots, 0.99\}$.

-   **Computation:** For synthetic data, this **must** be computed via Oracle Monte Carlo ($N=10^6$) or analytic derivation matching oracle precision ($< 10^{-5}$). The oracle size $N=10^6$ is immutable for v1.x.y.

-   **Independence:** Noise MUST be drawn independently across units unless explicitly specified.

------------------------------------------------------------------------

## Article II: Immutability and Reproducibility

Scientific benchmarks are worthless if the ground moves under our feet. To ensure that "Truth" remains identical across time, machines, and R versions, the computational substrate must be frozen.

### Section 2.1: The Frozen Logic Clause

Once a DGP ID and Version is released, its logic, parameters, and truth definitions are **Immutable**.

-   **Prohibition:** You cannot "fix" or "improve" a DGP in place. Any change to parameters or logic requires a version increment.

-   **RNG Stationarity:** To prevent "Dependency Rot" (e.g., changes in sampling algorithms between R versions), all synthetic data generation **MUST** occur within a fixed RNG context.

-   **Mandated State:** `RNGkind(kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rounding")`.

-   *Rationale:* This enforces the "classic" R behavior (pre-3.6.0), ensuring that a seed used in 2025 produces the exact same dataset in 2030.

-   **Side-Effect Isolation:** Aside from the mandated RNG lock, DGPs **MUST NOT** modify external system state, including:

-   `options()` (e.g., numeric precision). System time or locale settings.

-   Parallel backend configurations (BLAS/LAPACK threads).

### Section 2.2: The Seeding Mandate

Reproducibility is not optional; it is the primary function of the instrument.

-   **Explicit Arguments:** All DGPs **must** accept a `seed` argument.

-   **Internal Setting:** If provided, the DGP **must** call `set.seed(seed)` internally, immediately after establishing the RNG context defined in Section 2.1.

-   **Traceability:** The Runner **must** capture and store the seed in the result metadata.

-   **Bitwise Identity:** Two runs with the same `DGP ID`, `Version`, and `Seed` must produce **bitwise identical** dataframes and truth tables, regardless of the operating system or R version used.

------------------------------------------------------------------------

## Article III: The Interoperability Contracts

### Section 3.1: The Estimator Contract

Every estimator function must conform to: `function(df, tau, config) -> list(att, qst, meta)`.

-   **Covariate Access:** The Runner MUST physically sanitize `y0`, `y1`, `p`, and `structural_te` from the input dataframe before execution. Estimators will not receive these columns unless explicitly configured as Oracle
    -   *Exception:* Estimators explicitly configured as "Oracle" (e.g., `config$use_true_propensity = TRUE`) MAY access `p`.
-   **Tau Compliance:** Estimators **MUST** calculate QST only for the `tau` values provided by the Runner. The canonical grid applies to truth tables, not estimator inputs.
-   **Confidence Intervals:**
    -   Gatekeeper testing applies primarily to ATT.
    -   If an estimator reports CIs, they **MUST** be Bootstrap-based (default) unless the estimator explicitly declares an alternative valid methodology (e.g., Asymptotic, Bayesian) in `meta$ci_type`.

### Section 3.2: The DGP Contract (Bifurcated)

**A. Synthetic DGPs (`type="synthetic"`)** MUST return:

-   `df`: Tibble including `y`, `w`, `p`, `y0`, `y1`, and covariates named `x1...xk`.

-   `true_att`: Numeric scalar.

-   `true_qst`: Tibble with columns `tau` (numeric) and `value` (numeric).

-   **Strict Alignment:** This table **MUST** be evaluated exactly at the canonical grid $\{0.01, \dots, 0.99\}$. Any deviation in grid points renders the DGP unconstitutional.

-   `meta$structural_te`: Numeric vector matching `nrow(df)`.

**B. Real DGPs (`type="real"`)** MUST return:

-   `df`: Tibble including `y`, `w`, and covariates. **MUST NOT** include `y0`, `y1`.

-   `true_att`: Numeric scalar (if benchmark available) OR `NA`.

-   `true_qst`: **MUST** be `NULL`.

-   `meta$structural_te`: **MUST** be `NULL`.

-   `meta$params`: Empty list `list()` or citation metadata only.

------------------------------------------------------------------------

## Article IV: The Placebo Gatekeeper

To prevent the proliferation of estimators that hallucinate effects.

### Section 4.1: The Sharp Null (Pathwise Identity)

All DGPs in the Placebo Suite must enforce the **Sharp Null Hypothesis** via pathwise identity: $$Y_1 \equiv Y_0$$

-   **Implementation:** $Y_1$ is a copy by reference of $Y_0$. No independent noise is drawn for $Y_1$.

-   **Rationale:** This ensures $QST(u) \equiv 0$ exactly (zero variance in the estimand), detecting estimators that hallucinate signal from noise variance ("Variance Hallucination").

### Section 4.2: The Gatekeeper Protocol

The Gatekeeper enforces that any estimator claiming robustness must demonstrate the ability to preserve the Sharp Null across the entire Placebo Suite.

#### 4.2.1 Purpose

The Gatekeeper detects estimators that hallucinate treatment effects in the absence of signal—typically due to model misspecification, overfitting to noise variance, or unstable weighting.

#### 4.2.2 Criterion for Passing

An estimator **passes** the Gatekeeper if it preserves the Sharp Null within nominal error rates:

-   **For ATT (Scalar):** The 95% confidence interval for the ATT must include zero in at least **90% of independent runs**, across the full Placebo Suite.

-   **For QST (Distributional):** The estimator must not systematically "hallucinate" structure where none exists. A single run is considered a **Null Rejection** if the zero line ($y=0$) is excluded from the pointwise 95% confidence interval for **more than 10% of the grid points** (i.e., $>9$ out of 99 $\tau$ values).

    The rate of **Null Rejection runs** must not exceed **10%** across the Placebo Suite.

*(Rationale: This "10/10 Rule" accounts for the multiple-testing inherent in checking 99 quantiles with pointwise intervals, preventing false failures while strictly penalizing estimators that detect broad "tilts" or "shifts" in placebo data.)*

#### 4.2.3 Requirements

-   The Gatekeeper test **applies only to estimators that produce confidence intervals** (via bootstrap or a declared alternative).

-   Estimators **MUST** declare their confidence-interval methodology in `meta$ci_type`.

-   Estimators without confidence intervals are labeled **“Unverified”** rather than failing.

#### 4.2.4 Enforcement

-   Failing an individual placebo DGP does *not* constitute failure.

-   Systematic deviation—defined as violating the thresholds in 4.2.2—is a **constitutional failure**.

-   Estimators failing the Gatekeeper MUST be marked as **Non-Robust** in the registry.

#### 4.2.5 Scope

These rules apply **exclusively** to ATT and QST estimands defined in Article I.\
They do not constrain heterogeneous-effect estimators or real-data estimators whose target estimands are not placebo-evaluatable.

------------------------------------------------------------------------

## Article V: Computational Safety

### Section 5.1: The "Wide & Shallow" Concurrency Rule

-   **Runner Responsibility:** The Runner manages parallelism at the process level (Wide).

-   **Estimator Responsibility:**

    -   If `config$num_threads == 1`, the Estimator **MUST** restrict internal parallelism to 1 thread.
    -   If `config$num_threads > 1`, the Estimator **MAY** use up to that many threads.

-   This article specifies how parallel execution MUST behave when implemented. v0.1.x is **serial by default**; parallel execution is permitted only in **experimental mode** under strict protocol (see Article VI).

### Section 5.2: The Granularity Rule

To prevent data loss:

-   Results **must** be persisted (pinned) at the granularity of a single run (DGP × Estimator × Seed).

-   Aggregation into suites happens only *after* secure storage of atomic results.

------------------------------------------------------------------------

## Article VI: The Law of Atomic Persistence

To ensure the integrity of the benchmark registry during massively parallel execution:

-   **Atomicity:** Every simulation result (DGP × Estimator × Seed) must be persisted to a unique storage location or identifiable partition. Workers must never overwrite, append to, or modify existing result pins.

-   **Isolation:** Parallel workers are **strictly prohibited** from modifying shared board state (e.g., updating manifests, indices, or registries).

-   **Serialization:** Any operation that modifies the shared registry index (e.g., `write_board_manifest`) **must** be executed serially by the controller process only after all workers have terminated.

------------------------------------------------------------------------

## Article VII: Immutable DGP Versioning & Scientific Governance

### Section 7.1: Purpose

Data-Generating Processes (DGPs) are scientific claims expressed as executable code.\
Their evolution must be governed by principles of:

-   immutability\
-   reproducibility\
-   transparent correction\
-   versioned provenance

This Article defines the mandatory rules that regulate DGP lifecycles, versioning, and usage.

### Section 7.2: Immutability and Versioning

No mutation after release

-   **No mutation after release** Once a DGP version is published, its implementation is **frozen forever**. It MUST NOT be modified, corrected, simplified, refactored, or otherwise altered, **except for changes permitted by Section 7.2.d** (Performance-only Refactors).

-   **New versions only** Any scientific correction, specification shift, bug fix, or conceptual improvement MUST result in a **new version**.

-   **Version identity** A version is uniquely identified by its `(dgp_id, version)` pair. Both fields MUST be permanent, immutable, and globally unique.

-   **Performance-only Refactors** A code change MAY keep the same version **only if** a regression corpus demonstrates **bitwise-identical outputs**. The regression corpus MUST cover **both the generated data and the derived truth/oracle outputs**. If full bitwise verification across seeds or parameters is not feasible, the default requirement is to **bump the version**.

### Section 7.3: File Organization and Implementation Structure

-   **One file per conceptual DGP**\
    All versions of a DGP MUST reside in a single file, typically named:

`dgp-<dgp_id>.`

This file MUST contain:

-   the **immutable versioned implementations** (one function per version)

-   optional **internal core helpers**

-   no mutation of previously published functions

-   **Optional parameterized core**\
    Internal helpers MAY factor out shared logic to reduce duplication, provided versioned functions remain immutable and fully reproducible.

### Section 7.4: Scientific Status Codes

Each DGP version MUST carry one of the following statuses:

-   **stable** — validated, correct, and recommended\

-   **experimental** — under evaluation; not yet fully vetted\

-   **deprecated** — retained for backwards compatibility but superseded\

-   **invalidated** — known to be flawed; preserved only for reproducing past results

-   **Status MUST be explicit**\
    Each version MUST declare a status in the registry.

-   **Stability invariant**\
    For each `dgp_id`, there MUST be **0 or 1** stable versions.\
    More than one stable version per DGP is forbidden.

-   **Default fallback behavior**\
    When a user requests a DGP without specifying a version:

1.  If a **stable** version exists → use it.\
2.  If no stable version exists →
    -   use the highest-versioned `experimental` entry, and\
    -   emit a warning.\
3.  If neither exists → error.

Semantic versioning MUST use a deterministic parser; malformed semver MUST error.

-   **Warning protocol**\
    If the selected version is `deprecated` or `invalidated`, the system MUST emit a **loud warning**, including:

-   dgp_id\

-   version\

-   status\

-   rationale (mandatory for deprecated/invalidated)\

-   date of status change (if present)

Warnings may be silenced **only if the caller explicitly sets `quiet = TRUE` on the public API**.\
Internal runners MUST NOT silence warnings.

### Section 7.5: Registry Governance

A DGP registry MUST be maintained, containing one row per `(dgp_id, version)`.\
It MUST satisfy the following invariants:

-   **Registry invariants**

For every row: 1. `dgp_id` MUST be a valid identifier.\
2. `version` MUST follow semantic versioning.\
3. `status ∈ {stable, experimental, deprecated, invalidated}`.\
4. Exactly one row per `(dgp_id, version)`.\
5. At most one stable version per `dgp_id`.\
6. If a lookup requests a `dgp_id` absent from the registry → error.\
7. Every versioned row MUST include:\
- a callable generator\
- a human-readable `description`\
- a `rationale` for deprecated/invalidated entries\
- optional provenance metadata

Malformed semver MUST be rejected at validation time.

-   **Mixed-version warning**\
    If stable versions of a `dgp_id` span multiple design-spec lines, a warning MUST be issued.\
    Mixed spec lines among **non-stable** versions are informational only.

-   **Deterministic resolution**\
    When `version = NULL`, resolution MUST follow:

1.  Filter by `status` (stable → experimental → error).\
2.  If multiple candidates remain:
    -   choose the highest semantic version.\
3.  If semantic versions tie or cannot be parsed:
    -   error.

No implicit “latest” behavior is permitted outside these rules.

-   **Registry is the source of truth**\
    All DGP selection and execution MUST occur via the registry.\
    Direct calls to versioned DGP functions by external code are discouraged but not forbidden; however, runners MUST always resolve via the registry.

### Section 7.6: Provenance and Backwards Reproducibility

-   **Immutable historical binding**\
    Every experiment MUST record `(dgp_id, version)` permanently and unambiguously.

-   **Replaying past runs**\
    Users MUST be able to reproduce any historical run exactly by invoking the recorded `(dgp_id, version)`, regardless of later changes.

-   **Preservation of invalidated versions**\
    Invalidated versions MUST NOT be deleted.\
    They MUST remain executable for auditability and replication of historical results.

### Section 7.7: Interaction With Other Articles

This Article complements:

-   **Article I – Truth**\
    by preventing silent drift in the structural data-generating equations.

-   **Article II – Frozen Logic**\
    by providing structured immutability rather than informal expectations.

-   **Article VI – Provenance**\
    by ensuring that results can always be traced back to the exact code that generated them.

Together, these Articles enforce the scientific guarantees required of CausalStress.

### Section 7.8: Enforcement

Violations of this Article MUST trigger:

-   hard registry validation errors (VII.5.a)
-   resolution-time errors (VII.5.c)
-   or loud warnings (VII.4.d)

Compliance MUST be enforced continuously by automated tests.
