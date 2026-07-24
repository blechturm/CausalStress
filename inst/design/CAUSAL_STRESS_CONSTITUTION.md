# CAUSALSTRESS CONSTITUTION

**Version:** 2.0.1\
**Public name:** CausalStress Scientific Protocol\
**Date:** 2026-07-24\
**Status:** Ratified (DGP Contract Terminology Patch). Ratified by the maintainer on 2026-07-24 after the accepted DGP contract terminology RFC and independent synthesis review.

------------------------------------------------------------------------

## Preamble

`CausalStress` is a scientific instrument, not merely a software library.\
This Constitution codifies the *normative, versioned rules* that ensure benchmarks remain comparable across time, machines, and estimator implementations.

The Constitution is **stable but not frozen**:\
patch-level revisions MAY clarify intent, tighten definitions, or correct ambiguities,\
but MUST NOT change the semantic meaning of any article without a *major* version bump.

All contributors must treat this document as the supreme authority.\
Any code proposal that violates it must be rejected.

### Amendment History

-   **v2.0.1 (Ratified 2026-07-24):** Patch clarification of the DGP contract. Corrects Article III §3.2.A's synthetic-covariate notation from lowercase `x1...xk` to uppercase, consecutive, one-based `X1...Xk`, matching the immutable outputs of all 24 released package-managed synthetic DGP versions. The correction preserves the intended scientific meaning and changes no released DGP implementation, generated data, truth, RNG stream, fingerprint, estimator result, or campaign evidence. It also normalizes the human-facing term `Real DGP` to `real-data DGP` in the three live contract sites; the machine discriminator `type = "real"` is unchanged and real-data support remains deferred. Historical amendment and RFC records are preserved verbatim. The bump is *patch*.

-   **v2.0.0 (Ratified 2026-06-16):** Introduces the typed **estimand registry** (ATT, ATE, QST, CATE) and amends Articles **I, II, III, IV, V, and VI** for internal consistency — the estimand registry and typed scoring (Art. I), held-out evaluation-sample identity (Art. II §2.2), the typed estimator output contract (Art. III §3.1), the per-estimand gatekeeper (Art. IV), and fit-artifact/score-record persistence granularity (Art. V §5.2, Art. VI) — per accepted RFC-1 (`inst/design/rfc/20260616_estimand_registry_synthesis.md`). Existing ATT/QST truth, the real-DGP external-truth clause (§1.3), QST oracle-size immutability (§1.4), and the existing ATT/QST gatekeeper enforcement (§4.2.4) are **preserved**. The bump is *major*. Implementation is staged (Wave 1: ATT/ATE typed scoring; Wave 2: CATE). **Constitutional-review corrections (2026-06-16):** the RATIFY-WITH-AMENDMENTS findings of `rfc/20260616_constitution_2_0_0_review.md` were applied (B1 persistence grain; B2 staged-implementation rule; B3 held-out eval identity; B4 oracle-immutability scope; M1 target-level enumeration; M2 ATE scoring population; M3 stale CI/gatekeeper clause; m1 tau-grid wording; m2 stale release-line wording). Ratified by the maintainer on 2026-06-16.

------------------------------------------------------------------------

## Article I: The Definition of Truth

To prevent ambiguity between "Signal" and "Noise," all Synthetic DGPs must adhere to the **Two-Tier Truth Contract**: truth is defined across two **tiers** — Structural (noise-free) and Distributional (full realized) — over a registry of **estimands** at three governed **target levels** — population-scalar, distributional-curve, and unit-level. The estimands governed in v2.x are ATT, ATE, QST, and CATE (Section 1.7).

### Section 1.1: Interpretation

Where ambiguity exists, **the strictest interpretation prevails**, favoring reproducibility, transparency, and estimator safety.

### Section 1.2: Authority to Define DGPs

Only the core maintainers may define new DGP IDs.\
Community contributions MUST pass constitutional validation and MUST be versioned according to Article II.

### Section 1.3: Structural ATT

The Average Treatment Effect on the Treated (ATT) is defined strictly on the **noise-free structural component**. $$ATT_{true} = \frac{1}{N_{treated}} \sum_{i: W_i=1} \mathbb{E}[Y_1 - Y_0 \mid X_i]$$

-   **Constraint:** For Synthetic DGPs, the structural treatment effect $\tau(X)$ MUST be a deterministic measurable function of covariates $X$ only. It MUST NOT depend on treatment assignment $W$, propensity $p(X)$, or realized sample noise.

-   **Prohibition:** Truth must never be calculated as the sample mean of realized differences ($y_1 - y_0$) in heavy-tailed settings.

-   **Real Data:** For real-data DGPs, Truth must be defined externally (e.g., Experimental Benchmark) and never regenerated. **This truth is considered the Stabilized Experimental Estimate and is permitted to include inherent sampling noise.**

### Section 1.4: Distributional QST

The Quantile Shift (QST) is defined on the **full realized distribution** (Signal + Noise). $$QST(u) = Q_u(Y_1 \mid W=1) - Q_u(Y_0 \mid W=1)$$

-   **Grid:** The canonical truth grid is invariant: $u \in \{0.01, 0.02, \dots, 0.99\}$.

-   **Computation:** For synthetic data, this **must** be computed via Oracle Monte Carlo ($N=10^6$) or analytic derivation matching oracle precision ($< 10^{-5}$). The oracle size $N=10^6$ is immutable within a major constitutional line and MUST NOT change except by an explicit major constitutional amendment that names the change; the v1.x QST oracle size is carried into v2.x unchanged.

-   **Independence:** Noise MUST be drawn independently across units unless explicitly specified.

### Section 1.5: Structural ATE

The Average Treatment Effect (ATE) is defined on the **noise-free structural component** over **all** units — the population analogue of §1.3 without conditioning on treatment: $$ATE_{true} = \frac{1}{N} \sum_{i} \tau(X_i)$$

-   **Default convention:** finite-sample structural ATE over the **declared scoring population** of the ATE target — by default the full generated run sample — matching the finite-sample convention of Structural ATT (§1.3). A superpopulation ATE MAY be used only if a DGP explicitly declares analytic/oracle support, and the truth descriptor MUST distinguish it from the finite-sample ATE.

-   **Constraint & Prohibition:** identical to §1.3 — $\tau(X)$ is a deterministic function of $X$ only; truth is never the sample mean of realized $y_1 - y_0$.

### Section 1.6: Structural CATE

The Conditional Average Treatment Effect (CATE) is the **unit-level** structural effect $\tau(X_i) = \mathbb{E}[Y_1 - Y_0 \mid X_i]$ — the **conditional-mean** effect, **not** the realized individual effect $Y_1 - Y_0$.

-   **Truth:** the per-unit structural effect vector (`meta$structural_te`); no new oracle is required.

-   **Evaluation:** CATE is scored on a **held-out** evaluation sample by default (see Article III §3.1); the realized individual effect is never the estimand.

### Section 1.7: The Estimand Registry and Typed Scoring

Estimands are a governed, versioned vocabulary. Each is identified by a typed `estimand_target` (id, truth tier, target level, target population, evaluation policy, grid/metric identifiers).

| Estimand | Tier | Level | Population | Truth |
|---|---|---|---|---|
| ATT | Structural | population | treated | §1.3 |
| ATE | Structural | population | all | §1.5 |
| CATE | Structural | unit | held-out eval | §1.6 |
| QST | Distributional | distributional | treated | §1.4 |

-   **Typed scoring (no cross-scoring):** an estimator's output for a target MUST be scored only against that target's truth. Scoring is the intersection `requested ∩ estimator-produced ∩ DGP-truth-available`; any unscoreable request is recorded as an explicit **non-comparable** result with a machine-readable reason, never silently cross-scored.

-   **Real-data DGPs:** estimands lacking externally-supplied truth are **non-comparable** (no truth is regenerated). This extends the §1.3 real-data principle; it does **not** create a new "external truth tier." A generalized external-truth tier for ATE/CATE is deferred to a future real-data RFC.

-   **Staged implementation:** a registered estimand whose typed scoring is not yet implemented in the active release wave is recorded as **non-comparable** with the machine-readable reason `target_not_implemented`. This is an explicit interim status, never a silent omission, and never permits cross-scoring against another target's truth. (v2.0.0 stages CATE to Wave 2; until then a CATE request resolves to `target_not_implemented`.)

------------------------------------------------------------------------

## Article II: Immutability and Reproducibility

Scientific benchmarks are worthless if the ground moves under our feet. To ensure that "Truth" remains stable across time, machines, and R versions, the computational substrate must be frozen and declared.

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

-   **Same-Substrate Bitwise Identity:** Two runs with the same `DGP ID`, `Version`, and `Seed` must produce **bitwise identical** dataframes and truth tables when executed on the same declared computational substrate: R version, operating system/platform, RNG kind, BLAS/LAPACK/libm-relevant numeric substrate, and thread-cap environment.

-   **Cross-Substrate Reproducibility:** Across different operating systems, R versions, BLAS/LAPACK implementations, or platform math libraries, CausalStress claims documented tolerance-level numerical reproducibility unless a version-specific regression corpus proves bitwise identity. Release evidence MUST record the computational substrate used for reproducibility validation.

-   **Held-out Evaluation Samples:** When an estimand is scored on a Runner-generated held-out sample (e.g., CATE per §1.6), that sample is a **second governed random draw** and is itself truth-bearing. The Runner **must** capture and store, alongside the training seed: the held-out evaluation seed, its sample size, and its derivation relative to the training sample. The Runner **must** record the `unit_id` keying that binds held-out structural truth to estimator predictions, and that evaluation-sample identity is part of the score-record identity (Article V §5.2). The same-substrate bitwise-identity and cross-substrate tolerance guarantees above apply to the held-out sample and its truth exactly as to the training sample. Transductive estimators that require the evaluation covariates at fit time are the sole exception to train/eval separation; they MUST declare this, and their evaluation-sample identity is recorded in both the fit-artifact identity and the score-record identity.

------------------------------------------------------------------------

## Article III: The Interoperability Contracts

### Section 3.1: The Estimator Contract

Every estimator function must conform to: `function(df, tau, config) -> list(outputs, meta)`, where `outputs` is a named collection of **typed estimand outputs** declared in the estimator capability registry, keyed by `estimand_target_id`.

-   **Output shapes:** scalar (point estimate + optional CI) for ATT/ATE; curve (keyed to the Runner-provided QST tau grid) for QST; **unit-keyed** (a table keyed by the Runner-issued `unit_id`, with `estimate` and optional uncertainty) for CATE.

-   **Legacy compatibility:** the legacy `list(att, qst, meta)` shape remains supported as a compatibility shim; the Runner normalizes it to typed `outputs` before scoring.

-   **Covariate Access:** The Runner MUST physically sanitize `y0`, `y1`, `p`, and `structural_te` from the input dataframe before execution. Estimators will not receive these columns unless explicitly configured as Oracle. Oracle access MUST be column-scoped.
    -   *Exception:* Estimators explicitly configured for true-propensity oracle access (e.g., `config$use_true_propensity = TRUE`) MAY access `p`.
    -   *Exception:* Structural benchmark estimators explicitly configured for structural-treatment-effect access (e.g., `config$use_structural_te = TRUE`, or an internal descriptor default for benchmark-only estimators) MAY access `structural_te`.
    -   No ordinary runner airlock grant MAY expose `y0` or `y1` in v2.x.
    -   **CATE held-out predict input:** for CATE scoring, the held-out evaluation data passed to the estimator's prediction step MUST be **covariates plus the Runner-issued `unit_id` only** — excluding `y`, `w`, `y0`, `y1`, `p`, and `structural_te`. The `unit_id` is a synthetic row key, not truth. Held-out structural truth is retained by the Runner on a scorer-only channel and never reaches the estimator.
-   **Tau Compliance:** Estimators **MUST** calculate QST only for the `tau` values provided by the Runner. The canonical grid applies to truth tables, not estimator inputs.
-   **Confidence Intervals:**
    -   Gatekeeper testing applies through the **per-estimand components defined in Article IV**; CI-based gate checks apply only where an estimator reports CIs (or declares a valid alternative methodology).
    -   If an estimator reports CIs, they **MUST** be Bootstrap-based (default) unless the estimator explicitly declares an alternative valid methodology (e.g., Asymptotic, Bayesian) in `meta$ci_type`.

### Section 3.2: The DGP Contract (Bifurcated)

**A. Synthetic DGPs (`type="synthetic"`)** MUST return:

-   `df`: Tibble including `y`, `w`, `p`, `y0`, `y1`, and one or more covariates named `X1`, ..., `Xk`, using uppercase `X` with consecutive one-based integer suffixes.

-   `true_att`: Numeric scalar.

-   `true_qst`: Tibble with columns `tau` (numeric) and `value` (numeric).

-   **Strict Alignment:** This table **MUST** be evaluated exactly at the canonical grid $\{0.01, \dots, 0.99\}$. Any deviation in grid points renders the DGP unconstitutional.

-   `meta$structural_te`: Numeric vector matching `nrow(df)`.

**B. Real-data DGPs (`type="real"`)** MUST return:

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

The Gatekeeper is composed of **per-estimand components** attached to the estimands defined in Article I. The existing **ATT** and **QST** components (§4.2.2) and their enforcement (§4.2.4) are **unchanged** by this amendment.

-   **ATE:** a scalar placebo-gate component analogous to ATT. Its thresholds, difficulty tiers, and registry consequences are deferred to the Gatekeeper-recalibration RFC.

-   **CATE:** placebo evaluation is **NOT** a per-unit "$\hat\tau(X_i) \approx 0$" test (ill-posed for a unit-level estimand). Under the Sharp Null, a CATE estimator is **"Unverified"** for hallucination unless a principled heterogeneity-**detection** test (e.g. BLP/GATES, grouped-homogeneity, or RATE) is implemented; CATE accuracy is scored on non-null DGPs (PEHE plus a robust companion metric).

-   **Deferred policy:** pass/fail calibration, difficulty tiers, and the `Non-Robust` registry label for the new (ATE/CATE) components — and any recalibration of the existing ATT/QST components — are governed by the future **Gatekeeper recalibration RFC** (`horizon.md`), not by this amendment.

These rules do not constrain estimators whose target estimands are not placebo-evaluatable.

------------------------------------------------------------------------

## Article V: Computational Safety

### Section 5.1: The "Wide & Shallow" Concurrency Rule

-   **Runner Responsibility:** The Runner manages parallelism at the process level (Wide).

-   **Estimator Responsibility:**

    -   If `config$num_threads == 1`, the Estimator **MUST** restrict internal parallelism to 1 thread.
    -   If `config$num_threads > 1`, the Estimator **MAY** use up to that many threads.

-   This article specifies how parallel execution MUST behave when implemented. Release lines are **serial by default** unless the active release specification authorizes parallel execution under the atomic-persistence protocol of Article VI.

### Section 5.2: The Granularity Rule

To prevent data loss, results are persisted at two atomic grains:

-   A **fit artifact** **must** be persisted at the granularity of a single model fit (DGP × Estimator × Seed × fit configuration). For declared transductive estimators, the fit configuration includes the held-out evaluation-sample identity.

-   A **score record** **must** be persisted at the granularity of one fit × one scored `estimand_target` × one metric, including any held-out scoring-population identity (Article II §2.2). A single fit MAY yield multiple score records.

-   Aggregation into suites happens only *after* secure storage of these atomic results.

------------------------------------------------------------------------

## Article VI: The Law of Atomic Persistence

To ensure the integrity of the benchmark registry during massively parallel execution:

-   **Atomicity:** Every persisted artifact — each **fit artifact** and each **score record** at the grains defined in Article V §5.2 — must be persisted to a unique storage location or identifiable partition. Workers must never overwrite, append to, or modify existing result pins.

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

-   **Performance-only Refactors** A code change MAY keep the same version **only if** a regression corpus demonstrates **bitwise-identical outputs** under the same-substrate scope defined in Article II Section 2.2. The regression corpus MUST cover **both the generated data and the derived truth/oracle outputs**. If full bitwise verification across seeds or parameters is not feasible, the default requirement is to **bump the version**.

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
