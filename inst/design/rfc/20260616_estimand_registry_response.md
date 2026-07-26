# RFC Response: Estimand Registry Seed

**Status:** Response stage per `inst/design/rfc_cycle.md`
**Date:** 2026-06-16
**Reviewer stance:** Hostile but fair. The seed is directionally necessary, but it overclaims how much is already present and underspecifies the scientific and schema contracts that would make the amendment safe.

## Findings

1. **BLOCKER: CATE scoring on the training rows is not a defensible benchmark unless the RFC defines an evaluation protocol.**

   - **Claim attacked:** CATE can be added by exposing per-unit `structural_te` truth and computing PEHE (`inst/design/rfc/20260616_estimand_registry_seed.md:50-72`).
   - **Evidence:** The runner currently generates one DGP realization and passes that same data frame to the estimator (`R/cs-runner.R:89-107`), then scores scalar ATT and QST from the same run object (`R/cs-runner.R:243-274`). Existing DGP contracts and examples expose row contents, not a train/test split or held-out evaluation sample (`inst/design/CAUSAL_STRESS_CONSTITUTION.md:113-124`; e.g. `R/dgp-synth-baseline.R:23-26`).
   - **Why this is wrong/risky:** PEHE is usually interpreted as out-of-sample heterogeneous-effect accuracy or at least accuracy on a clearly defined evaluation population. If CausalStress fits and scores CATE on the same rows, adaptive learners can overfit heterogeneity and look good without estimating a transportable conditional effect. This is not a cosmetic concern; it changes the scientific claim from "CATE benchmark" to "in-sample treatment-effect surface reconstruction."
   - **Counter-proposal:** RFC-1 must define `cate_eval_policy` before acceptance: either (a) a held-out scoring sample generated from the same DGP and same structural law, (b) cross-fitting/sample-splitting for CATE scoring, or (c) an explicit in-sample metric that is not marketed as PEHE benchmark evidence. Store the policy in metadata and fingerprints.

2. **BLOCKER: Per-unit CATE output has no alignment contract. Position-based scoring will silently corrupt PEHE.**

   - **Claim attacked:** "CATE truth is therefore the per-unit vector itself" and raw predictions can be optional (`seed.md:50-72`, `seed.md:100`).
   - **Evidence:** Current synthetic DGPs return ordinary data frames with truth columns, but no stable `unit_id` contract (`inst/design/CAUSAL_STRESS_CONSTITUTION.md:113-124`; `R/dgp-synth-baseline.R:23-26`). The current row output schema has scalar ATT columns and one QST list-column, with no place for unit-level joins or prediction hashes (`R/cs-result-to-row.R:16-63`).
   - **Why this is wrong/risky:** If an estimator sorts rows, drops rows, performs internal complete-case handling, or returns predictions for a new matrix, a numeric vector of length `n` is not enough. Joining by position is a silent wrong-science failure mode exactly like C4, just at unit level.
   - **Counter-proposal:** Require every CATE-capable run to carry a runner-created immutable `unit_id` that is visible to the estimator and to the scoring truth table. CATE output must be a tibble keyed by `unit_id` with `estimate`; the runner joins to `structural_te` by key and hard-errors on missing, duplicate, or extra ids. Summary-only storage is acceptable only after keyed scoring has occurred; store a digest of the keyed prediction table even when raw predictions are not retained.

3. **BLOCKER: The CATE placebo gate is not a CATE validity test and is overstated as "more discriminating."**

   - **Claim attacked:** On placebo, `tau_hat(X_i) ~= 0` is a CATE gate and the placebo suite is "more discriminating for CATE than for ATT" (`seed.md:74-76`, `seed.md:103`).
   - **Evidence:** Article IV currently tests ATT CI coverage and QST null-rejection rates only (`inst/design/CAUSAL_STRESS_CONSTITUTION.md:159-178`). The implemented gatekeeper hardcodes scalar `att_covered` summaries and QST 10/10 logic (`R/cs-gatekeeper.R:35-65`, `R/cs-gatekeeper.R:93-145`). It has no non-null heterogeneity criterion.
   - **Why this is wrong/risky:** A constant-zero CATE estimator passes the proposed placebo gate perfectly while being useless on every non-null heterogeneous DGP. A biased-but-flat estimator can also pass. The gate detects hallucinated heterogeneity under a sharp null; it does not verify CATE recovery and cannot be sold as "more discriminating" without a non-null calibration suite.
   - **Counter-proposal:** Name it `cate_placebo_hallucination_gate`, not "CATE gatekeeper" generally. Pair it with a non-null CATE recovery gate over DGPs with known heterogeneous `tau(X)`, using PEHE or a robust alternative. Estimators with no per-unit uncertainty should be "Unverified" for interval-based CATE claims, consistent with Article IV Section 4.2.3 (`inst/design/CAUSAL_STRESS_CONSTITUTION.md:171-177`).

4. **BLOCKER: PEHE under heavy tails cannot be deferred as a loose RFC-3 hook.**

   - **Claim attacked:** PEHE is primary, with heavy-tail caveat deferred to Q5/RFC-3 (`seed.md:70-72`, `seed.md:91`, `seed.md:99`).
   - **Evidence:** The seed's own acceptance criteria require the synthesis to state the CATE metric position, including heavy-tail validity (`seed.md:112-114`). The research note warns that heavy-tail robustness claims must be framed carefully and that not all quantile/tail targets are robust (`inst/design/research/prior_art_killplot_heavytail_benchmarking.md:21-22`, `:82-84`).
   - **Why this is wrong/risky:** PEHE squares errors. In the regimes CausalStress wants to foreground, squared-error metrics can be dominated by rare unstable fits and may have infinite or practically unusable variance. If RFC-1 says "PEHE is the metric" and RFC-3 later says "PEHE is invalid here," the program has a circular dependency.
   - **Counter-proposal:** RFC-1 must define the metric registry now: `pehe_rmse` plus at least one robust CATE metric such as median absolute CATE error, trimmed RMSE, or quantile loss over `tau_hat(X)-tau(X)`. RFC-3 can attach validity envelopes by DGP regime, but the metric vocabulary and invalid/unverified semantics belong here.

5. **BLOCKER: The constitutional version-bump answer cannot be "minor/additive" by assertion.**

   - **Claim attacked:** Article I/IV amendment is additive and may be a minor bump (`seed.md:82-86`, `seed.md:102`).
   - **Evidence:** The Constitution says patch-level revisions may clarify but must not change the semantic meaning of any article without a major version bump (`inst/design/CAUSAL_STRESS_CONSTITUTION.md:14-16`). Article IV currently says gatekeeper rules apply "exclusively" to ATT and QST (`inst/design/CAUSAL_STRESS_CONSTITUTION.md:187-190`).
   - **Why this is wrong/risky:** Replacing "exclusively ATT and QST" with "estimands defined in Article I" changes the normative scope of Article IV. That is not a clarification. The seed may be right that old ATT/QST meanings remain intact, but it is still changing what the Constitution governs.
   - **Counter-proposal:** Either bump the Constitution to 2.0.0, or split the amendment so existing Article IV text is preserved verbatim and new CATE/ATE rules live in a clearly additive Article/section with explicit "does not alter ATT/QST enforcement" language. The synthesis must make the version decision, not leave it as ambiguity.

6. **MAJOR: "GRF/BART already compute CATE and merely collapse it" is factually overstated for the current adapters.**

   - **Claim attacked:** Existing estimators already compute CATE internally and exposing CATE "largely un-collapses what exists" (`seed.md:64-66`).
   - **Evidence:** The GRF adapter fits `grf::causal_forest()` and immediately calls `grf::average_treatment_effect(..., target.sample = "treated")`; the returned result contains `att`, `qst = NULL`, `cf = NULL`, and `capabilities = c("att")` (`R/est-grf-dr.R:128-150`, `R/est-grf-dr.R:200-217`). The BART adapter calls `bartCause::bartc(..., estimand = "att")` and extracts the ATT row from `summary(fit)$estimates`; it returns only `att` plus metadata (`R/est-bart.R:92-103`, `R/est-bart.R:157-180`).
   - **Why this is wrong/risky:** GRF and BART may have model objects from which heterogeneous predictions could be produced, but CausalStress currently discards them and does not define a prediction API, unit alignment, uncertainty shape, or scoring table. "Un-collapse" understates real implementation and validation work.
   - **Counter-proposal:** Treat GRF/BART CATE as new adapter work. Require each CATE adapter to document prediction source, target sample, row-key alignment, uncertainty availability, and whether predictions are in-sample or held-out.

7. **MAJOR: The seed understates current registry state by saying capability is "just supports_qst."**

   - **Claim attacked:** Registry capability is a single `supports_qst` boolean plus `oracle` flag (`seed.md:22-23`, `seed.md:64-66`).
   - **Evidence:** The registry also carries `oracle_columns`, `oracle_default_columns`, `version`, `source`, and `requires_pkgs` (`R/cs-estimator-registry.R:23-79`). Runtime registration validates column-scoped oracle grants and rejects unsupported truth columns (`R/cs-estimator-registry.R:127-236`).
   - **Why this is wrong/risky:** The design must not flatten oracle access, package availability, CI capability, and estimand capability into one "produces" vector. Those are separate axes with separate safety consequences.
   - **Counter-proposal:** Use a capability table/list with at least: `estimand_id`, `output_level`, `target_population`, `metric_support`, `ci_support`, `oracle_columns_eligible`, `requires_pkgs`, `eligibility_fn`, and `experimental_status`. Keep oracle-column governance separate from estimand production.

8. **MAJOR: `estimand_target = {id, target_level, conditioning}` is too small and will encode false equivalences.**

   - **Claim attacked:** The structured triple in Q2 is the proposed typed identifier (`seed.md:54-62`, `seed.md:96`).
   - **Evidence:** Existing QST truth depends on a canonical tau grid and oracle algorithm (`inst/design/CAUSAL_STRESS_CONSTITUTION.md:46-53`; `R/cs-oracle-truth.R:56-79`). ATT depends on realized treated units (`R/cs-truth.R:9-10`). CATE needs an evaluation population and possibly row-level prediction policy, none of which fit in `{id,target_level,conditioning}`.
   - **Why this is wrong/risky:** `conditioning = "treated"|"all"` cannot represent QST's tau grid, CATE evaluation sample, target sample vs training sample, superpopulation vs finite-sample truth, or per-estimand metric validity. A too-small type will look governed while still allowing C4-class mis-scoring.
   - **Counter-proposal:** Define `estimand_target` as a normalized descriptor with `id`, `truth_tier`, `effect_scale`, `target_population`, `evaluation_population`, `grid_id`, `metric_id`, and `version`. Store a compact canonical string for fingerprints, but keep the semantic object explicit.

9. **MAJOR: Multi-estimand runs need separate fit identity and score identity.**

   - **Claim attacked:** Q3 frames multi-estimand runs as an efficiency/storage choice (`seed.md:97`).
   - **Evidence:** Current fingerprints identify one scientific result from `(dgp_id, dgp_version, estimator_id, n, seed, tau_id, ci_intent, config)` (`R/cs-fingerprint.R:95-123`, `R/cs-fingerprint.R:164-181`). Result rows assume one ATT scalar and one QST table (`R/cs-result-to-row.R:16-63`).
   - **Why this is wrong/risky:** If one GRF fit emits ATT, ATE, and CATE, there is one expensive fit but multiple scored scientific claims. If the fingerprint is per target, resume may refit unnecessarily. If the fingerprint is per fit, downstream result rows can collide or be misinterpreted.
   - **Counter-proposal:** Introduce two identities: `fit_fingerprint` for data/model/config and `score_fingerprint` for `(fit_fingerprint, estimand_target, metric_id, truth_version)`. Store one run artifact with multiple score records, or one row per score sharing a `fit_fingerprint`.

10. **MAJOR: The schema-4 plan ignores the current schema/resume blast radius.**

    - **Claim attacked:** Add `estimand_target`, bump schema 3 to 4, preserve deterministic legacy comparison (`seed.md:78-80`, `seed.md:101`).
    - **Evidence:** Resume code has explicit branches for schema missing/1, 2, and 3, then hard-errors on unsupported schemas (`R/cs-runner.R:642-660`; `R/cs-run-campaign.R:229-287`). Batch consolidation independently validates batch artifact schema and required fingerprint metadata (`R/cs-consolidate.R:36-130`). The v0.1.10 closeout records that schema-2 resume without DGP version had to hard-abort because equivalence could not be proven (`inst/design/causalstress_v0_1_10_spec_packet/release_closeout.md:24-31`).
    - **Why this is wrong/risky:** A schema-4 bump is not a field addition. It touches resume, pin naming, task fingerprints, batch artifacts, consolidation, audit, collectors, and migration notes. The seed repeats the exact under-specification pattern that made schema-2 migration dangerous.
    - **Counter-proposal:** The synthesis must include a schema-4 migration matrix: old artifact schema -> behavior, absent target defaults, exact conditions under which old pins can be reused, and hard-abort cases. Require focused tests for grid resume, plan resume, batch consolidation, `force=TRUE`, and mixed schema boards.

11. **MAJOR: The blast-radius table misses several load-bearing surfaces.**

    - **Claim attacked:** Downstream consumers are summarized as collectors/plots/runners/gatekeeper/families (`seed.md:88-91`).
    - **Evidence:** ATT/QST assumptions are also embedded in `cs_science_payload()` (`R/cs-accessors.R:10-55`), `cs_meta_flatten()` (`R/cs-accessors.R:87-120`), `cs_audit()` provenance summaries (`R/cs-provenance.R:11-57`), `cs_summarise_runs()` (`R/cs-summary.R:33-66`), QST summaries (`R/cs-summary-qst.R:11-23`), plot helpers (`R/cs-plots.R:11-104`), batch/campaign dynamic registry reconstruction (`R/cs-campaign-runner.R:78-119`), and consolidation QST checks (`R/cs-consolidate.R:118-130`).
    - **Why this is wrong/risky:** A seed that does not name these layers will produce an implementation packet that misses them. The result will be a registry that says "CATE" while audit/science payloads and reports silently drop it.
    - **Counter-proposal:** Add these files to the RFC acceptance criteria and make "round-trip CATE through run -> pin/batch -> tidy -> science payload -> audit/summary/plot" a required contract test.

12. **MAJOR: ATE is "near-free" only after the RFC chooses finite-sample vs superpopulation truth and persists it.**

    - **Claim attacked:** ATE is a near-free second scalar (`seed.md:48`, `seed.md:52`).
    - **Evidence:** Existing truth helper only defines `cs_true_att(structural_te, w)` (`R/cs-truth.R:9-10`). Synthetic DGP contract requires `true_att`, not `true_ate` (`inst/design/CAUSAL_STRESS_CONSTITUTION.md:113-124`). Real DGPs may have `true_att = NA` and `meta$structural_te = NULL` (`inst/design/CAUSAL_STRESS_CONSTITUTION.md:127-133`).
    - **Why this is wrong/risky:** `mean(structural_te)` over generated rows is a finite-sample ATE, not necessarily the superpopulation ATE. That may be the right convention, but it must be stated because ATT is currently finite-sample over realized treated rows. If ATE is not persisted, users cannot audit or resume-scoring old runs safely.
    - **Counter-proposal:** Define `true_ate` as a finite-sample structural mean unless an oracle/analytic superpopulation ATE is explicitly declared. Add `true_ate` to DGP validation, result metadata, collectors, and fingerprints.

13. **MAJOR: Real DGPs and external estimators are not covered by the proposed estimand model.**

    - **Claim attacked:** The four-estimand registry can cover the package's scoring join generally (`seed.md:27-32`, `seed.md:62`).
    - **Evidence:** The Constitution explicitly says real DGPs must not include potential outcomes, must have `true_qst = NULL`, and must have `meta$structural_te = NULL` (`inst/design/CAUSAL_STRESS_CONSTITUTION.md:125-133`). External estimators can be registered at runtime with only a generator, scalar `supports_qst`, and oracle metadata today (`R/cs-estimator-registry.R:97-138`).
    - **Why this is wrong/risky:** Estimand capability is a relation among DGP, estimator, metric, and truth availability, not just an estimator property. A CATE-capable estimator on a real DGP is not scoreable unless external CATE truth exists. An ATE-capable estimator may be non-comparable on a DGP that exposes only ATT truth.
    - **Counter-proposal:** Add a DGP estimand availability registry and make the scoring join three-way: requested targets intersect estimator-produced targets intersect DGP-truth targets. Non-comparable rows must be explicit and tested.

14. **MINOR: The airlock claim is directionally correct but the seed should distinguish estimator input from scorer truth.**

    - **Claim attacked:** The airlock already holds back `structural_te` for scoring.
    - **Evidence:** The airlock drops `y0`, `y1`, `p`, and `structural_te` unless column-scoped oracle grants allow `p` or `structural_te` (`R/cs-airlock.R:49-62`). The runner still retains the unsanitized DGP object and computes truth outside estimator input (`R/cs-runner.R:91-107`, `R/cs-runner.R:243-274`).
    - **Why this matters:** This is the right safety shape, but CATE scoring must be explicit that `structural_te` is used only by the scorer unless the estimator has an oracle grant. Otherwise CATE examples will invite truth leakage.
    - **Counter-proposal:** Specify two channels: `estimator_df` after airlock, and `scoring_truth` retained by runner only. CATE scoring consumes `scoring_truth`, never the estimator input frame.

15. **MINOR: The seed's RFC ordering is only sound if RFC-1 resolves metric validity, not if it punts it.**

    - **Claim attacked:** RFC-1 is the linchpin, RFC-2 freezes the surface, and RFC-3 consumes the validity model (`seed.md:13`, `seed.md:88-91`).
    - **Evidence:** `rfc_cycle.md` says only accepted synthesis is binding and deferred points must be recorded (`inst/design/rfc_cycle.md:21-29`). The horizon already parks estimand expansion as requiring an RFC and Article I/IV amendment (`inst/design/horizon.md:15-46`).
    - **Why this is risky:** If RFC-1 accepts a hand-waved "validity hook," RFC-2 may freeze APIs around an invalid metric and RFC-3 will be forced to work around the wrong abstraction.
    - **Counter-proposal:** RFC-1 must define the validity object shape and at least the CATE metric vocabulary before RFC-2. RFC-3 may populate validity envelopes by family/regime but must not invent the contract after the public API is frozen.

## Dispositions on Q1-Q10

- **Q1 - CATE truth = structural noise-free tau(X): agree-with-conditions.** This is defensible for current synthetic DGPs because Article I already defines structural effects as deterministic functions of X (`Constitution.md:36-42`) and DGPs carry `meta$structural_te`. But the RFC must call it "conditional-mean structural effect," not realized individual effect, and must exclude real DGPs unless external CATE truth exists.

- **Q2 - `estimand_target` shape: disagree.** A flat enum is too weak, but the proposed triple is also too weak. Use a normalized descriptor with target population, evaluation population, truth tier, grid id, metric id, and descriptor version.

- **Q3 - Multi-estimand runs: the seed is asking the wrong question.** The real question is fit identity vs score identity. Permit multi-score artifacts only after defining `fit_fingerprint` and `score_fingerprint`; otherwise use one target per run for v0.2.0.

- **Q4 - Capability matrix: disagree with minimal `produces = c(...)`.** Start with a structured capability table now. A vector will not encode oracle access, CI support, DGP eligibility, metric validity, or experimental status.

- **Q5 - PEHE under heavy tails: disagree with deferral.** PEHE may be included, but robust CATE metrics and validity status must be part of RFC-1. RFC-3 can attach regimes; it cannot rescue an underspecified metric registry.

- **Q6 - Per-unit storage: agree with summary-by-default only if keyed scoring and audit digests are mandatory.** Raw predictions can be opt-in for size, but the runner must score a keyed prediction table and store enough digest/provenance to audit that scoring.

- **Q7 - Fingerprint back-compat: prefer schema-4 bump, but only with a migration matrix.** Treating absent target as ATT is acceptable only for legacy scalar ATT artifacts whose old row shape proves they are ATT. Do not reuse old pins for CATE/ATE scoring without explicit rescoring.

- **Q8 - Constitution version bump: disagree with implied minor default.** Changing Article IV's exclusive ATT/QST scope is a semantic amendment. Default to Constitution 2.0.0 unless synthesis preserves old text and adds a clearly separate, non-weakening extension.

- **Q9 - Gatekeeper for ATE: ATT gate does not suffice.** ATE should have its own placebo scalar gate if an estimator claims ATE CIs. CATE placebo does not subsume scalar gates for estimators that produce only scalar estimands, and a CATE zero gate does not validate non-null CATE accuracy.

- **Q10 - Scope creep: disagree with four-estimand implementation as one batch.** The contract can mention ATT/QST/ATE/CATE, but v0.2.0 implementation should be staged: first typed scalar ATT/ATE scoring plus schema/fingerprint migration, then CATE keyed output/scoring, then CATE metrics/gates. Do not ship CATE on top of scalar-shaped result infrastructure.

## Verdict

**NEEDS-SEED-V2**: must fix the CATE evaluation protocol, keyed unit-level scoring, CATE gatekeeper overclaim, heavy-tail metric validity, constitutional version-bump decision, schema-4 migration plan, and corrected GRF/BART/registry factual claims before synthesis.
