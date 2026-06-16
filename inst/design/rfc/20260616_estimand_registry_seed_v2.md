# RFC Seed v2: Estimand Registry, Typed Scoring, ATE, and CATE

**Status:** Seed v2 after response + response-review; awaiting synthesis per `rfc_cycle.md`
**Date:** 2026-06-16
**RFC:** RFC-1 of the v0.2.0 estimand / UX / families program
**Authors:** Max Thomasberger (maintainer), drafted with assistant
**Prior artifacts:** `20260616_estimand_registry_seed.md`, `20260616_estimand_registry_response.md`, `20260616_estimand_registry_response_review.md`

## Process Note

This is not a synthesis and it is not implementation. It is a revised seed that
incorporates the adversarial response and the response-review adjudication. The
synthesis author must still accept, reject, or amend this proposal.

The response-review is treated as the operative adjudication. Its corrected
severity ranking controls this seed v2: F1 and F2 are required design decisions
but not blocker-grade objections to the whole program; F3 replaces the naive
per-unit CATE placebo gate with a detection-test-or-Unverified structure; F4
requires PEHE plus a robust companion metric; F5, F9, F10, and F13 are
blocker-grade design commitments.

## Verified Baseline Facts

These facts were re-checked against source before authoring this file:

- The current estimator output contract is constitutionally scalar/list-column
  shaped: `function(df, tau, config) -> list(att, qst, meta)` in Article III
  Section 3.1.
- `cs_true_att()` exists, but no `cs_true_ate()` exists (`R/cs-truth.R`).
- `est_grf_dr_att()` fits `grf::causal_forest()`, immediately extracts
  `average_treatment_effect(..., target.sample = "treated")`, returns
  `capabilities = c("att")`, `target_level = "population"`, `qst = NULL`, and
  does not preserve the forest object in the result.
- Current capability representation is fragmented: registry rows carry
  `supports_qst`, `oracle_columns`, and `oracle_default_columns`, while some
  estimator result metadata already carry `capabilities` and `target_level`.
- Resume dispatch in `cs-runner.R`, `cs-run-campaign.R`, and
  `cs-run-one-seed.R` branches over missing/schema-1, schema-2, and schema-3
  fingerprints, then hard-errors on unsupported schemas.
- Current row/tidy/science surfaces are ATT/QST-shaped: `cs_result_to_row()`,
  `cs_collect_att()`, `cs_collect_qst()`, `cs_science_payload()`,
  `cs_meta_flatten()`, `cs_summarise_runs()`, `cs_summarise_qst()`,
  `cs_plot_att_error()`, `cs_plot_placebo()`, and `cs_plot_qst()`.

## 1. Problem Statement

CausalStress currently scores ATT and QST, but the package has no first-class
estimand identity. That leaves three defects:

1. **Wrong-target scoring remains structurally possible.** Audit C4 was fixed
   for `tmle_att`, but the runner still scores by output slot (`att`, `qst`)
   rather than by a typed scoring join.
2. **ATE and CATE are scientifically central but absent as scored targets.**
   ATE is a standard scalar estimand. CATE is central to causal-ML benchmarks
   and is needed before the UX/API can be frozen honestly.
3. **The current capability and output surfaces are fragmented.** The registry
   has `supports_qst`; estimator metadata sometimes has `capabilities` and
   `target_level`; the runner and collectors hardcode ATT/QST columns. Adding a
   fourth surface would make the system less governed, not more.

## 2. Goals and Non-Goals

### Goals

- Define a typed estimand contract covering ATT, QST, ATE, and CATE.
- Make scoring a three-way join:
  `requested targets` intersect `estimator-produced targets` intersect
  `DGP-truth-available targets`.
- Split run identity into `fit_fingerprint` and `score_fingerprint`, so one
  expensive fit can produce multiple scored estimands without resume collisions.
- Define schema-4 migration behavior before implementation.
- Define CATE truth, held-out evaluation, keyed alignment, and metric vocabulary.
- Consolidate existing capability surfaces into one registry/output contract.
- Propose ratifiable Constitution 2.0.0 amendment text for Articles I, III, and
  IV.

### Non-Goals

- No implementation in this RFC.
- No public API freeze. RFC-2 consumes the accepted estimand contract and must
  decide the final user-facing names.
- No DGP-family or lambda-sweep design. RFC-3 consumes the metric/validity
  vocabulary and attaches per-regime validity envelopes.
- No gatekeeper calibration policy. The gate structure is defined here, but
  pass/fail thresholds, difficulty tiers, and registry consequences are
  deferred to the Gatekeeper recalibration RFC parked in `horizon.md`.
- No bootstrap-CI validity claim for GenGC QST.
- No QTE of `Y1 - Y0`, LATE/IV, mediation, or survival estimands.

## 3. Proposed Estimand Set

| Estimand | Target level | Target population | Truth convention | v0.2.0 status |
| --- | --- | --- | --- | --- |
| ATT | population scalar | treated rows in scoring population | finite-sample mean of structural `tau(X)` among treated units | existing, typed |
| ATE | population scalar | all rows in scoring population | finite-sample mean of structural `tau(X)` across all rows | new scalar |
| QST | distributional curve | treated rows in canonical oracle population | canonical `tau` grid, oracle MC / analytic truth as Article I currently specifies | existing, typed |
| CATE | unit-level / prediction target | held-out evaluation rows by default | conditional-mean structural effect `tau(X)`, not realized `Y1 - Y0` | new, keyed |

The default ATE convention is finite-sample structural ATE on the scoring
population, matching the current finite-sample ATT convention. A superpopulation
ATE may be added only if a DGP explicitly declares analytic/oracle support and
the truth descriptor distinguishes it from finite-sample ATE.

Real DGPs have no structural CATE truth unless they explicitly declare external
benchmark truth. Under the current Constitution, real DGPs have
`meta$structural_te = NULL` and `true_qst = NULL`, so CATE, ATE, and QST are
non-comparable unless a future real-data benchmark contract supplies external
truth for those targets.

## 4. Estimand Target Descriptor

Seed v2 recommends a normalized descriptor, not a flat enum and not the v1
triple. The exact serialized shape remains a synthesis decision, but it must be
expressive enough to prevent false equivalences.

Minimum semantic fields:

```text
estimand_target:
  id: att | ate | qst | cate
  descriptor_version: 1
  truth_tier: structural | distributional | external
  target_level: population | distributional | unit
  target_population: treated | all | eval | external
  evaluation_policy: training_rows | heldout_rows | oracle_population | external
  grid_id: none | cs_tau_oracle_v1 | custom_tau_id
  metric_set_id: scalar_error_v1 | qst_curve_v1 | cate_pehe_mace_v1
```

Recommendation for synthesis: store this as a canonical list internally and
derive a stable `estimand_target_id` string for filenames, pins, and compact
tables. Do not reduce the authoritative descriptor to a bare string.

## 5. Three-Way Scoring Join

The runner must never score an estimator against truth for a target it did not
produce or truth the DGP did not provide.

For each task:

1. Normalize requested estimand targets.
2. Resolve estimator capabilities from the consolidated registry.
3. Resolve DGP truth availability from the DGP descriptor and the generated
   truth payload.
4. Compute:
   `scoreable = requested intersect estimator-produced intersect DGP-truth-available`.
5. Emit one score record for each scoreable target.
6. Emit explicit non-comparable records for requested targets that cannot be
   scored, with machine-readable reasons:
   `estimator_not_produced`, `truth_unavailable`, `metric_invalid_for_regime`,
   `ci_unavailable`, `gate_unimplemented`, or `not_requested`.

Non-comparable is not failure. It is a governed state that prevents silent
cross-target scoring while preserving the task audit trail.

## 6. Fit and Score Identity

Schema 4 must separate model/fit identity from scored-claim identity.

### `fit_fingerprint`

Identifies the data/model computation:

- DGP id and version.
- Training sample size and training seed.
- Evaluation sample size and evaluation seed when the fit consumes evaluation
  covariates or when held-out scoring is part of the task contract.
- Estimator id and version.
- Canonical estimator config after runner-only keys are removed.
- CI intent only if CI computation is part of the estimator fit rather than a
  separate score layer.
- Tau/grid settings only if the estimator fit itself depends on them.
- Computational schema version.

### `score_fingerprint`

Identifies one scored scientific claim:

- `fit_fingerprint`.
- Full canonical `estimand_target` descriptor.
- Metric id and metric version.
- Truth source id / truth version / oracle algorithm fingerprint where relevant.
- Scoring population id (`training`, `heldout`, `oracle`, or external id).
- CATE keyed prediction digest when raw per-unit predictions are not stored.
- CI/gate descriptor if the score includes coverage or gate status.

This split permits one GRF/BART/GenGC fit to produce ATT, ATE, QST, or CATE
score records without refitting and without row identity collisions. It also
lets a user recompute a score layer when metrics or truth descriptors change,
without pretending the model fit changed.

## 7. Schema-4 Migration Matrix

Schema-4 migration is a blocking design commitment. The synthesis must preserve
the following behavior unless it explicitly argues for a stricter alternative.

| Artifact / stored schema | Resume behavior for ATT/QST requests | Resume behavior for ATE/CATE requests | Notes |
| --- | --- | --- | --- |
| Missing schema / schema 1 | Legacy comparison only for legacy ATT pins, using the existing v0.1.7 path. Never infer ATE/CATE. | Not reusable; run schema-4 task or emit non-comparable if truth/capability unavailable. | Preserve read/audit support. |
| Schema 2 | Reusable for ATT only if existing schema-2 fingerprint comparison succeeds and DGP version can be proven; otherwise existing hard-abort behavior remains. QST reuse only if the stored result actually contains QST and tau identity matches. | Not reusable. | Do not weaken the v0.1.10 schema-2 hard-abort guard. |
| Schema 3 | Reusable as legacy ATT/QST score records if config fingerprint, DGP version, estimator version, tau identity, and CI intent match. | Not reusable. | Absent `estimand_target` means "legacy ATT/QST artifact", not "all targets". |
| Schema 4 | Use `fit_fingerprint` and `score_fingerprint` exactly. | Use `fit_fingerprint` and `score_fingerprint` exactly. | No fallback to target defaults inside schema 4. |
| Unknown schema > 4 | Hard abort. | Hard abort. | Same posture as current unsupported-schema errors. |

Resume dispatchers that must implement this consistently:

- `cs-runner.R` / `cs_run_seeds()` resume path.
- `cs-run-campaign.R` grid/campaign resume path.
- `cs-run-one-seed.R` single-seed helper path.
- Plan/batch runner task stamping.
- Consolidation and audit surfaces.

Batch artifacts need their own schema bump because a batch now contains fit
records, score records, non-comparable records, and possibly keyed CATE digests.
Legacy batch artifacts remain readable, but new consolidation must validate both
fit and score fingerprints.

## 8. CATE Evaluation Protocol

Seed v2 commits to held-out CATE evaluation as the default, following the CATE
benchmarking convention used in IHDP/Shalit et al. and later CATENets-style
workflows. In-sample PEHE may be retained as a secondary diagnostic, but it must
not be the headline CATE benchmark metric.

Required protocol:

1. Generate the training sample with `seed_train` under the mandated RNG context.
2. Generate a held-out evaluation sample with `seed_eval` under the same DGP
   id/version and mandated RNG context.
3. The estimator fits on sanitized training data.
4. CATE-capable estimators must predict on held-out evaluation covariates.
5. The scorer joins predictions to held-out `structural_te` truth by `unit_id`.
6. The runner computes CATE metrics on the held-out sample.

RNG and fingerprint surface:

- `seed_train` remains the current task seed.
- `seed_eval` is either user-supplied or deterministically derived from
  `(seed_train, dgp_id, dgp_version, "cate_eval")`.
- Both seeds, `n_train`, `n_eval`, and the derivation rule enter the
  `fit_fingerprint` and the score metadata.
- Same-substrate reproducibility evidence must cover both training and held-out
  evaluation data.

Cost:

- CATE scoring requires a second DGP draw by default.
- Memory remains bounded if raw per-unit predictions are opt-in and the default
  stored artifact keeps only keyed-score digests plus aggregate metrics.

## 9. CATE Alignment Contract

Position-based unit matching is not allowed for CATE scoring.

Required contract:

- The runner adds an immutable `unit_id` to training and evaluation data before
  estimator execution and before truth extraction.
- `unit_id` is not a truth leak; it is a synthetic row key.
- Evaluation keys must be unique and stable within a task.
- CATE estimator output must be a tibble with at least `unit_id` and `estimate`.
- Optional columns may include `se`, `ci_lo`, `ci_hi`, and method-specific
  diagnostics.
- The scorer hard-errors on missing, duplicate, or extra `unit_id` values.
- The scorer computes truth joins from runner-held `scoring_truth`, not from the
  estimator input data.

Raw per-unit predictions are opt-in for storage. Even when raw predictions are
not stored, the score record must include a digest of the keyed prediction table
and the number of scored units.

## 10. Metric Vocabulary

RFC-1 defines the metric vocabulary. RFC-3 assigns validity envelopes by DGP
family/regime.

### Scalar ATT/ATE

- Point error.
- Absolute error.
- Bias/RMSE summaries across replications.
- CI coverage and width when valid CIs are reported.

### QST

- Pointwise error on the requested tau grid.
- Absolute error by tau.
- Pointwise CI coverage and width when valid CIs are reported.
- Existing 10/10 null-rejection summary remains a legacy gate component until
  the Gatekeeper recalibration RFC revises policy.

### CATE

Required metrics:

- `pehe_rmse`: square root of mean squared error between held-out
  `tau_hat(X)` and held-out structural `tau(X)`.
- `cate_median_abs_error`: median absolute CATE error on held-out rows.

Optional diagnostic metrics:

- Trimmed RMSE.
- Quantile loss / error quantiles.
- Calibration slope/intercept or grouped calibration once specified by a later
  calibration RFC.

PEHE remains primary because it is the common CATE benchmark metric. The robust
companion is required so heavy-tail or unstable-fit regimes do not reduce the
entire CATE assessment to squared-error pathology. RFC-3 decides when a metric
is valid, diagnostic-only, or invalid under a DGP regime.

## 11. Capability Consolidation

Do not add a fourth capability surface. Consolidate the existing surfaces:

- Registry `supports_qst`.
- Registry oracle metadata (`oracle`, `oracle_columns`, `oracle_default_columns`).
- Estimator result metadata `capabilities`.
- Estimator result metadata `target_level`.

Recommended v0.2.0 shape:

```text
capability:
  estimator_id
  estimator_version
  estimand_target_descriptor
  output_shape: scalar | curve | keyed_unit
  prediction_surface: none | training | heldout | both
  ci_support: none | native | bootstrap | external
  oracle_columns_eligible
  requires_pkgs
  experimental_status
  eligibility_fn_id
```

During migration, `supports_qst` may be generated from the capability table for
legacy code paths, but the capability table becomes authoritative. Estimator
output metadata must be validated against the registry declaration; it must not
silently invent targets.

## 12. Gatekeeper Structure

RFC-1 defines pluggability and target routing only. It does not decide the final
pass/fail calibration policy.

### ATT and QST

Existing Article IV ATT and QST gates remain the legacy baseline until the
Gatekeeper recalibration RFC changes their thresholds, difficulty tiers, or
registry consequences.

### ATE

ATE gets a scalar placebo gate slot analogous to ATT because ATE is a distinct
scored estimand. Whether it shares ATT thresholds, has a different difficulty
tier, or is reported only as profile evidence is deferred to Gatekeeper
recalibration.

### CATE

The naive per-unit placebo gate is dropped. CATE accuracy is scored on non-null
DGPs via held-out PEHE plus robust companion metrics.

Under placebo, CATE receives one of two statuses:

- `Unverified` for hallucination/detection claims if no heterogeneity-detection
  test is implemented.
- A detection-test result if a later RFC implements a principled test such as
  BLP/GATES, Imai-Li grouped inference, Crump-Hotz-Imbens-Mitnik heterogeneity
  tests, or RATE-style prioritization diagnostics.

The response-review verified the load-bearing references for this posture:
Chernozhukov et al. 2018 (BLP/GATES), Imai and Li 2025, Crump et al. 2008,
Hahn/Dorie/Murray 2019 ACIC 2017, Yadlowsky et al. RATE, and Yu/Sun 2025.

Calibration, difficulty tiers, and registry labels such as `Non-Robust` are
deferred to the Gatekeeper recalibration RFC parked in `horizon.md`.

## 13. Proposed Constitutional Amendment

Seed v2 recommends a **Constitution 2.0.0** amendment. Reason: the change is
semantic under the preamble rule. It changes Article I's enumerated truth
estimands, Article III Section 3.1's estimator output contract, and Article IV's
exclusive ATT/QST gatekeeper scope. This is not a patch clarification.

The package version may still be v0.2.0; this decision concerns the Constitution
version.

### Article I Amendment Sketch

Replace the current ATT/QST-only truth definitions with a governed estimand
registry over two truth tiers:

- **Structural tier:** finite-sample structural ATT, finite-sample structural
  ATE, and structural CATE as conditional-mean `tau(X)` on the declared scoring
  population. Structural truth is never computed from realized `y1 - y0` in
  heavy-tailed settings.
- **Distributional tier:** QST remains the treated-population quantile shift on
  the canonical grid, computed by the existing oracle/analytic standard.
- **External tier:** real-data truth is allowed only when supplied externally by
  the DGP descriptor; absent external truth yields non-comparable score records,
  not regenerated truth.

The amendment must state that CATE means the conditional-mean structural effect,
not the realized individual effect.

### Article III Section 3.1 Amendment Sketch

Replace the fixed output signature:

```text
function(df, tau, config) -> list(att, qst, meta)
```

with a typed output contract:

```text
function(df, tau, config) -> list(outputs, meta)
```

where `outputs` is a named collection of typed estimand outputs declared in the
estimator capability registry.

Required output shapes:

- Scalar outputs: one-row or scalar estimate with optional CI fields.
- Curve outputs: keyed by grid id / tau id with estimates and optional CI fields.
- CATE outputs: keyed by runner-issued `unit_id` with estimates and optional
  uncertainty fields.

Legacy `att` and `qst` slots remain supported only as compatibility shims for
schema <=3 and must be normalized to typed outputs inside the runner before
scoring.

The airlock rule remains: estimators receive sanitized data by default. Runner
truth used for scoring is a separate scorer-only channel.

### Article IV Amendment Sketch

Replace "These rules apply exclusively to ATT and QST" with:

- Gatekeeper components are per-estimand plugins attached to Article I estimands.
- Existing ATT and QST gates remain defined as legacy components until the
  Gatekeeper recalibration RFC revises policy.
- New ATE and CATE gate components must be explicitly declared before they can
  produce pass/fail labels.
- CATE placebo evaluation is not a per-unit zero check. It is `Unverified`
  unless a heterogeneity-detection/calibration test is implemented.
- Gate calibration, difficulty tiers, and registry consequences are governed by
  the future Gatekeeper recalibration RFC.

## 14. Downstream Surface and Blast Radius

The synthesis and implementation packet must cover at least:

- `R/cs-runner.R`
- `R/cs-run-campaign.R`
- `R/cs-run-one-seed.R`
- `R/cs-campaign-runner.R`
- `R/cs-fingerprint.R`
- `R/cs-estimator-registry.R`
- `R/cs-truth.R`
- `R/cs-airlock.R`
- `R/cs-result-to-row.R`
- `R/cs-collect.R`
- `R/cs-accessors.R`
- `R/cs-provenance.R`
- `R/cs-summary.R`
- `R/cs-summary-qst.R`
- `R/cs-gatekeeper.R`
- `R/cs-plots.R`
- `R/cs-consolidate.R`
- DGP validation and registry validation.
- Estimator adapters for GRF, BART, TMLE, GenGC, IPW, LM, oracle estimators.
- Man pages, vignettes, README, and migration notes.

Required round-trip tests:

- Run -> pin -> resume for schema 4 scalar targets.
- Run -> batch -> consolidate -> read -> tidy for schema 4 score records.
- Legacy schema 2/3 boards remain readable and only resume under the migration
  matrix.
- CATE held-out scoring joins by `unit_id` and hard-errors on missing,
  duplicate, or extra keys.
- Real DGPs with unavailable truth emit non-comparable records.
- `cs_science_payload()`, `cs_meta_flatten()`, audit, collectors, summaries, and
  plots do not silently drop ATE/CATE.

## 15. Decision Options Remaining for Synthesis

The following are intentionally left open for the synthesis, with seed-v2
recommendations.

### O1. Exact descriptor serialization

- **Option A:** Canonical R list plus derived compact id.
- **Option B:** Flat string id plus sidecar registry.

Recommendation: Option A. The list is the source of truth; compact ids are
derived.

### O2. Multi-estimand request shape

- **Option A:** One task may request multiple estimands and emit multiple score
  records sharing a `fit_fingerprint`.
- **Option B:** One task requests one estimand; users rely on planner
  de-duplication to avoid redundant fits.

Recommendation: Option A in the contract, but implementation may stage it by
first normalizing legacy one-target tasks into the new fit/score shape.

### O3. v0.2.0 staging

- **Option A:** Ship typed ATT/QST + ATE first, then CATE.
- **Option B:** Ship ATT/QST/ATE/CATE in one release.
- **Option C:** Ship CATE-only expansion first.

Recommendation: Option A if implementation risk is high; Option B only if the
schema-4 migration and CATE keyed held-out scoring tests are complete. Do not
ship CATE on the current scalar-shaped infrastructure.

### O4. RFC-2 freeze risk

CATE's natural tidy shape is per-unit/keyed and may constrain the UX/API freeze.

Recommendation: RFC-2 must not freeze final collector/plot names until the
accepted RFC-1 synthesis fixes the CATE score-record shape. At minimum, RFC-2
should freeze generic typed collectors before freezing any CATE-specific
wide/long convenience API.

## 16. Disposition of Response and Response-Review

| Item | Disposition in seed v2 |
| --- | --- |
| F1 CATE train/test | Incorporated. Held-out CATE evaluation is the default; in-sample PEHE is secondary only. RNG/fingerprint cost is specified. |
| F2 keyed `unit_id` | Incorporated. Runner-issued immutable `unit_id` is required; keyed join hard-errors on mismatch. |
| F3 CATE placebo gate | Incorporated as restated. Naive per-unit gate is dropped; placebo CATE is Unverified unless detection tests are implemented; calibration deferred. |
| F4 PEHE heavy tails | Incorporated with corrected severity. PEHE primary plus required robust companion; regime validity deferred to RFC-3. |
| F5 version bump | Incorporated. Seed v2 recommends Constitution 2.0.0 because Articles I, III, and IV change semantically. |
| F6 GRF/BART overstated | Incorporated. Seed v2 treats GRF/BART CATE as real adapter work, not trivial un-collapse. |
| F7 registry state | Incorporated and expanded by N1. Capability consolidation replaces the fragmented surfaces. |
| F8 descriptor too small | Incorporated. v1 triple rejected; normalized descriptor recommended; exact serialization remains open. |
| F9 fit vs score identity | Incorporated as blocker-grade. `fit_fingerprint` and `score_fingerprint` are defined. |
| F10 schema-4 blast radius | Incorporated as blocker-grade. Migration matrix and affected dispatchers are specified. |
| F11 missed surfaces | Incorporated. Blast radius names accessors, audit, summaries, plots, batches, and docs. |
| F12 ATE finite vs superpopulation | Incorporated. ATE defaults to finite-sample structural ATE; superpopulation ATE requires explicit truth descriptor. |
| F13 real DGPs / three-way join | Incorporated as blocker-grade. Scoring join includes DGP truth availability and non-comparable records. |
| F14 airlock two channels | Incorporated. Estimator input and scorer truth are separate channels. |
| F15 RFC ordering | Incorporated. RFC-1 defines metric vocabulary and validity object shape; RFC-3 attaches envelopes; RFC-2 freeze risk is explicit. |
| N1 fragmented capability surfaces | Incorporated. The registry becomes authoritative and legacy fields are derived/shimmed. |
| N2 Article III scope | Incorporated. Amendment text changes Article III Section 3.1. |
| N3 RFC-2 freeze feasibility | Incorporated as O4 and blast-radius requirement. |
| N4 held-out RNG surface | Incorporated. `seed_eval`, `n_eval`, derivation rule, and reproducibility evidence are specified. |

No response finding is rejected outright. The only disagreements with Codex are
severity/calibration corrections inherited from the response-review: F1/F2/F4
are not blockers against the program, but F1/F2 are required; F3 is not "add a
better CATE recovery gate now" but "drop the naive per-unit placebo gate and use
detection-test-or-Unverified."

## 17. Acceptance Criteria for Synthesis

The synthesis is acceptable when it:

1. Accepts, rejects, or amends the Constitution 2.0.0 recommendation and gives
   ratifiable Article I, III, and IV text.
2. Fixes the authoritative `estimand_target` descriptor serialization.
3. Accepts or amends the `fit_fingerprint` / `score_fingerprint` split.
4. Finalizes the schema-4 migration matrix, including all resume dispatchers,
   batch consolidation, audit, and legacy read behavior.
5. Fixes the three-way scoring join and non-comparable record schema.
6. Accepts held-out CATE evaluation and keyed `unit_id` alignment, or records a
   maintainer-level rejection of the response-review ruling.
7. Fixes the CATE metric vocabulary: PEHE plus robust companion, with validity
   envelopes delegated to RFC-3.
8. Defines the gatekeeper plugin interface while explicitly deferring calibration
   and registry consequences to the Gatekeeper recalibration RFC.
9. Chooses the implementation staging option for v0.2.0.
10. States the exact RFC-2 surface that may be frozen and the surfaces that must
    remain experimental until CATE score shape is implemented.

No implementation may begin until the synthesis is accepted and the
constitutional amendment path is ratified.
