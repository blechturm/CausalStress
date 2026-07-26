# CausalStress v0.2.0 Spec

**Status:** ACTIVE - activated after adversarial review
**Packet:** `causalstress_v0_2_0_spec_packet`
**Date opened:** 2026-06-16
**Authority:** Draft packet proposal. Implementation MUST NOT begin until this spec is reviewed and the packet is activated in `inst/design/README.md`.
**Constitutional baseline:** `CAUSAL_STRESS_CONSTITUTION.md` v2.0.0, "CausalStress Scientific Protocol", ratified 2026-06-16.
**Authorizing RFC:** `inst/design/rfc/20260616_estimand_registry_synthesis.md` (RFC-1 Estimand Registry), accepted 2026-06-16.

## Objective

Implement Wave 1 of the v0.2.0 estimand-registry program:

1. Introduce typed estimand targets and typed scoring across the runner, registry, persistence, and collection surfaces.
2. Preserve existing ATT and QST behavior through a compatibility shim.
3. Add ATE as the first new scalar estimand using structural truth already carried by synthetic DGPs.
4. Split fit identity from score identity so one fit can produce multiple scored records without refitting.
5. Explicitly stage CATE to Wave 2 by hard-rejecting CATE-only tasks before execution and emitting `target_not_implemented` rows for CATE requested alongside implemented targets.

This release is the foundation release for the v2.0.0 protocol. It should make cross-scoring structurally impossible without taking on the high-risk CATE implementation surface yet.

## Scope

- Implement canonical `estimand_target` descriptors for ATT, ATE, QST, and CATE, with derived compact target ids only as serialization/filename conveniences.
- Add typed estimator outputs: `list(outputs, meta)`, where `outputs` is keyed by `estimand_target_id`.
- Keep `list(att, qst, meta)` as a legacy input shape and normalize it before scoring.
- Add the three-way scoring join: `requested` intersect `estimator-produced` intersect `DGP-truth-available`.
- Emit explicit non-comparable records for unscoreable requested targets.
- Add the required non-comparable reason vocabulary at minimum:
  `estimator_not_produced`, `truth_unavailable`, `metric_invalid_for_regime`, `ci_unavailable`, `gate_unimplemented`, `not_requested`, `target_not_implemented`.
- Implement synthetic ATE truth as `mean(meta$structural_te)` over the declared ATE scoring population, defaulting to the full generated run sample.
- Pin Wave 1 ATE scoring to the default full generated run sample; configurable or non-default ATE scoring populations are deferred.
- Migrate the structural oracle benchmark estimator (`oracle_att`) to emit typed ATT and ATE outputs while preserving the legacy `oracle_att` id and ATT compatibility behavior.
- Preserve real-DGP external-truth behavior: real DGPs without externally supplied ATE/CATE truth produce `truth_unavailable`, never regenerated truth.
- Introduce fit-artifact and score-record identities in the runner, resume, persistence, consolidation, audit, and collection paths.
- Write schema-4 artifacts for new runs and provide explicit migration/compatibility behavior for historical schema 1-3 artifacts.
- Update `contracts.md` so the code-facing estimator-output contract documents the typed `outputs`/`meta` shape and legacy shim.
- Keep ATT/QST gatekeeper components and their existing enforcement unchanged.
- Add an ATE gate slot only as structure; calibration, difficulty tiers, and registry consequences remain deferred.
- Represent CATE as a registered target but do not implement CATE fitting, prediction, PEHE, held-out evaluation, or CATE UX in Wave 1.
- Ensure CATE requests before Wave 2 follow the deterministic staging rule: CATE-only tasks hard-reject before execution; mixed-target tasks emit `target_not_implemented` for CATE and continue scoring implemented targets.

## Non-Scope

- CATE implementation beyond explicit staging and `target_not_implemented` handling.
- Held-out CATE evaluation sample generation, CATE prediction APIs, PEHE, robust CATE companion metrics, or CATE collection/plot UX.
- Gatekeeper recalibration policy, difficulty tiers, or any change to the existing ATT/QST `Non-Robust` consequences.
- A generalized external-truth tier for ATE/CATE real DGPs.
- New DGP families, kill-plot/families campaign work, sensitivity-axis work, or new scientific estimators.
- GenGC QST bootstrap inference validation or bootstrap-calibration changes.
- RFC-2 UX freeze. RFC-2a design may begin after this spec review, but the scalar/typed-scoring UX must not freeze until Wave 1 lands and is reviewed.
- Documentation-site work, pkgdown, CI/coverage infrastructure, and vignettes unless a later packet explicitly promotes them.

## Design

### Typed estimand targets

The canonical target descriptor is an R list, not a bare string. It includes the target id, truth tier, target level, target population, evaluation policy, and any grid/metric identifiers required to score the target. A compact `estimand_target_id` may be derived for filenames, pins, and table columns, but the descriptor remains authoritative.

Wave 1 must ship descriptors for:

| Target | Tier | Level | Population | Wave 1 scoring |
| --- | --- | --- | --- | --- |
| ATT | structural | population-scalar | treated | scoreable where produced and truth exists |
| ATE | structural | population-scalar | all | scoreable where produced and truth exists |
| QST | distributional | distributional-curve | treated | scoreable where produced and truth exists |
| CATE | structural | unit-level | held-out eval | not implemented; `target_not_implemented` |

### Estimator output normalization

The runner accepts two shapes during Wave 1:

- New shape: `list(outputs, meta)`.
- Legacy shape: `list(att, qst, meta)`.

Legacy outputs are normalized before scoring:

- `att` becomes the ATT typed output.
- `qst` becomes the QST typed output.
- Missing legacy fields are absence of production, not estimator failure.

No estimator output may be scored against a different target. An ATE output must not be scored as ATT, and an ATT output must not be scored as ATE.

### ATE truth and scoring

For synthetic DGPs, finite-sample ATE truth is computed from structural truth:

```r
mean(meta$structural_te)
```

The scoring population is the declared ATE target population, fixed in Wave 1 to the full generated run sample. Configurable non-default scoring populations and superpopulation ATE are out of Wave 1 unless separately authorized by a later packet.

For real DGPs, ATE truth is unavailable unless externally supplied by the DGP contract. Wave 1 must emit `truth_unavailable` rather than regenerating, estimating, or borrowing ATT truth.

### Scorer-only truth channel and airlock

ATE truth is computed by the runner/scorer from the DGP result's scorer-only truth channel. `structural_te` must not be exposed to ordinary estimators through the typed-output path.

The Wave 1 flow is:

1. The DGP result carries truth fields, including `meta$structural_te`, in runner-owned state.
2. The runner airlock sanitizes estimator input according to Constitution Article III before estimator execution.
3. The estimator returns typed outputs or a legacy output shape.
4. The scorer computes ATT/ATE/QST truth from runner-owned truth state and scores only matching targets.

The structural oracle benchmark is the controlled exception: `oracle_att` remains an oracle estimator with scoped `structural_te` access, and Wave 1 migrates it to produce typed ATT and ATE outputs for contract testing. That exception must remain explicit in the estimator registry and must not generalize to ordinary estimators.

### Fit and score identity

Wave 1 introduces two identities:

- Fit artifact identity: data/model/config identity for a single model fit.
- Score record identity: fit identity plus scored estimand target, metric, truth version, and `scoring_population_id`.

One fit may yield multiple score records. Score-layer recomputation must not require refitting when the fit artifact is unchanged.

Truth version means the DGP truth identity used for scoring: `(dgp_id, dgp_version)` plus any oracle/truth-table identity or hash that distinguishes generated truth payloads.

`scoring_population_id` is an active Wave 1 score-record field. ATT/QST use their declared treated-population ids; ATE uses the fixed full-generated-run-sample id. It must be populated in new schema-4 score records, not left as a Wave 2 placeholder.

Evaluation-sample identity is not part of the fit artifact in Wave 1 because CATE is not implemented. Schema 4 must still reserve nullable score-record fields required by Wave 2 CATE so Wave 2 is additive, not a schema migration:

- `seed_eval`
- `n_eval`
- `eval_derivation`
- `unit_id_digest`
- `prediction_digest`
- `transductive`

For future declared transductive estimators, evaluation-sample identity becomes part of the fit identity as required by the Constitution.

### Persistence and resume

Schema-4 artifacts must distinguish fit artifacts from score records. Workers must only write their own atomic artifacts and must not modify shared board state.

Historical schema 1-3 artifacts are read-only historical inputs in Wave 1. Collectors and audit helpers may import them through explicit compatibility paths for historical summaries, but schema 1-3 artifacts are not valid resume targets for schema-4 runs. Any attempt to resume a schema 1-3 artifact into a schema-4 run must fail closed with a classed migration/fresh-run error.

### Collection, tidy, science payload, and audit surfaces

Wave 1 defines a canonical typed score surface. Public and internal collection paths must be driven from this surface rather than from ATT/QST-specific hardcoded columns.

The canonical score table is long-form. It has one row per scalar score or curve point, with enough keys to reconstruct score records:

- run identity: `dgp_id`, `dgp_version`, `estimator_id`, `estimator_version`, `n`, `seed`
- artifact identity: `fit_fingerprint`, `score_fingerprint`, `schema_version`
- target identity: `estimand_target_id`, target descriptor fields, `metric_id`
- point coordinate: `tau`/`tau_index` for QST rows, `NA` for scalar ATT/ATE rows
- values: `estimate`, `truth`, `error`, optional `ci_lo`, `ci_hi`
- status: `score_status` in `scored`, `non_comparable`, `estimator_error`, `runner_error`
- non-comparability: `non_comparable_reason`, populated only when `score_status = "non_comparable"`

Non-comparable requested targets must appear as rows in this typed surface. Their estimate/truth/error/CI fields are `NA`, and `non_comparable_reason` records the reason (`truth_unavailable`, `target_not_implemented`, etc.).

QST may render as multiple rows sharing a score record identity, one row per runner-provided tau grid point. Tau coordinates are point coordinates, not separate estimand targets.

Legacy user-facing projections may remain for compatibility:

- ATT collectors may filter `estimand_target_id == "att"` and expose the existing ATT column names.
- QST collectors may filter `estimand_target_id == "qst"` and expose the existing tau-grid shape.
- Science payloads and audit summaries must include the typed score surface or a lossless projection of it, including non-comparable rows and reasons.

The old ATT/QST-shaped helpers must either become compatibility projections over the typed surface or be explicitly marked legacy. They must not remain independent scoring authorities.

### Gatekeeper

ATT and QST gatekeeper behavior is unchanged in Wave 1.

ATE receives a gatekeeper component slot only. Thresholds, difficulty tiers, and registry consequences for ATE are not decided in this packet.

CATE receives no placebo gate in Wave 1. The staging rule is deterministic:

- CATE-only tasks hard-reject before estimator execution with a classed `target_not_implemented` error.
- Multi-target tasks that include CATE emit a `target_not_implemented` non-comparable score row for CATE and continue scoring implemented targets.

Both branches must be tested.

### Wave 2 carry-forward obligations

Wave 2 CATE implementation must inherit the RFC-1 obligations already ratified by the Constitution:

- held-out evaluation by default
- deterministic `unit_id`
- eval predict airlock = covariates plus runner-issued `unit_id` only
- exclusion of `y`, `w`, `y0`, `y1`, `p`, and `structural_te` from the CATE predict input
- scorer-only held-out structural truth
- eval identity fields recorded in score-record identity, and fit identity for declared transductive estimators

## Constitutional Compliance

- Article I: implements the registered estimand set, typed scoring, ATE truth, and CATE staged implementation.
- Article II: preserves RNG and truth reproducibility obligations; Wave 1 does not create held-out CATE draws.
- Article III: implements the typed estimator output contract and legacy compatibility shim; preserves airlock rules.
- Article IV: preserves existing ATT/QST gatekeeper enforcement; adds only the ATE structural slot authorized by v2.0.0.
- Article V: implements fit-artifact and score-record granularity.
- Article VI: preserves atomic persistence and worker isolation under the new artifact identities.
- Article VII: preserves DGP immutability; no DGP logic changes are authorized by this spec.

Any implementation that requires changing a released DGP's scientific logic, regenerating real-DGP truth, cross-scoring an estimator output, or applying new gatekeeper policy is out of scope until separately authorized.

## Acceptance Criteria

- The package exposes or internally maintains canonical `estimand_target` descriptors for ATT, ATE, QST, and CATE.
- All runner scoring flows use the three-way scoring join and emit explicit non-comparable records instead of silent drops or cross-scoring.
- Legacy `list(att, qst, meta)` estimator outputs still run and produce ATT/QST scored values numerically identical to the pre-Wave-1 regression corpus; only the schema envelope may differ.
- ATE truth for synthetic DGPs equals `mean(meta$structural_te)` over the full generated run sample in Wave 1.
- Real DGP ATE/CATE requests without external truth produce `truth_unavailable`.
- CATE-only tasks before Wave 2 hard-reject before estimator execution; mixed-target tasks before Wave 2 emit a `target_not_implemented` row for CATE and score implemented targets.
- Fit and score identities are distinct in new artifacts; one fit can produce multiple score records without overwriting.
- Schema 4 populates `scoring_population_id` in Wave 1 score records; ATT/QST/ATE use their declared Wave 1 population ids.
- Schema 4 reserves nullable Wave 2 CATE eval-identity fields (`seed_eval`, `n_eval`, `eval_derivation`, `unit_id_digest`, `prediction_digest`, `transductive`).
- Schema 1-3 artifacts are not valid schema-4 resume targets; resume attempts fail closed with a classed migration/fresh-run error.
- Historical schema 1-3 artifacts remain importable for historical collection/audit summaries where explicit compatibility paths exist.
- The canonical typed collection surface is long-form and includes scored rows and non-comparable rows with machine-readable reasons.
- ATT/QST collection helpers are compatibility projections over the typed score surface or explicitly marked legacy.
- Science payload and audit outputs preserve typed target ids, score statuses, and non-comparable reasons.
- ATT/QST gatekeeper behavior is unchanged by focused regression tests.
- ATE gatekeeper policy is not calibrated, labeled, or made registry-consequential in Wave 1.
- The structural oracle benchmark (`oracle_att`) emits typed ATT and ATE outputs, preserving legacy ATT behavior while providing an end-to-end ATE-producing estimator.
- `contracts.md` is updated so its estimator-output contract entry names the typed `outputs`/`meta` contract, legacy shim behavior, and the Wave 1 target support rules.
- A focused end-to-end ATE test runs a synthetic DGP through the typed runner with the structural oracle benchmark and verifies scored ATE equals `mean(meta$structural_te)`.
- A contract test proves ordinary estimators never receive `y0`, `y1`, `p`, or `structural_te` under the typed path while scorer-side ATE truth still computes correctly.
- The release has tests covering output normalization, no-cross-scoring, non-comparable reasons, ATE truth, real-DGP truth unavailability, both CATE staging branches, artifact identity uniqueness, typed collection/audit output, and legacy ATT/QST compatibility.
- Legacy ATT/QST regression tests prove scored ATT/QST values are numerically identical to the pre-Wave-1 corpus; only the schema envelope may differ.
- Before implementation begins, this spec is reviewed adversarially and all findings are routed into packet artifacts.

## RFC Findings Consumed

| Source | Disposition in v0.2.0 Wave 1 |
| --- | --- |
| RFC-1 output contract | Implement typed `outputs` with legacy shim. |
| RFC-1 three-way scoring join | Implement for all requested targets. |
| RFC-1 fit/score identity | Implement for Wave 1 scalar/distributional targets; reserve nullable Wave 2 CATE eval-identity fields in schema 4. |
| RFC-1 ATE | Implement finite-sample structural ATE truth and scoring. |
| RFC-1 CATE | Register target only; stage implementation to Wave 2 via deterministic `target_not_implemented` behavior. |
| RFC-1 CATE Wave 2 obligations | Carry forward deterministic `unit_id`, held-out eval identity, and eval predict airlock = covariates plus runner-issued `unit_id`. |
| RFC-1 gatekeeper | Structure only for ATE; defer calibration and registry consequences. |
| Constitution v2.0.0 review | Use ratified Article I/II/III/IV/V/VI wording as the binding baseline. |
| Horizon gatekeeper recalibration | Explicitly deferred; no policy change in Wave 1. |

## Open Decisions

| Decision | Owner | Required before |
| --- | --- | --- |
| Whether RFC-2a scalar UX freeze begins immediately after Wave 1 or after one additional UX cleanup packet | Maintainer | Closeout |
