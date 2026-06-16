# Constitutional Review: Proposed Constitution v2.0.0

**Status:** Ratification review
**Date:** 2026-06-16
**Reviewer posture:** Constitutional Review Board
**Reviewed amendment:** `git diff HEAD -- inst/design/CAUSAL_STRESS_CONSTITUTION.md`
**Last ratified text:** `HEAD:inst/design/CAUSAL_STRESS_CONSTITUTION.md` (v1.8.2)
**Mandate:** accepted RFC-1 synthesis `20260616_estimand_registry_synthesis.md`, including Revision 1 and Revision 2, plus the adjudication trail and `horizon.md` deferrals.

## Executive Verdict

**RATIFY-WITH-AMENDMENTS.**

The amendment is directionally faithful to RFC-1: it adds ATT/ATE/QST/CATE as governed estimands, moves the estimator return contract to typed `outputs`, preserves the existing real-DGP external-truth clauses, and does not bake in the deferred gatekeeper recalibration or a generalized external-truth tier.

It is not ratifiable as written. Four issues are constitutional blockers because the proposed text either silently weakens an existing guarantee or declares new CATE / multi-estimand behavior without the enforceable identity, staging, and RNG rules that RFC-1 made load-bearing.

---

## BLOCKER Findings

### B1. Atomic persistence still uses the v1 result grain and contradicts the accepted fit/score model

**Clause attacked:** Article V Sec. 5.2 and Article VI.

**Diff evidence:** The amendment changes Articles I, III, and IV, but leaves Article V/VI unchanged. The proposed v2.0.0 text still says:

- Article V Sec. 5.2: "Results must be persisted (pinned) at the granularity of a single run (DGP x Estimator x Seed)."
- Article VI: "Every simulation result (DGP x Estimator x Seed) must be persisted to a unique storage location or identifiable partition."

The accepted synthesis requires a different identity model:

- Sec. 1.2: estimator outputs keyed by `estimand_target_id`, but score records keyed by `(estimand_target_id, metric_id)` / `score_fingerprint`.
- Sec. 1.5: `fit_fingerprint` plus `score_fingerprint`; one fit can produce multiple scored estimands and score-layer recompute without refitting.
- Sec. 1.11/Sec. 1.12: staged multi-estimand requests share a fit identity.

**Why this is a constitutional defect:** The old atomic grain (`DGP x Estimator x Seed`) cannot uniquely identify multiple score records from one fit. CATE alone can have multiple metrics for one target; future multi-estimand tasks intentionally produce more than one scored record from one fit. Leaving Article VI at the old grain creates a supreme-law collision: the Constitution declares typed scoring, but its atomic persistence rule still permits only one result identity at the old scalar grain. That reopens overwrite/collision ambiguity in exactly the layer Article VI is supposed to govern.

**Mandatory corrective text:**

Replace Article V Sec. 5.2 with:

> To prevent data loss, fit artifacts and score records are atomic at their declared identities. A fit artifact is persisted at the granularity of one model fit. A score record is persisted at the granularity of one fit plus one scored estimand target and, where applicable, one metric and scoring population. Aggregation into suites happens only after secure storage of these atomic artifacts.

Replace Article VI's Atomicity bullet with:

> **Atomicity:** Every fit artifact and every score record must be persisted to a unique storage location or identifiable partition under its declared identity. Workers must never overwrite, append to, or modify existing fit artifacts, score records, or result pins.

This keeps the Constitution at the right abstraction level: it constitutionalizes the fit/score distinction and uniqueness requirement without freezing schema numbers or file names.

### B2. The Rev 2 `target_not_implemented` interim rule is missing from the Articles

**Clause attacked:** Amendment history and Article I Sec. 1.7.

**Diff evidence:** The amendment history states implementation is staged: "Wave 1: ATT/ATE typed scoring; Wave 2: CATE." Article I Sec. 1.7 says unscoreable requests get a machine-readable non-comparable reason, but it does not name the required interim rule.

The accepted synthesis is explicit:

- Sec. 1.4 adds `target_not_implemented`.
- Sec. 1.11 says CATE requests before Wave 2 must be emitted as non-comparable with reason `target_not_implemented` or hard-rejected, never silently scoreable.
- Sec. 4 acceptance item 3 repeats that interim rule.

**Why this is a constitutional defect:** The Constitution would declare CATE a valid estimand before the runner implements CATE. Without a binding interim rule, an implementation can silently omit CATE, emit empty rows, or let CATE appear scoreable before Wave 2. That is exactly the transition gap Rev 2 was written to close.

**Mandatory corrective text:** Add this bullet to Article I Sec. 1.7:

> **Staged implementation:** During any staged implementation, a declared estimand whose runner/scorer support has not shipped MUST be rejected before execution or recorded as non-comparable with reason `target_not_implemented`. It MUST NOT be silently dropped, cross-scored, marked failed, or allowed to appear scoreable.

### B3. Held-out CATE evaluation creates a second governed random draw, but Article II remains singular-seed only

**Clause attacked:** Article I Sec. 1.6, Article II Sec. 2.2, and Article III Sec. 3.1.

**Diff evidence:** The amendment adds "CATE is scored on a held-out evaluation sample by default" (Sec. 1.6) and defines the held-out predict-input airlock (Sec. 3.1), but Article II Sec. 2.2 remains unchanged:

- "All DGPs must accept a `seed` argument."
- "The Runner must capture and store the seed in the result metadata."
- Same-substrate identity is keyed by `DGP ID`, `Version`, and `Seed`.

The accepted synthesis made the held-out draw load-bearing:

- Sec. 1.5: eval-sample identity (`seed_eval`, `n_eval`, derivation rule) belongs in the score/prediction fingerprint, not the fit fingerprint.
- Sec. 1.6: deterministic `unit_id` assignment must enter the eval-sample identity/fingerprint.
- Sec. 1.7: `seed_eval` is derived from the training seed, DGP id/version, and CATE eval purpose; predict input is covariates plus `unit_id`.
- Open risks: held-out CATE doubles generation cost and adds the `seed_eval`/`n_eval` surface; deterministic `unit_id` must be pinned.

**Why this is a constitutional defect:** A held-out CATE evaluation set is not just an implementation detail. It is a second truth-bearing sample with its own randomness and row identity. If the Constitution only requires storing "the seed," the runner can satisfy Article II while failing to record which evaluation sample was scored. That breaks same-substrate reproducibility and makes CATE score records non-auditable.

**Mandatory corrective text:** Add this to Article I Sec. 1.6 or Article II Sec. 2.2:

> For held-out CATE evaluation, the evaluation sample is a governed scoring population. Its seed, size, deterministic derivation rule, and deterministic `unit_id` assignment MUST be recorded in the score identity or release evidence. The evaluation sample and its structural truth are scorer-only unless an estimator explicitly declares a transductive fitting mode; otherwise, evaluation-sample identity MUST NOT affect the fit identity.

### B4. QST oracle-size immutability accidentally expires at v1.x.y

**Clause attacked:** Article I Sec. 1.4, Distributional QST.

**Diff evidence:** The proposed v2.0.0 text carries forward the ratified v1.8.2 sentence unchanged:

> "The oracle size N=10^6 is immutable for v1.x.y."

The amendment history claims existing QST truth is unchanged.

**Why this is a constitutional defect:** In a proposed v2.0.0 Constitution, leaving the immutability guarantee scoped only to `v1.x.y` silently narrows the QST guarantee. It can be read as saying the oracle size is no longer constitutionally frozen in v2.x, which contradicts the mandate to preserve existing QST truth.

**Mandatory corrective text:**

> The oracle size N=10^6 is immutable within the v2.x line unless changed by a future major constitutional amendment.

An even more durable version is:

> The oracle size N=10^6 is immutable within a major constitutional line unless that major amendment explicitly changes it.

---

## MAJOR Findings

### M1. Article I says there are only two target levels, but the registry creates a third

**Clause attacked:** Article I introduction and Sec. 1.7 table.

**Diff evidence:** The new Article I introduction says estimands live at two target levels: "population (scalar) and unit." The new Sec. 1.7 table then assigns QST the level "distributional."

**Why this is a constitutional defect:** Target level is part of the typed `estimand_target`. A contradictory enumeration in Article I makes descriptor validation ambiguous: is QST's distributional level constitutionally valid, or did the introduction define the complete set?

**Corrective text:**

> ... over a registry of estimands at governed target levels: population-scalar, distributional-curve, and unit-level.

### M2. The ATE finite-sample rule uses "generated rows" but the amendment introduces explicit scoring populations

**Clause attacked:** Article I Sec. 1.5.

**Diff evidence:** Sec. 1.5 says finite-sample ATE is "over the generated rows." RFC-1, however, requires score identity to include the scoring population, and CATE explicitly introduces held-out scoring populations.

**Why this is a constitutional defect:** "Generated rows" is not precise enough once a task may have a training sample, an evaluation sample, and possibly future score-layer recomputation. For ATE, the rule needs to say which governed population is averaged. Otherwise two compliant implementations could compute different ATE truth for the same task.

**Corrective text:**

> **Default convention:** finite-sample structural ATE over the declared scoring population for the ATE target, defaulting to the primary generated run sample unless the DGP truth descriptor explicitly declares an analytic/oracle superpopulation target. The truth descriptor MUST distinguish finite-sample ATE from any superpopulation ATE.

### M3. The old "Gatekeeper testing applies primarily to ATT" CI clause is now stale

**Clause attacked:** Article III Sec. 3.1, Confidence Intervals.

**Diff evidence:** The amendment changes Article IV Sec. 4.2.5 from "exclusively ATT and QST" to per-estimand components and adds an ATE gate slot, but leaves Article III Sec. 3.1 saying:

> "Gatekeeper testing applies primarily to ATT."

**Why this is a constitutional defect:** The sentence was tolerable in v1 as a rough description of the ATT-heavy gate. In v2 it conflicts with the new per-estimand structure and can be misread as downgrading QST and ATE gate applicability. Because Article IV is the authoritative gatekeeper article, Article III should refer to it instead of preserving a stale scalar-era summary.

**Corrective text:**

> Gatekeeper testing applies according to the per-estimand components defined in Article IV. CI-based gate checks apply only where an estimator reports confidence intervals or declares a valid alternative uncertainty method.

---

## MINOR Findings

### m1. `tau_id` is undefined at constitutional level

**Clause attacked:** Article III Sec. 3.1 output shapes.

**Diff evidence:** The amendment says QST output is a "curve (keyed by `tau_id`)." No Article defines `tau_id`.

**Why this matters:** The Constitution should define the normative requirement, not an undefined implementation label. The stable rule is that QST outputs must align to the runner-provided tau grid and declared QST target.

**Corrective text:**

> curve keyed to the runner-provided QST tau grid for QST

### m2. Article V still says "v0.1.x is serial by default"

**Clause attacked:** Article V Sec. 5.1.

**Diff evidence:** The text is unchanged from v1.8.2 and still says:

> "v0.1.x is serial by default"

**Why this matters:** This is not caused by RFC-1, but ratifying a v2.0.0 Constitution with a stale v0.1.x release-line statement weakens the document's authority and creates confusion about whether the rule is historical or current.

**Corrective text:**

> By default, release lines are serial unless their active release specification authorizes parallel execution under the Article VI protocol.

---

## Verified Conformity Points

- **Real-DGP external truth is preserved.** Article I Sec. 1.3's Real Data clause remains verbatim, Article III Sec. 3.2.B remains the real-DGP contract, and Sec. 1.7 correctly says the amendment does not create a new external truth tier. This satisfies RFC-1 Rev 1 B1.
- **No generalized external-truth tier is baked in.** Sec. 1.7 correctly defers generalized ATE/CATE external truth to a future real-data RFC.
- **Gatekeeper recalibration is not decided here.** Article IV Sec. 4.2.5 creates ATE/CATE structure but defers thresholds, difficulty tiers, registry consequences for new gates, and recalibration of ATT/QST to the future Gatekeeper RFC in `horizon.md`.
- **The major version bump is correct.** The estimator output signature and Article IV scope change are semantic changes to constitutional Articles. A 2.0.0 bump is required under the Preamble rule.
- **The airlock direction is faithful.** The CATE predict-input airlock uses covariates plus runner-issued `unit_id` and excludes `y`, `w`, `y0`, `y1`, `p`, and `structural_te`, matching RFC-1 Rev 1/2.

## Ratification Verdict

**RATIFY-WITH-AMENDMENTS**, not as-is.

Mandatory before ratification:

1. Amend Article V/VI to support separate fit artifact and score record identities.
2. Add the `target_not_implemented` staged-implementation rule.
3. Add held-out CATE evaluation-sample seed/size/derivation/`unit_id` traceability.
4. Fix QST oracle-size immutability so it remains binding in v2.x.
5. Correct the target-level enumeration to include distributional targets.
6. Tighten ATE truth to the declared scoring population.
7. Replace the stale Article III CI/gatekeeper sentence with a reference to Article IV per-estimand gates.

After these amendments, the proposal should be ratifiable without a redraft. Without them, ratification would silently weaken QST truth immutability and declare a multi-estimand/CATE constitutional regime whose persistence, transition, and RNG guarantees are not enforceable.
