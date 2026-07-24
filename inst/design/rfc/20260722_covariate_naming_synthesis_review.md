# RFC Synthesis Review: DGP Contract Terminology Clarifications

**Status:** REVIEW — non-binding
**Date:** 2026-07-24
**Reviewer:** Codex
**Reviews:** `20260722_covariate_naming_synthesis.md`
**Seed author disclosure:** I authored the seed under review's upstream.

## Verdict

**CONCUR WITH CORRECTIONS — ready after the listed fixes.**

I confirm the recommended **patch** grade, the uppercase `X1...Xk` decision,
the synthetic-only scope, and the `real-data DGP` prose normalization. The
literalist counter-argument is real and was stated fairly, but the repository
history supports treating lowercase `x1...xk` as a transcription defect: the
baseline generator used uppercase `X1...X5` before the lowercase constitutional
clause was introduced, and every subsequently registered version retained that
convention.

Three corrections are required before maintainer final review:

1. The synthesis must not claim that code or tests made a lowercase-`x1` DGP
   constitutionally non-conformant. The Constitution is supreme; under the
   literal v2.0.0 text, the code is the non-conforming side of the mismatch.
2. The constitutional text should retain the accepted uppercase, consecutive,
   one-based rule but remove the undefined reserved-column parenthetical,
   especially the anachronistic reference to Runner-issued `unit_id`.
3. Validator enforcement must be a release-blocking v0.2.0 correction, not an
   unspecified later follow-through. Article VII requires continuous automated
   enforcement.

These corrections do not require another full RFC cycle. They require a revised
synthesis (or explicit maintainer amendments at final review) before
ratification.

## Independently Verified Facts

- `cs_dgp_registry()` contains 24 rows across 12 distinct `dgp_id` values, and
  `type = rep("synthetic", 24)` (`R/cs-dgp-registry.R:10-63`). There are no
  registered `type = "real"` rows.
- I executed every registered generator in a temporary R process with `n = 12`
  and truth construction disabled. All 24 returned exactly consecutive,
  one-based uppercase covariate names: `X1...X5`, `X1...X4`, `X1...X50`, or
  `X1...X100`, as appropriate. No lowercase, gapped, bare-`X`, latent, or other
  non-core feature names appeared.
- Static inspection agrees with execution. Eleven DGP IDs construct explicit
  uppercase columns, including baseline (`R/dgp-synth-baseline.R:70-74,
  145-149`), nonlinear heteroskedastic (`R/dgp-synth-nonlinear-heteroskedastic.R:
  77-80,158-161,239-242,317-320`), and Kang–Schafer
  (`R/dgp-synth-placebo-kangschafer.R:70-80`). The high-dimensional generator
  uses `paste0("X", seq_len(p_hd))` in all three versions
  (`R/dgp-synth-hd-sparse-plm.R:43,120,194`) before binding those columns
  (`:75,152,226`). Kang–Schafer's latent `Z1...Z4` are generated at lines 40-43
  but are not returned in `df`.
- The uppercase implementation predates the lowercase constitutional clause.
  Git history shows uppercase baseline output in commit `224ba24` (2025-11-29),
  while the current lowercase clause was introduced by `06fe90e` (2025-12-01).
  This is strong evidence of a transcription defect rather than a new scientific
  naming policy.
- Constitution v2.0.0 nevertheless literally requires lowercase `x1...xk`
  (`inst/design/CAUSAL_STRESS_CONSTITUTION.md:160`). It also declares itself
  supreme over code (`:19-20`) and directs strict interpretation (`:34`).
- No covariate-name validator exists. `cs_check_dgp_synthetic()` checks core
  columns but not feature names (`R/cs-contracts.R:518-676`);
  `cs_validate_dgp_registry()` checks `y0`, `y1`, `p`, and `structural_te`
  (`R/cs-validate-registry.R:160-173`); and `cs_validate_dgp()` requires only
  `y` and `w` before its truth checks (`R/cs-validate-dgp.R:22-50`). A direct
  contract probe confirmed that a structurally valid DGP with lowercase `x1`
  currently passes `cs_check_dgp_synthetic()`.
- Estimator discovery is heterogeneous. `est_lm_att` and `est_ipw_att` require an
  uppercase `X` prefix (`R/est-lm-att.R:43`, `R/est-ipw-att.R:42`). GRF, BART,
  GenGC, GenGC-DR, and TMLE use exclusion-based discovery
  (`R/est-grf-dr.R:55`, `R/est-bart.R:92`, `R/est-gengc.R:44`,
  `R/est-gengc-dr.R:53`, `R/est-tmle.R:55,107`).
- Existing tests demonstrate uppercase usage but do not comprehensively enforce
  the constitutional predicate across all versions. Examples include
  `tests/testthat/test-dgp-baseline.R:15-16`,
  `tests/testthat/test-dgp-heavytail.R:14-15`, and
  `tests/testthat/test-dgp-hd-sparse-plm.R:8-9`.
- The prose term and discriminator are separable. The validator admits the
  machine values `synthetic` and `real` (`R/cs-validate-registry.R:27`), while
  the Constitution uses human-facing “Real DGP(s)” at lines 49, 90, and 170.
- The proposed RFC performs no generator, truth, RNG, oracle, fingerprint,
  discriminator, or campaign-artifact mutation.

## Grade Adjudication (patch vs major)

**CONFIRM: patch v2.0.1.**

The case-sensitive identifier change is formally meaningful, so the literalist
counter-argument is not a straw man. A DGP returning lowercase `x1` satisfies
the current sentence as written; changing the sentence alters which names the
contract admits. The synthesis represents that argument fairly at
`20260722_covariate_naming_synthesis.md:97-105`.

The countervailing evidence is stronger:

- uppercase generator behavior predates the lowercase clause;
- all 24 released package-managed versions use uppercase;
- existing tests and two built-in estimators assume uppercase;
- no scientific, truth, RNG, or artifact behavior changes; and
- the Preamble expressly allows patch revisions to clarify intent and tighten
  definitions (`CAUSAL_STRESS_CONSTITUTION.md:15-17`).

The proper patch justification is therefore **correction of a transcription
defect plus a bounded forward clarification**, not the claim that code and tests
outrank the Constitution. The amendment record should say that the intended
scientific contract and all released behavior are unchanged. It should avoid
the absolute phrase “no semantic change,” because the formal set of admitted
column-name strings does change.

## Findings

### MAJOR-1 — The patch argument reverses the authority order

- **Claim:** The synthesis says a hypothetical lowercase-`x1` DGP “was never
  actually conformant” because shipped estimators and tests would reject it
  (`20260722_covariate_naming_synthesis.md:90-91`).
- **Evidence:** The Constitution says it is supreme and violating code must be
  rejected (`CAUSAL_STRESS_CONSTITUTION.md:19-20`). Its current contract
  literally says `x1...xk` (`:160`). The current internal validator also accepts
  lowercase `x1`.
- **Why it matters:** Code/test behavior is evidence of intended wording, but it
  cannot redefine constitutional conformity. Leaving this argument in place
  weakens the governance model the amendment is supposed to preserve.
- **Fix:** Delete that sentence. Replace it with the historical evidence that
  uppercase implementation predates the lowercase clause and that the patch
  corrects the constitutional transcription while preserving released
  behavior.

### MAJOR-2 — The proposed constitutional bullet over-specifies reserved names

- **Claim:** The synthesis adds “no gaps, no duplicates,” reserved-column
  disjointness, and Runner-issued keys “such as `unit_id`” to §3.2.A
  (`20260722_covariate_naming_synthesis.md:132-136`).
- **Evidence:** `unit_id` is a Runner-issued held-out prediction key under the
  CATE estimator contract (`CAUSAL_STRESS_CONSTITUTION.md:142,150`) and held-out
  sample identity (`:132`), not a required output of the synthetic generation
  `df` in §3.2.A (`:158-168`). No authoritative reserved-column vocabulary
  currently exists; the airlock has only a purpose-specific drop list
  (`R/cs-airlock.R:50`).
- **Why it matters:** The casing decision does not require constitutionalizing
  an open-ended set of current and future Runner keys. “Consecutive” already
  entails no gaps; unique column names and reserved-name handling are operational
  validator concerns. The parenthetical mixes generation, scoring, and held-out
  prediction layers.
- **Fix:** Keep uppercase, consecutive, one-based `X1...Xk` in §3.2.A. Remove
  “no gaps, no duplicates,” the reserved-column parenthetical, and `unit_id`.
  Let the correction packet define an operational core/non-feature list without
  elevating that mutable list into this amendment.

### MAJOR-3 — Enforcement must block the v0.2.0 release

- **Claim:** The synthesis authorizes a validator in a “later” packet and says
  not to implement it in this cycle (`20260722_covariate_naming_synthesis.md:
  118-119,162-180`).
- **Evidence:** None of the three validation surfaces checks covariate names, and
  Article VII requires constitutional compliance to be enforced continuously by
  automated tests (`CAUSAL_STRESS_CONSTITUTION.md:448`).
- **Why it matters:** A ratified rule with no release-bound enforcement repeats
  the defect this RFC is correcting. Staged implementation can follow
  ratification, but public v0.2.0 must not ship before it lands.
- **Fix:** State that implementation is outside the RFC document itself but is a
  release-blocking item in the narrow v0.2.0 correction packet. Require one
  shared predicate used by internal contract validation, all-version registry
  validation, and public DGP certification, with positive and negative tests.

### MINOR-1 — Claude's MAJOR-2 finding has correct facts but excessive severity

- **Claim:** The response treats heterogeneous estimator discovery and future
  real-data brittleness as MAJOR.
- **Evidence:** The seed said “several” estimators use `X*`, which is factually
  correct: two do. The uppercase amendment is limited to synthetic DGPs, while
  real-data covariates remain unconstrained.
- **Why it matters:** The observation is a useful deferred compatibility risk,
  but it does not change this amendment's decision or text. Future real-data DGPs
  may, rather than necessarily will, retain empirical feature names.
- **Fix:** Retain the risk but grade it MINOR or NOTE. Route it to the future
  real-data RFC; do not make it a ratification condition.

### MINOR-2 — The amendment-history language is internally inconsistent

- **Claim:** The synthesis calls the patch “notation-only” and “no semantic
  change,” then describes a “forward-only definitional tightening”
  (`20260722_covariate_naming_synthesis.md:35,145-155`).
- **Evidence:** Case-sensitive names change literal conformance, and the proposed
  consecutiveness language intentionally narrows future synthetic DGPs.
- **Why it matters:** The history should accurately state why the change remains
  patch-grade without pretending the formal text is unchanged.
- **Fix:** Use the shorter amendment entry proposed below. Describe no change to
  intended scientific meaning or released behavior, rather than an unqualified
  absence of semantic change.

### MINOR-3 — Historical preservation is correct but its cited prohibition is overstated

- **Claim:** The synthesis says editing the 2.0.0 amendment-history entry is
  “forbidden” (`20260722_covariate_naming_synthesis.md:193`).
- **Evidence:** The governance index explicitly requires verbatim preservation
  for archived records and versioned correction for scientific registry specs
  (`inst/design/README.md:27,65,70-76`). It does not state a general textual ban
  covering every prior Constitution history sentence.
- **Why it matters:** The recommendation is right, but the stated authority is
  broader than the cited rule.
- **Fix:** Preserve the v2.0.0 entry and historical RFCs for audit integrity; do
  not claim a broader written prohibition than the repository contains.

### NOTE-1 — Option B is deferred, not eliminated

Rejecting arbitrary registry-declared synthetic covariate names for this patch
is reasonable. The maintainer-gated DGP process (`CAUSAL_STRESS_CONSTITUTION.md:
38-39`) bounds the immediate cost. However, the planned user-defined DGP
extension may eventually benefit from semantic feature names. After v2.0.1,
relaxing the uppercase synthetic contract would require deliberate
constitutional work, likely a major-grade change. The synthesis should state
that cost instead of saying the constraint costs “nothing scientific.”

### NOTE-2 — Terminology and scoping are sound

I confirm `real-data DGP` as the clearer human term, limited to the three live
sites. Keeping `type = "real"` unchanged is correct. Coupling the two small
corrections in one v2.0.1 amendment is efficient so long as they are enumerated
separately. Existing historical RFCs and the v2.0.0 history entry should remain
unchanged.

### NOTE-3 — No runtime change is smuggled in

The synthesis does not itself modify implementation, generators, RNG, truth,
or fingerprints. Its detailed reserved-name predicate does, however, make an
unnecessary implementation-design decision; MAJOR-2 removes that excess while
preserving the required validator outcome.

## Assessment of the Synthesis's Adjudication

- **MAJOR-1 (correction + tightening): CONFIRM IN PART.** Calling the casing
  mismatch a correction is right. Explicit uppercase consecutiveness is a
  defensible clarification of `X1...Xk`. Reserved-column disjointness and
  `unit_id` go beyond the seed and should not be constitutionalized here.
- **MAJOR-2 (estimator heterogeneity): CONFIRM FACTS, DISPUTE SEVERITY.** Two
  estimators require `X*`; five inspected estimators are name-agnostic. This is
  a future real-data compatibility note, not a material amendment defect.
- **MAJOR-3 (no validator): CONFIRM AND STRENGTHEN.** The characterization is
  accurate. Enforcement must be explicitly required before the public v0.2.0
  tag, not merely assigned to an unspecified later packet.

The synthesis otherwise represents the seed fairly: it preserves Option A,
keeps real-data support deferred, does not rename released generators, and does
not alter runtime identity. The reserved-column clause is the only material
position added beyond what this seed author would accept.

## Proposed Constitutional Text — corrections

**Article III §3.2.A — replacement bullet:**

> - `df`: Tibble including `y`, `w`, `p`, `y0`, `y1`, and one or more
>   covariates named `X1`, ..., `Xk`, using uppercase `X` with consecutive
>   one-based integer suffixes.

This expresses the accepted decision without freezing a Runner-wide reserved
column vocabulary into the synthetic generation contract.

**Live prose normalization:**

- §1.3: use “real-data DGPs” in the Real Data clause.
- §1.7: rename the bullet to “Real-data DGPs.”
- §3.2.B: use “Real-data DGPs (`type="real"`).”
- Do not change the machine discriminator or historical v2.0.0/RFC wording.

**Replacement amendment-history entry:**

> - **v2.0.1 (Ratified <maintainer stamps date>):** Patch clarification of the
>   DGP contract. Corrects Article III §3.2.A's synthetic-covariate notation
>   from lowercase `x1...xk` to uppercase, consecutive, one-based `X1...Xk`,
>   matching the immutable outputs of all 24 released package-managed synthetic
>   DGP versions. The correction preserves the intended scientific meaning and
>   changes no released DGP implementation, generated data, truth, RNG stream,
>   fingerprint, estimator result, or campaign evidence. It also normalizes the
>   human-facing term `Real DGP` to `real-data DGP` in the three live contract
>   sites; the machine discriminator `type = "real"` is unchanged and real-data
>   support remains deferred. Historical amendment and RFC records are preserved
>   verbatim. The bump is *patch*.

## Open Risks the Review Adds

- **Future synthetic extension API:** uppercase `X1...Xk` becomes a deliberate
  v2.x constraint for user-defined synthetic DGPs. If semantic names later prove
  scientifically or ergonomically important, relaxing it will require explicit
  constitutional reconsideration rather than a routine API tweak.
- **Feature roster remains implicit:** prefix-based and exclusion-based
  estimators do not share one authoritative covariate roster. The future
  real-data RFC should prefer a Runner-supplied feature roster over either
  naming heuristic.
- **Reserved names belong to operational design:** the correction packet needs a
  single internal definition of core/truth/Runner fields for validation, but
  that list should not be embedded in this narrow constitutional casing patch.
- **Enforcement sequencing:** ratification may precede implementation, but the
  validator and tests must land before v0.2.0 is tagged.

## Recommendation to Maintainer

Do not ratify the synthesis verbatim. Require the three MAJOR corrections above,
then accept the revised synthesis as Constitution v2.0.1 without reopening the
full RFC cycle:

1. preserve constitutional supremacy in the patch rationale;
2. use the minimal §3.2.A wording proposed here;
3. make validator enforcement a v0.2.0 release blocker;
4. retain the three live `real-data DGP` prose changes and unchanged
   `type = "real"`; and
5. keep real-data implementation, arbitrary synthetic feature names, and
   estimator-roster redesign deferred.

No generator version, runtime artifact, or fingerprint migration is warranted.

---

*End of review. Advisory and non-binding; no ratification performed.*
