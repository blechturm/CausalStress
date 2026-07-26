# Phase 2 — Lane 1: Causal Estimands & Identification

**Authorship note:** the isolated Phase-2 agents were terminated by an
infrastructure credit limit (prior model) after writing their check scripts;
the orchestrator completed the lane reports and ran the checks. Isolation was
therefore compromised at Phase 2 — recorded as a review limitation. Evidence
discipline (labeled claims; checks outrank opinions) is preserved: every
empirical claim below cites a runnable, seeded script under `checks/` and its
captured output.

## 1. Reconstructed claim (carried from Phase 1)

CausalStress supports: *for a registered `(dgp_id, version)`, estimator E — given
only airlock-sanitized data — attains measured error/coverage against exactly
known, versioned, typed truth for a specific estimand, with each output scored
only against its own target's truth (no cross-scoring).* Carried unchanged; the
one refinement is empirical confirmation of the **finite-sample ATT vs. ATE**
distinction and the **QST superpopulation-vs-sample tier asymmetry** (below).

## 2. What is scientifically sound

- **Oracle exactness + no-cross-scoring + airlock** — the load-bearing "no
  misleading comparisons across estimands/populations" claim. `lane1_check1`
  (5 seeds, ALL PASS, `checks/lane1_check1_typed_scoring_output.txt`):
  `est_oracle_att` error `0.000e+00` vs recorded ATT **and** ATE truth; ATT truth
  bitwise-equals an independent treated-mean of `structural_te`, ATE truth the
  full-sample mean; the two differ (finite-sample gap ≈ +0.11 every seed) so ATT
  and ATE truth **cannot be silently interchanged**; unproduced targets (QST for
  `oracle_att`) and unimplemented targets (CATE) return `non_comparable` rows
  with `NA` value fields — never a cross-scored number; CATE-only tasks hard-abort
  with `causalstress_target_not_implemented_error`; the airlock frame drops
  `y0/y1/p/structural_te`. Const. §1.3/§1.5/§1.7; `R/cs-contracts.R`
  `cs_build_score_surface`. **EMPIRICAL, Tier 2 (valid): sound.**
- **QST oracle is deterministic and truth-valid.** `lane1_check2`
  (`checks/lane1_check2_qst_truth_recompute_output.txt`): recomputing the
  `synth_baseline` oracle after cache deletion is **bitwise-identical**; the
  persisted oracle agrees with an independent non-CRN Monte Carlo within ≤1.4 SE
  at every τ that resolved. Confirms the CRN coupling used only in the oracle path
  is estimand-invariant for the marginal-quantile contrast. **EMPIRICAL: sound.**
- **Placebo sharp null is exact** (cross-lane, `lane2_pathology_v2` §B): all
  placebo QST truth is 0 at all 99 τ — the QST estimand is exactly zero-variance
  under the null, as Art. IV §4.1 requires. **EMPIRICAL: sound.**

## 3. Findings

**F1 — QST tail-truth precision is ~10⁻³, not the "<10⁻⁵" §1.4 implies.**
Claim challenged: Constitution §1.4 parenthetical "oracle … matching oracle
precision (`< 10⁻⁵`)". Type **EMPIRICAL** — `lane1_check2` estimates the oracle's
own Monte-Carlo SE at N=10⁶ by CRN replication: **4.2×10⁻³ at τ=0.01**, ~2×10⁻³ at
τ=0.99, ~6×10⁻⁴ mid-grid — two to three orders above 10⁻⁵. Tier threatened:
**scientifically valid** for QST *tail* comparisons (an estimator "error" of a few
×10⁻³ at τ=0.01 is at the truth's own noise floor). v0.3.0 bearing: argues for a
foundational documentation/repair pass before families lean on QST curves.
Action class: **documentation only** (clarify that `<10⁻⁵` is an analytic-agreement
tolerance, not the tail MC-SE) **+ future spec ticket** (expose QST truth MC-SE).

**F2 — QST tier asymmetry is real and under-documented.**
Claim: QST truth is superpopulation-level (oracle over the law of X∣W=1) while the
estimator's curve is computed on the finite run sample; §1.4/§1.7 do not flag that
at small n a portion of measured QST "error" is irreducible sampling noise, not
estimator deficiency. Type **DEFINITIONAL** (coherent but incompletely disclosed;
empirical shadow in `lane1_check3` is DEFERRED — the script aborts on
`heavytail_v160` via the `include_truth=FALSE` self-check bug, see DGP-lane F6;
rerun cost ≈2 min once generators accept data-only calls). Tier: **valid** at
small n. v0.3.0: minor. Action class: **documentation only**.

**F3 — CATE design is coherent and near-implementable; one scientific decision is open.**
Claim: the proposed CATE design (held-out second governed draw, `unit_id`-keyed
predictions, PEHE + robust companion, heterogeneity-**detection** gate) is
complete enough to implement without new scientific decisions. Type
**DEFINITIONAL** — Const. §1.6, §2.2 "Held-out Evaluation Samples", §3.1 CATE
predict-input airlock, RFC-1 synthesis §1.7 are mutually consistent and specify
truth (`meta$structural_te`), evaluation identity, and metric. The **one** open
decision is the heterogeneity-detection test itself (BLP/GATES/RATE named only
"e.g." in Const. §4.2.5), correctly deferred to the Gatekeeper-recalibration RFC.
Tier: **valid** (design). v0.3.0 bearing: **CATE is design-ready** save one RFC
decision — a genuine decision input for CATE-first vs families-first. Action
class: **RFC** (already routed).

## 4. No material finding

- **No-cross-scoring** is empirically airtight (F-suite above): the score surface
  is `requested ∩ produced ∩ truth-available`, and no path emits a value for a
  target lacking matching truth.
- **Finite-sample ATT/ATE convention** (§1.3/§1.5) is coherent and the two truths
  are numerically distinct and correctly separated — no ambiguity found.
- **Real-data external-truth clause** (§1.3, §1.7): coherent design; unimplemented
  and correctly deferred — nothing to challenge at the definitional level.
