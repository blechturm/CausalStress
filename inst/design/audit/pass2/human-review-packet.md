# CausalStress — Scientific Design Review (PASS 2): Human-Review Packet

**For a causal-inference expert, ~1 hour.** HEAD `9c16cd2`. Core ≤1500 words.
Pass 2 restored genuine reviewer isolation (3 independent sidecar-fed agents) and
required **intent-first** classification: behavior fulfilling a DGP's declared
scientific purpose is *sound*, not a defect. It corrects two headline errors from
pass 1 that came from not reading the DGP sidecars/narratives.

> **Maintainer adjudication (2026-07-26): REQUEST CHANGES — accepted.** Authoritative
> dispositions in `maintainer-adjudication.md`. Read this packet against: (1)
> `synth_heavytail` is an **intentional estimand-boundary DGP** — its "structural
> ATT" is the governed **signal anchor**, not a conventional mean potential-outcome
> ATT (which does not exist under Cauchy noise); **no demotion** (the pathology is
> Paper 3's result); do **not** use it for an ATT shootout — use QST. (2) The
> metric-regime guard is an **aggregation-layer** concern: atomic ATT `point_error`
> rows stay **scored** (they are the breakdown evidence); suppressing them is
> **rejected**. (3) The **QST oracle tail MC-SE (~4×10⁻³)** finding is **carried**
> (pass-2's "oracle error = 0" is scorer consistency, not truth accuracy); "CRN
> eliminates MC noise" is a doc defect. (4) "verified" ⇒ "declared signatures
> reproduced"; 10 DGPs remain experimental.

## 1. The claim, and its scope

CausalStress claims to *identify where estimators succeed, degrade, and fail under
controlled synthetic stress, without misleading comparisons across estimands,
populations, failures, or DGPs.* Above contract compliance (out of scope), two
tiers: **scientifically valid** and **externally representative**.

**Scope statement (the one place generic limits appear).** All truth is
model-internal; oracle correctness does not establish external representativeness.
All 12 DGPs are **selection-on-observables** (unconfoundedness + SUTVA by
construction) with Gaussian/uniform/Bernoulli covariates — so "where estimators
fail" means *within correct identification*, at finite benchmark n.

## 2. Assumptions the claim requires

1. Truth is estimator-independent and exactly computable. **Verified** (C1).
2. Each output scored only against its own target's truth. **Verified** (C1).
3. Each DGP delivers its declared stress mechanism. **Verified for all 12** (C3).
4. The user is steered to a **well-posed** estimand for each DGP's moment regime
   (mean-based metrics need finite variance). **Prose-only, not enforced** (C2).
5. Aggregates are stable, uncertainty-signalled, survivorship-safe. **Stable where
   a mean exists; dispersion reported; failure denominators not yet** (C4).

## 3. What the checks establish

| Check (`pass2/checks/`) | Result | Establishes / rules out |
|---|---|---|
| **C1** typed scoring, reconfirmed | oracle ATT/ATE error `0.0` on baseline **and** heavytail; unproduced/unimplemented → `non_comparable`+NA; airlock strips truth cols | No silent cross-scoring; truth exact even on the heavy-tail DGP. |
| **C2** metric-regime (`p2_lane1`/`p2_lane3_metric_regime`) | mean-ATT on no-mean heavytail scored `"scored"`, reason `NA`, structurally identical to Gaussian baseline; `metric_invalid_for_regime` occurs **once** in `R/` (its definition), emitted nowhere | The "use QST on heavy tails" steer is **prose-only** — no machine guardrail stops a naive mean-ATT ranking. |
| **C3** 12-DGP intent (`p2_lane2_dgp_intent`) | all 12 pathology signatures reproduced; heavytail mean-ATT sd 9.13→1.88→9.62 (non-shrinking) while its ATT/QST truth stay well-posed; hd_sparse `y1==y0+1` exact; 5 placebos exact sharp null | Every DGP delivers its declared stress; heavytail & hd_sparse behaviors are **intended**, not defects. |
| **C4** ranking + survivorship + gatekeeper (`p2_lane3_*`) | `baseline` ordering **stable** across disjoint seeds; `heavytail` flips (intended); `cs_run_grid` 4/8 failures → `n_runs=8`, no failure denominator; CI-less → **UNVERIFIED** | Rankings are stable where a mean is well-posed; gatekeeper doesn't punish caution; survivorship denominators are missing (self-deferred). |

**Two pass-1 errors corrected here** (both from sidecar-blindness): the heavy-tail
ranking instability is the DGP's **intended "L2 break"** (Cauchy mixture → no mean →
sample mean can't converge; the ATT *truth* stays well-posed — only the mean
*estimator* is ill-posed); and hd_sparse's constant-shift shared noise is its
**declared design**, immaterial to every scored truth.

## 4. Open definitional questions (governing clause cited)

- **Metric regime is prose-only.** The Families spec designs `moment_regime$rmse_valid`
  and `cs_summarise_stress` (Art VI/VIII), and `metric_invalid_for_regime` exists in
  the non-comparable vocabulary (cs-contracts.R:103) — but point-DGP scoring
  (`cs_build_score_surface`) never emits it. Const. §1.3 bars sample-mean truth in
  heavy tails but does not gate a mean *estimator's* scoring. **The tool knows, in
  prose, what it does not enforce in code.**
- **Registry 1.4.0 is stale.** Four DGP specs in the frozen doc disagree with the
  live sidecars/code (overlap 3→9, hd_sparse p50→100/ρ0.5→0.95, …). Sidecars +
  narratives are authoritative; the Registry should be bumped or stamped superseded.
- **Moment regime is invisible to metadata.** heavytail (no variance) and qte1
  (Student-t₄, finite variance) both carry `noise:"heavy"`, `target:"both"` — the
  data model can't distinguish "breaks L2" from "merely heavy."

## 5. Value decisions reserved for the maintainer (options + trade-offs)

- **Enforce metric-regime now, or defer to families?** *Enforce now* — wire
  `metric_invalid_for_regime` (or a regime caveat in `cs_summarise_runs`) so a
  naive user can't rank a no-mean mean-ATT; small, closes the one live guardrail.
  *Defer to families* — the machinery is already designed there; point DGPs stay
  simple. Either is defensible; the current maintainer-only, prose-documented
  state is not dangerous, just unguarded — but note the no-mean DGP in question
  (`synth_heavytail`) is **stable/recommended**, not experimental (meta-review D1),
  so the gap sits on a first-class DGP, which argues for enforcing over deferring.
- **Families vs. CATE next.** *Families first* now also **closes the metric-regime
  gap** (its `moment_regime` machinery is exactly the fix) and yields interpretable
  breakdown curves — strongest value. *CATE first* extends to unit-level methods;
  design-ready save one RFC decision (heterogeneity-detection test).
- **Extend scope to hidden-bias / interference DGPs?** Widens external relevance but
  needs new estimand truth tiers (constitutional work).

The review takes **no position** on these.

## 6. v0.3.0 recommendation and flip conditions

**Recommendation: families first** (reframed from pass 1's "repairs first"). Pass 1
recommended foundational repairs on the strength of findings this pass **withdrew
or corrected** — the ranking layer is stable where a mean exists, `sd_error` is
already reported, and the single live gap (metric-regime enforcement) is **precisely
what the parameterized-family machinery already designs**. So families-first is not
building on shaky foundations; it is the vehicle that *closes* the surviving finding
while delivering the highest-value output (kill-plot curves). Do first, cheaply, as
part of that program: the survivorship denominators (S2) and the documentation/
metadata fixes (S4/S5/S6).

**Flip to foundational-repairs-first** only if the deferred CI-coverage experiment
exposes a *calibration* defect on a finite-variance DGP (none found this pass) — a
genuine correctness problem would outrank families. **Flip to CATE-first** if the
maintainer's priority is unit-level methods.

## 7. Index of runnable checks (`inst/design/audit/pass2/checks/`)

| Script | Purpose | Cost |
|---|---|---|
| `p2_lane1_metric_regime.R` (+`.txt`) | is mean-ATT on a no-mean DGP machine-flagged? | ~2 min · RUN, all PASS |
| `p2_lane1_reconfirm_typed_scoring.txt` | re-run of pass-1 oracle/no-cross-scoring/airlock | RUN, all PASS |
| `p2_lane2_dgp_intent.R` (+`.txt`) | 12-DGP intent verification; heavytail non-convergence; hd_sparse shift; placebos | ~3 min · RUN, 7/7 PASS |
| `p2_lane3_metric_regime.R` (+`.txt`) | regime enforcement + uncertainty-column inventory | ~2 min · RUN |
| `p2_lane3_ranking.R` (+`.txt`) | ranking stability: baseline vs heavytail | ~2 min · RUN |
| `p2_lane3_survivorship.R` (+`.txt`) | failure denominators via `cs_run_grid` (pass-1 probe rebuilt) | ~2 min · RUN |
| `p2_lane3_gatekeeper.R` (+`.txt`) | CI-less→UNVERIFIED, conservative→PASS, 10/10 rule | ~2 min · RUN |
| (salvaged) `../checks/lane1_check1`, `lane2_pathology_v2`, `lane3_statistics` | pass-1 checks reused/corrected | — |

**Deferred experiments:** CI coverage calibration (~30-45 min); difficulty-star
empirical calibration (~20 min).

**Process note.** Phase 2 (the substantive lane reviews) was genuinely isolated
this pass — three independent sidecar-fed agents that **converged**, which is the
strongest evidence the intent-aware reading is correct. Phase 3 (adversarial) was
orchestrator-run; the fresh Phase-5 meta-review (repo access) is the independent
control and is charged specifically with catching any surviving intent-vs-defect
misclassification.
