# CausalStress — Scientific Design Review: Human-Review Packet

**For a causal-inference expert, ~1 hour.** HEAD `9c16cd2`. Core ≤1500 words;
appendix indexes runnable checks. Findings are advisory — routed via action
classes, never applied.

## 1. The claim, and its scope

CausalStress claims to *identify where causal estimators succeed, degrade, and
fail under controlled synthetic stress, without misleading comparisons across
estimands, populations, failures, or DGPs.* This review adjudicates two tiers
above contract compliance (assumed established): **scientifically valid** and
**externally representative**.

**Scope statement (the one place generic limits appear).** Every finding is
conditional on synthetic data: a synthetic benchmark bounds external validity by
construction, and all twelve DGPs are *selection-on-observables* — treatment is
unconfounded given the recorded X. The instrument measures estimator behavior
**assuming identification holds**; it cannot, by design, speak to robustness under
unmeasured confounding, measurement error, missingness, or dependent units.
"Where estimators fail" means *within correct identification*.

## 2. Assumptions the claim requires

1. Truth is estimator-independent and exactly computable (structural τ(X)
   deterministic in X; QST via N=10⁶ oracle). **Verified** (§3, C1–C2).
2. Each output is scored only against its own target's truth. **Verified** (C1).
3. Each DGP isolates an interpretable stress mechanism. **Partly false** (§3,
   C3: multi-axis coupling; mislabeled severity).
4. Aggregates over seeds/DGPs are honest — stable, uncertainty-quantified,
   survivorship-safe. **Not yet** (C4; source S3/S4).
5. The placebo gatekeeper measures hallucination, not caution. **Verified** (C5).

## 3. What the checks establish

| Check | Result | What it rules out / establishes |
|---|---|---|
| **C1** typed scoring (`lane1_check1`, 5 seeds, all PASS) | oracle error `0.0` vs ATT & ATE truth; truths bitwise-recomputed; ATT≠ATE gap ≈0.11; unproduced/unimplemented → `non_comparable`+NA; airlock strips truth cols | **Rules out** silent cross-scoring / population confusion at the atomic score row. Core positive result. |
| **C2** QST oracle (`lane1_check2`) | recompute bitwise-identical; agrees with independent MC ≤1.4 SE; **tail MC-SE ≈4.2×10⁻³** at τ=0.01 | Truth is deterministic & valid; but QST **tail** truth carries ~10⁻³ noise, not the "<10⁻⁵" §1.4 implies. |
| **C3** DGP pathology (`lane2_pathology_v2`) | placebos exact (y0≡y1, truth 0); naive-bias ladder separates (0.13→3.67→−19.7); overlap ESS ratio 0.0097; **σ(X)–p coupling −0.65**; **tilt_mild sd(logit p) 0.607 < baseline 0.715**; **hd_sparse sd(y1−y0−τ)=0** | Placebos & difficulty are real; but attribution to *one* named axis fails for `nonlinear_heteroskedastic`, `tilt_mild` is milder than baseline, and `hd_sparse_plm` shares within-unit noise (Registry §4 line 48 mandates independence) → degenerate flat QST. |
| **C4** ranking stability (`lane3_statistics` §C) | `baseline` lm<ipw stable; **`heavytail` ordering flips** across seed sets (0.60→2.16); Spearman 0.80 | Reported rankings on heavy-tailed DGPs can **invert under reseeding** at n=400/15 seeds. `cs_summarise_runs` reports per-cell `sd_error` (dispersion) but **no** SE-of-mean/CI or rank-stability signal (`cs-summary.R:62`), so nothing flags a within-noise ranking. |
| **C5** gatekeeper (`cs-gatekeeper.R:40-54,128-158` + `lane3` §B) | CI-less → **UNVERIFIED** (NaN hazard fixed); 10/10 rule matches §4.2.2; conservative wide-CI → PASS | Gate screens hallucination and does **not** punish caution. "Passed" ≠ calibrated/robust. |

Two agent-written checks were **defective** and demoted (checks outrank opinions,
so a broken check supports nothing): the C2 τ=0.95 "FAIL" was a floating-point
`match()` artifact, not an oracle gap; the survivorship probe misused the runner
API — so the survivorship finding rests on source + the project's own deferral,
with a corrected harness listed as a deferred experiment.

## 4. Open definitional questions (governing clause cited)

- **QST tier asymmetry.** Truth is superpopulation (oracle over the law of X∣W=1;
  Const. §1.4), estimator curve is finite-sample. At small n a portion of measured
  QST "error" is irreducible sampling noise. Coherent but under-disclosed —
  document, and quantify the floor (experiment 3).
- **Noise independence vs. constant-shift DGPs.** Registry 1.4.0 §4 (line 48):
  "ε₀, ε₁ are independent draws." `hd_sparse_plm` uses ε₁≡ε₀ (constant +1 shift).
  Either restore independence (new version) or add an explicit Registry carve-out;
  either way its QST is non-informative and should be reported as such.
- **"Stable" semantics.** `cs-dgp-registry.R` marks only 2 of 12 DGPs stable; 10
  are "Pending human validation." Does "stable" certify data/truth reproducibility
  only, or also *estimator-comparison* stability? C4 shows the two differ (the
  stable `heavytail` is where rankings are least stable).

## 5. Value decisions reserved for the maintainer (options + trade-offs)

- **Families vs. CATE next (the headline v0.3.0 choice).**
  *Families first* — highest scientific value: turns single-point comparisons into
  interpretable breakdown/kill-plot curves and directly fixes the C3
  axis-attribution problem (one governed dial at a time); but inherits the
  aggregation gaps (C4/survivorship) unless those are fixed first.
  *CATE first* — extends to unit-level methods; design is coherent and
  near-implementable (one open RFC decision on the heterogeneity-detection test);
  but adds a second scoring surface (held-out PEHE) onto the same
  not-yet-survivorship-honest aggregation layer.
- **Should "stable" require comparison-stability**, not just reproducible
  data/truth? (Raises the bar; would currently de-stabilize `heavytail`.)
- **Extend scope to identification-failure DGPs?** Widens external relevance but
  requires *new estimand truth tiers* (the current structural definitions are
  incoherent to score under unconfoundedness failure), i.e. constitutional work.

The review takes **no position** on these.

## 6. v0.3.0 recommendation and flip conditions

**Recommendation: foundational scientific repairs first** — before families or
CATE, land (a) aggregate Monte-Carlo uncertainty + minimum-seed guidance and
(b) survivorship-honest denominators, plus the narrow DGP corrections (axis
labeling; `hd_sparse` independence). Rationale: both families' kill-plots and
CATE's PEHE tables consume the aggregation/uncertainty layer; shipping either on
seed-unstable aggregates that report dispersion but no mean-uncertainty/
rank-stability signal, and are survivorship-blind, would propagate C4/S4 into the
very curves meant to be the scientific payoff.

**Flip to families-first** if experiments 1–2 show rankings are already stable and
survivorship-honest at realistic seed counts (the foundational blocker dissolves;
families then deliver the most value). **Flip to CATE-first** if the maintainer's
priority is unit-level methods *and* experiment 2 clears survivorship for the
held-out path. **Bounded-parallel** only if experiments 1–2 pass and S5/S6 are
ticketed.

## 7. Index of runnable checks (`inst/design/audit/checks/`)

| Script | Purpose | Cost |
|---|---|---|
| `lane1_check1_typed_scoring.R` (+`_output.txt`) | oracle exactness, no-cross-scoring, airlock | ~1 min · RUN, all PASS |
| `lane1_check2_qst_truth_recompute.R` (+`_output.txt`) | QST oracle determinism, validity, tail MC-SE | ~4 min · RUN (τ=0.95 lookup artifact noted) |
| `lane1_check3_qst_noise_floor.R` | QST finite-sample floor | DEFERRED (needs `include_truth=TRUE` rerun, ~3 min) |
| `lane2_pathology_checks.R` (original, agent) | pathology/axis/placebo | aborts on `include_truth=FALSE` bug |
| `lane2_pathology_v2.R` (+`_output.txt`) | corrected: 12-DGP pathology, coupling, shared-noise, include_truth probe | ~2 min · RUN |
| `lane3_statistics.R` (+`_output.txt`) | survivorship (defective), gatekeeper-vs-caution, ranking stability | ~3 min · RUN |

**Deferred experiments (synthesis §"Cannot decide"):** reseeded ranking+MC-SE
(~20 min); corrected survivorship harness (~10 min); QST noise-floor (~3 min);
hd_sparse QST non-informativeness (~5 min); CI coverage calibration (~30–45 min).

**Review limitation:** Phase-2/3 reviewer contexts were **not** genuinely isolated
— the isolated agents were credit-terminated after writing their checks and the
orchestrator completed the lanes and rebuttals. Empirical findings rest on
rerunnable seeded scripts; the adversarial-independence guarantee is weakened and
the Phase-5 meta-review (fresh agent, repo access) is the compensating control.
