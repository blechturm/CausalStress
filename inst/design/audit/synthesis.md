# Synthesis — Scientific Design Review of CausalStress

**Git HEAD:** `9c16cd2d6a8915aef808bb3c865b8224778fa227` · **Date:** 2026-07-26
**Claim under review:** CausalStress's design identifies where estimators succeed,
degrade, and fail under controlled synthetic stress, without misleading
comparisons across estimands, populations, failures, or DGPs.
**Tier separation:** reproducible/contract-compliant ≠ scientifically valid ≠
externally representative.

**Process limitation (material):** the three isolated Phase-2/3 lane agents were
terminated by an infrastructure credit limit after authoring their check scripts;
the orchestrator completed the lane reports and Phase-3 rebuttals and ran the
checks. Reviewer contexts were therefore **not** genuinely isolated in Phases 2–3
(Phase 1 was). This weakens the adversarial-independence guarantee; it does not
weaken the empirical findings, which rest on rerunnable seeded scripts under
`checks/` whose outputs are captured and cited. The Phase-5 meta-review is a fresh,
independent agent with repository access.

## Phase-1 divergence (documentation signal)

The three docs-only reconstructions **converged** on estimand
populations/targets: ATT/ATE finite-sample structural (in-sample; ATE pinned to
the full run sample in Wave 1); QST distributional, treated-conditional, with
**superpopulation** oracle truth vs. a finite-sample estimator curve; CATE
unit-level conditional-mean on a held-out draw, `target_not_implemented`. All
three **independently** flagged the QST tier asymmetry and the deferred
survivorship denominators. Convergence on the hard points is evidence the
governance documents define the estimands coherently; the QST tier asymmetry was
salient enough that all three surfaced it unprompted — a documentation-clarity
signal, not a contradiction.

## Resolution hierarchy applied

1. **Checks outrank opinions.** Every finding below with an EMPIRICAL basis cites a
   captured check output. Two agent-authored checks were defective and are
   demoted: the `lane1_check2` τ=0.95 "FAIL" is a floating-point `match()`
   artifact (not an oracle gap); the `lane3` survivorship probe misused the runner
   API (supports no verdict — F7 rests on source + self-documented deferral).
2. Experiment-resolvable disagreements with unrun checks → "Cannot decide"
   experiments (≤6) below.
3. Definitional disagreements → citation coherence.
4. Value disagreements → maintainer, no position taken.

## Findings (graduated conclusions)

**S1 — Typed no-cross-scoring + oracle exactness + airlock: the core anti-misleading
guarantee holds at the atomic layer. → Sound for the stated scope.**
`lane1_check1` (5 seeds, all PASS): oracle error 0 vs recorded ATT and ATE truth;
truths independently recomputed and bitwise-equal; ATT≠ATE (finite-sample gap
≈0.11) so no silent interchange; unproduced/unimplemented targets yield
`non_comparable` rows with NA values; airlock strips truth columns. This is the
strongest positive result and directly supports the "no misleading comparisons
across estimands/populations" clause — **for a single scored row**.

**S2 — Placebo sharp null is exact; the gatekeeper measures hallucination without
punishing caution. → Sound for the stated scope.**
All five placebos: `identical(y0,y1)`, truth 0 at every τ (`lane2_pathology_v2`
§B). `cs-gatekeeper.R:40-54` maps CI-less estimators to UNVERIFIED (resolving the
v0.1.9 NaN hazard, reproduced in raw R by `lane3` §B); the 10/10 QST rule
(`:128-158`) matches Const. §4.2.2; a conservative wide-CI estimator PASSES
(`lane3` §B). Scope note (documentation): the gate is a one-sided hallucination
screen bounded by a selection-on-observables placebo suite — "passed" ≠
"well-calibrated" and ≠ "robust to unmeasured confounding."

**S3 — Estimator rankings are seed-unstable on heavy-tailed DGPs, and no aggregate
Monte-Carlo uncertainty is reported. → Materially misaligned with the intended
claim (aggregate layer).**
`lane3` §C: on `synth_heavytail` the lm/ipw ATT ordering flips across disjoint
seed sets (mean|error| 0.60→2.16, ≈3×; Spearman 0.80). `cs_summarise_runs` reports
per-cell dispersion `sd_error` (`cs-summary.R:62`) but **not** the
standard-error-of-the-mean, a CI on `mean_error`, or any rank-stability signal —
so a *reported ranking* on the harder DGPs can invert under reseeding with nothing
converting the reported spread into a within-noise warning. Tier 2. This remains
the single most important repair; scope it as "surface MC-SE/CI + rank-stability
guard," not "add MC uncertainty that is wholly absent."
*(Corrected after Phase-5 meta-review, which caught that the original "no SE/MC
uncertainty column" overstated the gap — `sd_error` is present. This is the one
detectable cost of the compromised Phase-2/3 isolation: a single-author blind spot
that rode uncorrected into the strongest negative finding. See meta-review.md.)*

**S4 — Aggregate summaries are not survivorship-honest (self-acknowledged). → Sound
with explicit limitations (atomic) / under-validated (aggregate).**
Per-task failures are captured atomically (`success=FALSE` rows; batch
reconciliation), but `cs_summarise_runs` exposes only `n_runs` and `na.rm` means
(`cs-summary.R:58-66`); horizon.md item 6 (~lines 308, 535-540) names this the
"survivorship gap" and defers it. Aggregate/kill-plot conclusions are not yet
survivorship-safe. (Empirical probe was defective — see resolution note.)

**S5 — Some stress DGPs couple multiple axes or carry mislabeled severity. →
Promising but scientifically under-validated.**
`nonlinear_heteroskedastic` couples nonlinearity+heteroskedasticity+selection
(cor σ,p=−0.65; cor μ₀,logit p=0.36, `lane2` §D) — failure is not attributable to
one named axis; `synth_tilt_mild` selection (sd logit p 0.607) is *weaker* than
the sanity baseline (0.715) (`lane2` §A,G). Per-DGP attribution of estimator
failure to a named mechanism is therefore partly unsupported — the central design
argument *for* parameterized families (one governed dial at a time).

**S6 — `hd_sparse_plm` violates the Registry's noise-independence rule and yields a
degenerate QST. → Materially misaligned (one DGP × QST cell).**
Registry 1.4.0 §4 line 48 mandates independent ε₀,ε₁ for non-placebo DGPs; `lane2`
§E shows `hd_sparse_plm` has sd(y1−y0−τ)=0 (shared ε), so its QST truth is flat +1
(Registry line 257) — QST discriminates nothing on this DGP. A verified
governing-clause contradiction plus a non-informative estimand cell.

**S7 — Ten of twelve DGPs are self-labeled scientifically unvalidated. → Sound with
explicit limitations.**
`cs-dgp-registry.R` status column + README disclaimer: only `baseline`,
`heavytail` are `stable`; the other ten are `experimental`/"Pending human
validation." The claim's breadth rests on DGPs the project has not signed off. Not
a defect — an accurate self-limitation that scopes tiers 2–3.

**S8 — QST tail-truth precision is ~10⁻³, not "<10⁻⁵". → Sound with explicit
limitations.**
`lane1_check2`: oracle MC-SE ≈4.2×10⁻³ at τ=0.01. QST *tail* comparisons at small
n / heavy tails sit at the truth's own noise floor; the §1.4 "<10⁻⁵" reads as an
analytic-agreement tolerance, not the tail MC-SE. Documentation + expose QST MC-SE.

**S9 — The suite spans stress *within* correct identification only. → Sound with
explicit limitations (scope, Tier 3).**
Every DGP is selection-on-observables; none has unmeasured confounding,
measurement error, missingness, or dependent units (`lane2` F5). Conclusions about
"where estimators fail" are valid *given identification*; external
representativeness is bounded accordingly. Extending scope requires new estimand
truth tiers, not just new DGPs (estimands A3).

**S10 — CATE design is coherent and near-implementable. → Promising; one RFC
decision open.**
Const. §1.6/§2.2/§3.1 + RFC-1 are mutually consistent (held-out draw, `unit_id`
airlock, PEHE + companion); the only open scientific choice is the
heterogeneity-detection test, already routed to the Gatekeeper-recalibration RFC.

## Cannot decide without the following experiments (≤6)

1. **Reseeded ranking + MC-SE study** (closes S3): `lane3_statistics.R` §C at
   100 seeds, B≥200 bootstrap, n∈{400,2000}, over the 2 stable × core estimators;
   report per-cell SE and rank-inversion frequency. Cost ≈20 min.
2. **Corrected survivorship harness** (closes S4 empirically): register a
   deterministic partial-failure estimator via `cs_register_estimator`, run through
   `cs_run_grid` (tibble path), and confirm whether failed tasks poison
   `mean_error` loudly or are dropped. Cost ≈10 min.
3. **QST finite-sample noise-floor** (closes S8/estimands F2): rerun
   `lane1_check3` with `include_truth=TRUE`; quantify sample-oracle abs-error vs
   frozen truth at n∈{500,2000} to bound the irreducible QST floor. Cost ≈3 min.
4. **hd_sparse QST non-informativeness** (confirms S6 consequence): score a correct
   location-shift estimator's QST on `hd_sparse_plm`; show abs_error is entirely
   finite-sample. Cost ≈5 min.
5. **CI coverage calibration** (not run this round): nominal-95% bootstrap ATT CI
   coverage for `lm_att`/`ipw_att` on the 2 stable DGPs, 200 seeds, B=999. Cost
   ≈30–45 min (DEFERRED).

## v0.3.0 recommendation

**Foundational scientific repairs first** — specifically S3 (aggregate MC
uncertainty + minimum-seed guidance) and S4 (survivorship-honest denominators),
plus the narrow corrections S5/S6 (axis labeling; `hd_sparse` independence). These
are prerequisites for *any* interpretable breakdown curve: parameterized families
(the strongest use case, per DGP S5) and CATE both consume the aggregation/
uncertainty layer, and shipping families or CATE on top of seed-unstable,
uncertainty-free, survivorship-blind aggregates would propagate S3/S4 into
kill-plots and PEHE tables.

**Flip conditions.**
- → **families first** if experiments 1–2 show aggregate rankings are already
  stable and survivorship-honest at realistic seed counts (S3/S4 dissolve), since
  families then deliver the highest scientific value (S5) with no foundational
  blocker.
- → **CATE first** if the maintainer's value priority (VALUE, not adjudicated
  here) is unit-level methods AND experiment 2 clears survivorship for the
  held-out scoring path; S10 shows CATE is design-ready save one RFC.
- → **bounded parallel program** only if experiments 1–2 pass *and* the S5/S6
  DGP corrections are ticketed, so families design proceeds while CATE's RFC
  decision resolves.

**Value items reserved for the maintainer (no position taken):** families vs.
CATE scientific priority; whether "stable" should require comparison-stability not
just data/truth reproducibility (statistics A3); whether to extend scope to
identification-failure DGPs (estimands A3 / DGP F5).
