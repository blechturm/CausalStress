# PASS 2 — Synthesis (sidecar-aware, intent-first)

**Git HEAD:** `9c16cd2` · **Date:** 2026-07-26 · Findings routed, never applied.
This pass restores genuine reviewer isolation in Phase 2 (three independent
sidecar-fed agents) and adds the mandatory rule that behavior fulfilling a DGP's
**declared scientific intent is SOUND, not a defect**. It supersedes pass 1's
scientific conclusions where they conflict; pass-1 files are retained as the
record (incl. their correction trail).

## Phase-1 divergence

The three docs-only reconstructions **converged strongly** on estimand
populations/targets and, critically, all three independently read the heavytail
Cauchy mixture as making mean-based estimation *deliberately* ill-posed with QST
as the intended robust target. Minor divergences (all folded in): the estimands
lane sharpened that the ATT/ATE **truth** stays well-posed (only the mean-estimator
is ill-posed); the statistics lane found `metric_invalid_for_regime` exists in the
vocabulary; two lanes flagged Registry 1.4.0 as stale. Convergence of three
isolated readers on the intent-aware reading is itself strong evidence it is
correct — and is exactly what pass 1 (single-author, sidecar-blind) lacked.

## Corrections to pass 1 (resolution hierarchy: checks + intent outrank prior opinion)

- **WITHDRAWN — "heavytail ranking instability = aggregation defect" (pass-1's
  headline "materially misaligned").** It is the DGP's **intended "L2 break"**
  demonstration: `0.8·N+0.2·Cauchy` has no finite mean, so the sample mean cannot
  converge (p2_lane2 S2: across-seed sd 9.13→1.88→9.62, non-shrinking, vs baseline
  0.105→0.035→0.024 ~1/√n; p2_lane3 ranking: baseline STABLE, heavytail flip).
  The DGP, its structural ATT truth, and its QST truth are all sound.
- **WITHDRAWN — "hd_sparse violates independence / degenerate QST = misaligned."**
  The constant +1 shift with shared noise is the **declared design** (Registry
  §2 l.237-239; narrative §4); it is immaterial to every scored truth (ATT=1, QST=+1
  identical under shared/independent ε; only the constitutionally-barred paired
  difference differs). SOUND-by-intent; residual is a Registry §1.2 doc-consistency
  note only.
- **CORRECTED — "no Monte-Carlo uncertainty column."** `cs_summarise_runs` reports
  `sd_error` (cs-summary.R:62). Only the SEM/CI/rank-stability *inferential* signal
  is absent.
- **CLEARED — "tilt_mild mislabeled" and "nonlinear_het coupling = defect."**
  tilt_mild is correctly milder than baseline (sidecar `overlap: mild`); the
  nonlinear_het multi-axis coupling is an intended combined stressor. Both were
  intent-blind pass-1 errors.

## Findings (7; correctly scoped, intent-aware)

**S1 — Metric-regime appropriateness is enforced nowhere at the machine level
(prose-only). → Sound with a documented boundary.** *(estimands F1 = statistics
F1 = dgp F4; the correctly-scoped successor to pass-1's withdrawn headline.)*
`metric_invalid_for_regime` is defined (cs-contracts.R:103) but emitted **nowhere**;
`cs_build_score_surface` has no regime branch; mean-ATT on the no-mean heavytail
DGP is scored `"scored"`, `non_comparable_reason=NA`, structurally identical to the
Gaussian baseline (|error| up to 38.6 across seeds). The Families spec already
designs the fix (`moment_regime$rmse_valid`, `cs_summarise_stress`) but point-DGP
scoring never adopted it. **Adversarial downgrade (Phase 3 A1):** not a correctness
defect — DGP *authorship* is maintainer-gated (Const. §1.2), the narrative steers to
QST, and the enforcement is v0.3.0-families-designed. **Maintainer re-scoping (see
`maintainer-adjudication.md` §2 — this supersedes the wording below):** the guard
belongs at the **AGGREGATION layer, NOT the atomic score row.** Atomic ATT
`point_error` rows on `synth_heavytail` MUST stay **scored** — the realized estimate
vs. the governed structural-signal truth *is* the breakdown evidence Paper 3 exists
to produce; marking them `non_comparable` (rejected) would erase it. "Invalid
comparison must not mean skipped execution." The invalid quantities are the
*aggregates* (mean/RMSE/SEM/ranking, which have no finite moments here); mark those
interpretation-limited and provide robust median/quantile summaries; reserve
`metric_invalid_for_regime` for the invalid aggregate metric, not the estimand
target; families state = `diagnostic_only`. Action: **families program** + a
`cs_summarise_runs()` roxygen warning **now**. Tier: valid (a missing aggregation
guardrail on a **stable/recommended** DGP, not a wrong number).
*[Corrected after Phase-5 meta-review (D1): an earlier draft claimed "heavy-tail DGPs
are experimental" as a downgrade leg — false. `synth_heavytail` v1.6.0 is `stable`
(Const. §7.4 "validated, correct, recommended"); only `synth_placebo_heavytail` is
experimental. The gap therefore bites on a **recommended** DGP, which raises S1's
priority; it remains a spec-decision (not a correctness defect) on the other three
legs, but the "only experimental" comfort is withdrawn.]*

**S2 — Aggregate summaries expose no failure denominator (survivorship). → Sound
with explicit limitations.** *(statistics F2, now empirically demonstrated via a
correctly-rebuilt probe — pass 1's was defective.)* `cs_run_grid` with 4 of 8 seeds
failing yields `n_runs=8`, no `n_success/n_failed`; error columns lack `na.rm` so a
failure NA-poisons the means (a crude tell, not a count). Self-deferred:
horizon.md:535-542, 308-311. Blocks no current claim. Action: **future spec
ticket** (already routed).

**S3 — Aggregates report dispersion (`sd_error`) but no SEM/CI/rank-stability
signal. → Sound with explicit limitations.** A user comparing two `mean_error`s has
no built-in way to judge whether a gap is within Monte-Carlo noise (SEM =
`sd_error/√n_runs` is one division away but unreported). Action: **future spec
ticket** (enhancement). horizon.md:285.

**S4 — Registry 1.4.0 is stale vs the authoritative sidecars/code. → Documentation
defect.** *(dgp F1.)* Four DGPs have wrong specs in the frozen doc (overlap 3→9,
hd_sparse p50→100/ρ0.5→0.95, nonlinear μ₀ form, placebo_tilted 1.0/1.2→0.6/0.8;
p2_lane2 S6). Sidecars/narratives are correct. Action: **documentation only** (bump
or stamp "superseded").

**S5 — Metadata cannot distinguish moment regimes. → Metadata defect.** *(dgp F4.)*
`synth_heavytail.yml` and `synth_qte1.yml` both tag `noise: "heavy"` and
`target: "both"`, though Student-t(4) has finite variance and the Cauchy mixture has
none — from the machine-readable sidecar a user cannot tell which breaks L2. Action:
**metadata-fix** (a `moment_order` / `att_mean_illposed` flag). This is the
data-model root of S1.

**S6 — hd_sparse independence-clause doc-consistency. → Documentation only.**
*(dgp F2.)* Registry §1.2 global "ε₀,ε₁ independent" isn't reconciled with its own
constant-shift hd_sparse block. Const. §1.4 already grants "unless explicitly
specified." Action: **documentation only** (add to the exception list).

**S7 — Whole suite is selection-on-observables. → Sound with explicit limitations
(scope, Tier 3).** *(dgp F5.)* No unmeasured confounding, measurement error, or
interference; conclusions about "where estimators fail" are valid *given
identification*. Action: **documentation** (covered-vs-uncovered taxonomy) + a
future hidden-bias/interference DGP family.

**S8 — QST oracle tail truth carries ~10⁻³ Monte-Carlo uncertainty (carried from
pass 1; pass 2 wrongly dropped it). → Sound with explicit limitations.** The N=10⁶
QST oracle's own MC-SE is ~**4.16×10⁻³ at τ=0.01** (`lane1_check2` output), not
<10⁻⁵. Pass-2's "oracle error = 0" (`lane1_check1`) is **scorer consistency**
(oracle estimator vs cached truth), **not** population-truth accuracy, and does not
supersede this. **No constitutional violation:** Const. §1.4 "<10⁻⁵" is the tolerance
for an analytic derivation matching the MC oracle, not a bound on the MC oracle's own
SE. **Real doc defect:** six v1.6.0 dossiers + `R/dgp-synth-heavytail.R:144` claim CRN
"eliminates Monte-Carlo noise" — CRN *reduces* the contrast's MC variance but cannot
eliminate empirical-quantile sampling uncertainty. Action: **documentation only**
(carry the finding + fix the wording; expose QST truth MCSE in future); no amendment.

*(CATE design coherence — estimands lane — carried: coherent and near-implementable,
one open RFC decision on the heterogeneity-detection test. Not re-listed as a
defect.)*

**Verification-language correction (maintainer §5):** claims that "all 12 DGPs are
verified / deliver their declared stress" are softened to *"the targeted declared
signatures were reproduced under fixed seed"* — signature reproduction, not full
scientific validation; 10 of 12 DGPs remain `experimental`.

## What is sound (the core of the claim holds)

Typed no-cross-scoring + oracle exactness + airlock (p2 reconfirmed, error 0 on
baseline and heavytail); the gatekeeper certifies coverage and refuses CI-less
estimators (UNVERIFIED, not NaN); each of the 12 DGPs **reproduced its declared
stress signature under fixed seed** (signature reproduction, not full validation —
10 remain experimental); placebos are exact sharp nulls; rankings are **stable where
a mean is well-posed**;
and the truth layer is well-posed on every DGP including heavytail. The instrument's
"no misleading comparisons" guarantee holds **structurally where it is designed to**.
The surviving gaps are documentation, metadata, and one designed-but-unwired
enforcement — **not scientific correctness.**

## Cannot decide without experiments (≤6)

1. **CI coverage calibration** (not run): nominal-95% bootstrap ATT CI coverage for
   lm_att/ipw_att on the two stable DGPs, 200 seeds, B=999. Cost ≈30-45 min.
2. **Difficulty-star empirical calibration**: do the author-assigned ★ track a
   reference estimator's error ordering across all 12 DGPs? Cost ≈20 min.

## v0.3.0 recommendation

**Families first** (reframed from pass 1). Pass-1's "foundational repairs first"
rested on findings that this pass **withdrew or corrected**: the ranking layer is
stable where a mean exists, `sd_error` is reported, and the one live gap (S1
metric-regime enforcement) is **precisely what the parameterized-family machinery
already designs** (`moment_regime$rmse_valid`). So families-first is not a bet on
top of shaky foundations — it is the vehicle that *closes* the surviving finding,
while delivering the interpretable breakdown/kill-plot curves that are the highest
scientific value. Prerequisites to fold into the families program (not blockers):
S2 survivorship denominators (needed before any survivorship-honest kill-plot), and
the S4/S5/S6 documentation/metadata fixes (cheap, do first).

**Flip conditions.**
- → **foundational-repairs-first** only if the CI-coverage experiment (1) reveals a
  *calibration* defect on a finite-variance DGP (none found this pass) — that would
  be a genuine correctness problem outranking families.
- → **CATE first** if the maintainer's priority (VALUE, not adjudicated) is
  unit-level methods; CATE is design-ready save one RFC decision.

**Value items reserved for the maintainer:** families vs. CATE priority; whether to
enforce metric-regime now vs. defer to families (S1); whether to extend scope to
hidden-bias/interference DGPs (S7).
