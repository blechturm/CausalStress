# Phase 2 — Lane 3: Benchmark Statistics

**Authorship note:** the isolated Phase-2 statistics agent was credit-terminated
before writing any check; the orchestrator authored and ran `lane3_statistics.R`
and read the gatekeeper/summary source directly. Isolation compromised at Phase 2
— recorded limitation. Empirical claims cite
`checks/lane3_statistics_output.txt`; source claims cite file:line.

## 1. Reconstructed claim (carried)

"Estimator error against pre-registered, typed truth is reproducible and honestly
comparable," resting on typed no-cross-scoring, per-task failure accounting, CI
gating, and a per-estimand placebo gatekeeper. Carried unchanged; Phase-2 evidence
sharpens *where* "honestly comparable" currently holds (per-DGP, single-seed-set)
versus where it does not yet (cross-seed rankings, aggregate uncertainty,
survivorship-honest denominators).

## 2. What is scientifically sound

- **The gatekeeper measures hallucination without punishing caution, and the
  historical NaN hazard is fixed.** `R/cs-gatekeeper.R:40-54`: `n_verified =
  sum(!is.na(att_covered))` gates `coverage_rate`, and `case_when(n_verified==0 ~
  "UNVERIFIED", coverage_rate >= threshold ~ "PASS", …)` — a CI-less estimator is
  **UNVERIFIED**, exactly as Const. §4.2.3 requires (this resolves the v0.1.9-audit
  M2 NaN→mislabel hazard, which `lane3_statistics` §B reproduces in raw R:
  `mean(NA, na.rm=TRUE)=NaN`). A conservative wide-CI estimator covers 0 in 100% of
  runs → **PASS** (§B): caution is not gated. The QST 10/10 rule
  (`:128-158`, `run_fail = null_rejection_rate > 0.10`, verdict FAIL if
  `run_fail_rate > 0.10`) matches Const. §4.2.2. **EMPIRICAL + source: sound.**
- **Typed no-cross-scoring underpins fair comparison** (cross-lane
  `lane1_check1`): the join is `requested ∩ produced ∩ truth-available`; ATE is
  never scored as ATT. **EMPIRICAL: sound.**

## 3. Findings

**F1 — Estimator rankings are seed-unstable on heavy-tailed DGPs.**
Type **EMPIRICAL** (`lane3_statistics` §C): on `synth_heavytail` the lm_att vs
ipw_att ATT ordering **flips** between disjoint seed sets (A: ipw<lm; B: lm<ipw),
with mean|error| swinging 0.60→2.16 (≈3×) at n=400, 15 seeds; Spearman rank corr
A-vs-B = 0.80. On `synth_baseline` the ordering is stable. Tier: **scientifically
valid** (a reported ranking on a heavy-tail DGP at modest replication can invert
under reseeding). v0.3.0: a **foundational repair** before families' "kill-plot"
curves inherit unstable rankings. Action: **future spec ticket** (minimum-seed
guidance keyed to DGP tail behavior).

**F2 — Aggregates report per-cell dispersion but not the mean's uncertainty or a rank-stability signal.**
Type **EMPIRICAL + DEFINITIONAL** (`R/cs-summary.R:57-67`): `cs_summarise_runs`
returns `n_runs, mean_true_att, mean_est_att, mean_error, sd_error, mean_abs_error,
max_abs_error, mean_att_covered, mean_att_ci_width`. It **does** report
`sd_error = stats::sd(att_error)` (cs-summary.R:62) — per-cell Monte-Carlo
dispersion, from which the SE of the mean is `sd_error/sqrt(n_runs)`; the raw
material for MC uncertainty is therefore present. **Absent**: a
standard-error-of-the-mean column, a CI on `mean_error`, and any rank-stability /
seed-noise signal tying dispersion to whether a ranking gap (F1) exceeds noise —
so a user must hand-derive the SE and gets no warning when a ranking is within
noise. horizon.md (ACIC item, ~line 285) lists "Monte Carlo uncertainty" as
unresolved. Tier: **valid**. v0.3.0: foundational. Action: **future spec ticket**
(surface MC-SE/CI on aggregates + a rank-stability guard).
*[Corrected after Phase-5 meta-review: the original overstated this as "no SE/MC
uncertainty column"; `sd_error` is reported — see meta-review.md (c).]*

**F3 — Aggregate summaries are not survivorship-honest (self-acknowledged).**
Type **DEFINITIONAL** (`R/cs-summary.R:58-66`): the only denominator is `n_runs =
dplyr::n()`; coverage/CI-width use `na.rm=TRUE`; there is no
planned/attempted/succeeded/failed/timeout/missing vocabulary. Per-task failures
*are* captured atomically (`contracts.md` Runner Output Contract, `success=FALSE`
rows), but the aggregation layer collapses them. horizon.md item 6 (~lines
308, 535-540) explicitly names this the "survivorship gap" and defers it. Tier:
**valid** for aggregate/kill-plot conclusions. Action: **future spec ticket**
(already routed in horizon). *Note:* my empirical survivorship probe
(`lane3_statistics` §A) was **defective** (API misuse — `cs_summarise_runs`
expects a flattened tibble, not result-lists; stub estimator errored on all
seeds), so this finding rests on source + the project's own deferral, and a
corrected harness is a deferred experiment (cost ≈10 min).

**F4 — Raw errors are not comparable across DGPs of different outcome scale.**
Type **DEFINITIONAL** (empirical shadow `lane2_pathology_v2` §A: kangschafer naive
bias −19.7 on a Y≈210 scale vs baseline 0.13 on a Y≈1 scale — ~150×). `mean_error`
also averages *signed* errors (cancellation). Any cross-DGP aggregate of raw
`mean_error`/`mean_abs_error` implicitly assumes scale comparability that does not
hold; per-DGP summaries are fine. Tier: **valid** (only if cross-DGP aggregation is
attempted). Action: **documentation only** (warn against cross-DGP raw-error
pooling; prefer per-DGP or scale-normalized reporting).

## 4. No material finding

- **Gatekeeper correctness** (ATT UNVERIFIED handling, QST 10/10 rule,
  conservative-estimator pass) verified against current source — the review
  question "does the gatekeeper punish caution?" resolves **no** (it is a one-sided
  hallucination screen; "passed" ≠ "well-calibrated" is a documentation nuance, not
  a defect).
- **Per-task failure capture** at the atomic layer (`success=FALSE` rows,
  batch reconciliation `length(results)+nrow(errors)==nrow(tasks)`) is sound; the
  gap is strictly at the *summary* layer (F3).
- **CI gating** (<90% bootstrap replicate success ⇒ NA CI + `success=FALSE`) is a
  coherent validity guard — not challenged.
