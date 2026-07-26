# PASS 2 · Phase 2 · Lane STATISTICS — report

Full-repo review of the statistical-integrity surface. Intent-first: a behavior
that fulfils a DGP's declared scientific intent is SOUND, not a finding. All
EMPIRICAL claims below are backed by a seeded, runnable check under
`inst/design/audit/pass2/checks/` (prefix `p2_lane3_`), each with a captured
`*_output.txt`.

**Generic limits (stated once, apply to every empirical claim):** checks are
synthetic-only, single-machine, fixed explicit seeds, modest `n`
(300–800) and seed counts (8–20); they establish *mechanism/direction*
(what the tooling does or does not do), not calibrated effect sizes. R 4.5.2,
`NOT_CRAN=true`, `pkgload::load_all(".")`.

---

## 1. Reconstructed claim

CausalStress "identifies where estimators succeed, degrade, and fail under
controlled synthetic stress, without misleading comparisons across estimands,
populations, failures, or DGPs" (triage L62–67). The statistics lane owns the
guarantees behind *non-misleading*: (a) metrics scored only where they are
well-posed in the DGP's moment regime (L2/RMSE need finite variance; Families
§8.1.3, Art VI); (b) Monte-Carlo aggregation that does not launder failures or
noise into point verdicts; (c) an Article-IV gatekeeper that verifies coverage
and refuses to certify what it cannot verify. Typed no-cross-scoring is the
structural spine: `scoreable = requested ∩ produced ∩ truth-available`, else a
machine `non_comparable` reason (contracts.md:69–79).

**Corrected inheritance from pass 1.** The heavy-tail ATT ranking/RMSE
instability is the *intended* "L2 break" demonstration, not an aggregation
defect (triage L18–22; heavytail.Rmd §2/§4). Pass-1's headline S3 is withdrawn;
its correctly-scoped successor is the **metric-regime** question in §3-F1. Also
corrected: `cs_summarise_runs` **does** report `sd_error` (pass-1's "no MC
uncertainty" was wrong — see §2 and the bounded refinement in §3-F3).

---

## 2. What is scientifically sound

**S1 · Gatekeeper is correct (Article IV).**
`cs_summarise_gatekeeper()` (R/cs-gatekeeper.R) certifies only what it can
verify. A correct, conservative estimator (lm_att + bootstrap CIs) on a placebo
DGP **PASSES** the ATT gate (coverage 1.000, n_verified 12); a **CI-less**
estimator maps to **UNVERIFIED** — the `n_verified == 0` branch fires
(cs-gatekeeper.R:37–54), it does *not* silently PASS/FAIL or crash on NA; the
QST 10/10-rule branch (cs-gatekeeper.R:115–158) is present.
*Check: `p2_lane3_gatekeeper.R` / `_output.txt`.*

**S2 · heavy-tail ranking instability is INTENDED, not a defect.**
On `synth_baseline` (Gaussian, finite variance, mean well-posed) the lm_att vs
ipw_att ordering is **stable** across two *disjoint* seed sets (lm<ipw in both).
On `synth_heavytail` (0.8·N + 0.2·Cauchy, no finite mean) the ordering **flips**
(ipw<lm → lm<ipw) and RMSE swings (A: 0.60–0.64; B: 1.93–2.16). This is exactly
the DGP's declared purpose — "any L2/MSE estimator is not well-posed"
(heavytail.Rmd §4, l.145–147; §2 l.99–109) — and triage L18–22 ("the
instability is the correct result"). Reported as SOUND.
*Check: `p2_lane3_ranking.R` / `_output.txt`.*

**S3 · ATT truth stays well-defined under heavy tails.** Structural ATT is
`E[τ(X)|W=1]` from the deterministic signal, independent of realized noise
(heavytail.Rmd §5, l.155–158; `cs_true_att`). So the truth layer is sound even
where the *metric regime* for a mean-targeting estimator is ill-posed — the
instability lives in the estimator/metric, not the truth.

**S4 · Typed no-cross-scoring reasons are correctly emitted.**
`cs_build_score_surface()` (cs-contracts.R:372–491) emits `target_not_implemented`
(CATE, :404–411), `estimator_not_produced` (:413–420), and `truth_unavailable`
(:422–429); ATT/QST/ATE are each scored only against their own truth. On both
baseline and heavytail, lm_att ATT rows are all `score_status == "scored"` with
`non_comparable_reason == NA`. *Check: `p2_lane3_metric_regime.R`.*

**S5 · `sd_error` IS reported (pass-1 correction).** `cs_summarise_runs()`
returns `n_runs, mean_error, sd_error, mean_abs_error, max_abs_error,
mean_att_covered, mean_att_ci_width` (cs-summary.R:57–67). Dispersion of the ATT
error across seeds is exposed. *Check: `p2_lane3_metric_regime.R` (SCOPE 3).*

---

## 3. Findings (3; each correctly scoped, intent-aware)

### F1 — Metric-regime enforcement is PROSE-ONLY (no machine gate on a no-mean DGP)
- **Claim under test:** does the tool treat mean-based scoring (mean_error/RMSE)
  of ATT on a NO-MEAN DGP any differently than on a finite-variance one?
- **Files/lines:** `cs-contracts.R:103` (the reason token
  `metric_invalid_for_regime` is *defined* in the vocabulary) vs
  `cs-contracts.R:372–491` (`cs_build_score_surface` — the only scoring path for
  point DGPs — **never emits it**); `cs-summary.R:57–67`; contracts.md:76–79
  ("includes at minimum"); Families §8.1.3 / Art VI (moment_regime, rmse_valid,
  l.90–98, 188–190).
- **Type + evidence (EMPIRICAL, `p2_lane3_metric_regime.R` / `_output.txt`):**
  running the *same* mean-targeting estimator (lm_att) on `synth_baseline` vs
  `synth_heavytail`, the scoring machinery is byte-for-byte structurally
  identical — no warning on either; **0** score rows carry
  `metric_invalid_for_regime`; all ATT rows are `scored`; the summariser carries
  **no** regime / moment / metric-validity column; `mean_error`/RMSE are reported
  as plain numbers (heavytail just larger: mean_error 0.78 & RMSE 3.23 vs
  baseline 0.004 & 0.034). A source-grep confirms `metric_invalid_for_regime`
  occurs **exactly once** in all of `R/` — its definition. `provenance$dgp_noise_family`
  does carry `"heavy"`, but it is descriptive metadata, not a scoring gate.
  **Conclusion: "use QST/robust on heavy tails" is ENFORCED nowhere at the
  machine level; it is prose-only** (heavytail.Rmd §2/§8, sidecar `target: both`).
  Nothing stops a user naively ranking a mean-ATT across a no-mean DGP.
- **Scoping (intent-first):** this is the correctly-scoped successor to pass-1's
  withdrawn S3. It is *not* the aggregation defect pass-1 alleged — the
  instability itself is intended (S2) and ATT truth is sound (S3). The gap is
  narrowly that the vocabulary token exists but is unwired for point DGPs, so a
  user who did *not* read the dossier gets no signal.
- **Tier:** moderate / most material of this lane (it is the central scope-1
  question) but bounded — no false number is produced against a valid truth; the
  harm is a missing guardrail, not a wrong score.
- **v0.3.0 bearing:** the intended home is the parameterized-family machinery
  (`moment_regime$rmse_valid`, Families Art VI/VIII; `cs_summarise_stress`
  gates RMSE on `moment_regime$rmse_valid`, spec l.1785–1788). Point-DGP scoring
  never adopted it.
- **Action class:** SPEC-DECISION — either wire `metric_invalid_for_regime` (or a
  regime caveat surfaced by `cs_summarise_runs`/`cs_collect_scores`) into
  point-DGP scoring for declared no-mean DGPs, or explicitly document the token
  as v0.3.0-families-reserved. Not a fix to apply in this pass.

### F2 — `cs_summarise_runs` exposes no failure denominator (survivorship gap)
- **Claim under test:** when an estimator errors on some seeds, do summaries
  expose the failures in denominators, or silently average survivors?
- **Files/lines:** `cs-summary.R:57–67` — `n_runs = dplyr::n()` (:58) counts
  *attempted* rows; error columns `mean_error`/`sd_error`/`mean_abs_error`/
  `max_abs_error` (:61–64) use **no `na.rm`**; coverage/width columns (:65–66)
  use **`na.rm = TRUE`**. There is no `n_success`/`n_failed`/`n_attempted` column.
- **Type + evidence (EMPIRICAL, `p2_lane3_survivorship.R` / `_output.txt`):**
  rebuilt the pass-1 DEFECTIVE probe correctly — pass-1 fed structured
  result-lists to `cs_summarise_runs` (needs a flattened tibble) and never
  actually tested this. Registered a deterministic partial-failure estimator
  (errors on even run-seeds via `config$seed`), ran through `cs_run_grid`
  (seeds 1:8 → 4 succeed, 4 fail). Result: the summary reports **n_runs = 8**
  while only **4** runs succeeded, with **no** success/failure column. Because
  the error columns lack `na.rm`, a single failed seed **NA-poisons** all error
  means to `NA` (a crude "something broke" tell, but it reports neither the
  count nor a survivors-only mean); the coverage/width columns' `na.rm = TRUE`
  is the survivor-averaging path (inert here only because `ci_method = "none"`).
  A reader cannot tell from `cs_summarise_runs` alone that half the seeds failed.
- **Tier:** low — KNOWN and DEFERRED.
- **v0.3.0 bearing / protection cite:** explicitly deferred in
  **horizon.md:535–542** ("extend the public summaries … to report
  planned/attempted/succeeded/failed/timeout/missing denominators instead of
  silent `na.rm = TRUE` means — the survivorship gap identified in the
  2026-07-20 external review"), coupled to the runner-integrity migration and to
  horizon.md:308–311 (ship these denominators *before* survivorship-honest
  claims).
- **Action class:** DEFER (already tracked) — no v0.2.0 mislabelling claim rests
  on survivorship-honest denominators.

### F3 — Aggregation reports dispersion but no Monte-Carlo uncertainty of the mean
- **Claim under test:** is there a standard-error-of-the-mean / CI /
  rank-stability signal, or only dispersion?
- **Files/lines:** `cs-summary.R:57–67`. `sd_error` present (:62); **absent:** any
  `se_/mcse/ci_lo/ci_hi/rank/stability` column.
- **Type + evidence (EMPIRICAL, `p2_lane3_metric_regime.R` SCOPE 3):** column
  inventory confirms dispersion only. SEM would be `sd_error/sqrt(n_runs)`
  (e.g. 0.0159 in the demo) but is not reported; there is no CI-of-the-mean and
  no cross-seed rank-stability statistic. Consequence: when comparing two
  estimators' `mean_error`, the summary gives the reader no built-in way to judge
  whether a gap is inside Monte-Carlo noise (the ranking check in F/S2 had to be
  built by hand across disjoint seed sets).
- **Tier:** low (bounded refinement, not a correctness bug). Corrects pass-1's
  overstatement: dispersion *is* present; only the *inferential* signal (SEM/CI/
  rank-stability) is missing.
- **v0.3.0 bearing:** Monte-Carlo uncertainty is carried as an unknown/deferred
  scoring detail in horizon.md:285.
- **Action class:** ENHANCEMENT / DEFER — add `se_mean`/MCSE (and optionally a
  rank-stability diagnostic) to `cs_summarise_runs`.

*(Findings cap 10, typical 3–7; three filed. Stale-registry vs sidecar numeric
drift, e.g. hd_sparse_plm p/ρ/overlap, is a DGP-lane concern and is not
adjudicated here.)*

---

## 4. Where I found NO material finding (non-empty)

- **Gatekeeper (S1):** correct on both branches; CI-less → UNVERIFIED, not a
  silent pass or NaN crash (the v0.1.9 NaN hazard is closed by the `n_verified`
  gate). No finding.
- **heavy-tail ranking flip (S2):** intended demonstration; filing it as a defect
  would repeat pass-1's error. No finding.
- **ATT truth under heavy tails (S3):** structural, noise-independent, sound. No
  finding.
- **Typed no-cross-scoring (S4):** reasons emitted correctly; ATT/ATE/QST never
  cross-scored in the paths exercised. No finding.
- **`sd_error` reporting (S5):** present and correct — the pass-1 "no MC
  uncertainty" claim is over-stated; only the narrower SEM/CI gap (F3) survives.
- **Ranking stability on finite-variance DGP:** the tool is *not* unstable where a
  mean is well-posed (`synth_baseline` ordering stable across disjoint seeds) —
  the stated non-misleading guarantee holds where it is meant to.

---

### Check index
| Check | Scope | Result |
|---|---|---|
| `p2_lane3_metric_regime.R` | metric-regime enforcement (1) + uncertainty columns (3) | prose-only; `metric_invalid_for_regime` never emitted; sd_error present, no SEM |
| `p2_lane3_survivorship.R` | failure denominators (4) | n_runs=8 with 4 failures, no success/fail column (deferred) |
| `p2_lane3_ranking.R` | ranking stability (2) | baseline STABLE; heavytail FLIP (intended) |
| `p2_lane3_gatekeeper.R` | gatekeeper (5) | conservative PASS; CI-less UNVERIFIED; 10/10 rule present |
