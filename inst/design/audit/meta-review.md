# Phase 5 — Independent Meta-Review

**Reviewer:** fresh Phase-5 agent, full repository access, uninvolved in Phases 0–4.
**Date:** 2026-07-26 · **Scope:** spot-check load-bearing claims at SOURCE
(code, governance docs, captured check outputs); judge whether the packet fairly
represents the underlying lane reports and checks. No new scientific findings —
only confirmations, deliverable-vs-source discrepancies, and a fairness judgment.

## (a) Confirmation rate

**7 of 8 verification items fully confirmed at source. 1 item (claim 3) is
partially confirmed with one material discrepancy** (the empirical ranking-flip is
confirmed; the accompanying source claim that `cs_summarise_runs` exposes *no*
Monte-Carlo-uncertainty column is contradicted by the source). Every item was
verifiable at source — this meta-review is **not** a skip.

## (b) Claim-by-claim verdicts

### 1. S1/C1 — typed scoring & oracle exactness — **CONFIRMED**
`checks/lane1_check1_typed_scoring_output.txt` shows, across seeds 101–105:
`oracle ATT error == 0 (got 0.000e+00)` and `oracle ATE error == 0` (lines 4–5,
16–17, 28–29, 40–41, 52–53); `ATT truth != ATE truth` finite-sample gap
+0.11666/+0.11837/+0.09425/+0.11521/+0.11385 (lines 8, 20, 32, 44, 56 → mean
≈0.11, matching "≈0.11"); unproduced QST → `non_comparable/estimator_not_produced`
and unimplemented CATE → `non_comparable/target_not_implemented`, both with NA
value fields (lines 10–12 etc.); airlock "drops y0/y1/p/structural_te (kept:
y,w,X1..X5)" (line 73). Mechanism cross-checked in `R/cs-contracts.R`:
`cs_truth_available_targets` (lines 311–327) admits `att`/`qst`/`ate` only when
their truth objects are present and well-formed; `cs_build_score_surface`
(lines 373–464) emits `non_comparable` rows for `cate` (target_not_implemented,
404–410), not-produced targets (estimator_not_produced, 413–419), and
truth-absent targets (truth_unavailable, 422–428) before any scoring. Source and
output agree.

### 2. S2/C5 — gatekeeper — **CONFIRMED**
`R/cs-gatekeeper.R`: `n_verified = sum(!is.na(att_covered))` (line 40),
`coverage_rate` guarded to `NA_real_` when `n_verified==0` (if_else 41–45), and
`status = case_when(n_verified == 0L ~ "UNVERIFIED", coverage_rate >= threshold ~
"PASS", TRUE ~ "FAIL")` (49–53) — CI-less maps to UNVERIFIED, never NaN/PASS. QST
rule: `run_fail = null_rejection_rate > 0.10` (line 134) and `status =
case_when(... run_fail_rate > 0.10 ~ "FAIL", TRUE ~ "PASS")` (152–158) — exactly
as claimed. Constitution cross-check: §4.2.2 "The rate of Null Rejection runs must
not exceed 10%" ("10/10 Rule", `CAUSAL_STRESS_CONSTITUTION.md:214,216`); §4.2.3
"Estimators without confidence intervals are labeled 'Unverified' rather than
failing" (line 224). The NaN hazard the fix resolves is reproduced in
`lane3_statistics_output.txt` §B: `mean(covers0, na.rm=TRUE)= NaN`, and the
conservative wide-CI estimator covers 0 in 100% of runs (fraction 1.00 ≥ 0.90).
Source, output, and Constitution agree.

### 3. S3/C4 — ranking instability — **PARTIALLY CONFIRMED (one DISCREPANCY)**
*Empirical portion — CONFIRMED.* `lane3_statistics_output.txt` §C: `synth_heavytail
lm_att` 0.6356 vs `ipw_att` 0.6014 on seed set A (1:15) → ipw<lm; 1.9302 vs 2.1601
on seed set B (101:115) → lm<ipw; `synth_heavytail A:ipw<lm B:lm<ipw
ordering_stable=FALSE` (lines 15–27). `Spearman rank corr A vs B: 0.8000` (line
24) — matches the stated 0.80. The 0.60→2.16 ("≈3×") swing is the `ipw_att` cell
(0.6014→2.1601). `synth_baseline` ordering stable.
*Source portion — DISCREPANCY.* All three deliverables state
`cs_summarise_runs` carries no Monte-Carlo-uncertainty/SE column: lane3 F2 —
"returns `n_runs, mean_error, mean_abs_error, max_abs_error` — **no SE / MC
uncertainty column**"; synthesis S3 — "reports point means with no SE
(`cs-summary.R:58-66`)"; packet C4 — "no MC-SE column warns the user
(`cs-summary.R:58-66`)". **The source contradicts this.** `R/cs-summary.R:62`,
inside the cited 58–66 range, computes `sd_error = stats::sd(att_error)` — the
Monte-Carlo standard deviation of the per-seed ATT error, from which the SE of the
mean is `sd_error/sqrt(n_runs)`. The full return set (lines 57–67) is `n_runs,
mean_true_att, mean_est_att, mean_error, sd_error, mean_abs_error, max_abs_error,
mean_att_covered, mean_att_ci_width`; the docstring (lines 12–17) also lists
`sd_error`. So the flat claim "no MC-uncertainty column" and lane3 F2's itemized
column list (which omits `sd_error`) are inaccurate. The deeper concern survives
in weakened form — there is no *standard-error-of-the-mean* column, no CI, and no
rank-inversion/seed-noise warning — but the categorical "no MC uncertainty"
framing overstates the gap, and the check that would have enumerated the columns
empirically (`lane3` §A) errored (`runs must be a data.frame or tibble`), so the
column list was asserted from a mis-read of source, not observed.

### 4. S6 — hd_sparse independence violation — **CONFIRMED**
`lane2_pathology_v2_output.txt` §E: `synth_hd_sparse_plm sd(y1-y0-tau)=0.0000`
(line 51) — shared within-unit noise. Registry mandate confirmed:
`CAUSAL_STRESS_DGP_REGISTRY_1.4.0.md:48` — "**Independence:** ε₀, ε₁ are
independent draws from the specified noise law," under §1.2 step 4 which scopes it
to "all synthetic (non-placebo) DGPs" (line 47). Generator confirmed:
`R/dgp-synth-hd-sparse-plm.R` every version sets `tau <- rep(1, n)` then `y1 <- y0
+ tau` (v1.3.0 line 51; v1.4.0 line 128; v1.5.0 line 202 — the latter is the
`synth_hd_sparse_plm` latest wrapper at line 247–249 and the version the check
exercised, §A shows `v1.5.0`), so y1 = y0 + 1 with a single ε in y0 → sd(y1−y0−τ)
≡ 0. Degenerate-QST consequence confirmed at Registry line 257: "true_qst
corresponds to a constant +1 shift ... on `cs_tau_oracle()`." A genuine
governing-clause contradiction plus a non-informative QST cell.

### 5. S4 — survivorship / defective-check honesty — **CONFIRMED**
The probe is honestly labeled defective and not cited as positive evidence:
synthesis §"Resolution hierarchy" ("the `lane3` survivorship probe misused the
runner API (supports no verdict …)") and S4 ("Empirical probe was defective");
packet §3 ("the survivorship probe misused the runner API — so the survivorship
finding rests on source + the project's own deferral"); lane3 F3 ("my empirical
survivorship probe … was **defective** (API misuse)"). The API error is real:
`lane3_statistics_output.txt` §A — "summarise error: `runs` must be a data.frame
or tibble." (line 4), matching the `cs_summarise_runs` type guard at
`R/cs-summary.R:24-28`. The deferral is real: `horizon.md` item 6 (lines
535–540) — "extend the public summaries … to report
planned/attempted/succeeded/failed/timeout/missing denominators instead of silent
`na.rm = TRUE` means — the survivorship gap …"; and item 8 (line 308) —
"ship explicit planned/attempted/succeeded/failed/timeout/missing denominators
before claiming … survivorship-honest kill plots." (Minor citation note: the
synthesis labels the 535–540 material "item 6" while line 308 is actually item 8 —
both cited locations do defer survivorship, so the substance holds.)

### 6. S8 — QST tail precision — **CONFIRMED**
`lane1_check2_qst_truth_recompute_output.txt`: `tau 0.01 : 4.16e-03` (line 14) —
i.e. ~4×10⁻³, matching "≈4.2×10⁻³". The τ=0.95 "FAIL" is correctly attributed as a
float-match artifact, not a real oracle NA: the script does `o_sel <-
oracle$value[match(taus_sel, oracle$tau)]`
(`lane1_check2_qst_truth_recompute.R:59`), and 0.95 fails to float-match the
seq-generated `oracle$tau` grid, so the *lookup* returns NA (output line 10: oracle
`NA` while indepMC resolves to 1.80665, and §C independently computes an oracle
MC-SE `tau 0.95 : 2.58e-03` at line 19, proving the truth exists). Both synthesis
(§"Resolution hierarchy": "the `lane1_check2` τ=0.95 'FAIL' is a floating-point
`match()` artifact") and packet (§3, C2 row + note) label it as an artifact.
Constitution §1.4 "<10⁻⁵" parenthetical confirmed at
`CAUSAL_STRESS_CONSTITUTION.md:59` — "analytic derivation matching oracle
precision ($< 10^{-5}$)."

### 7. Caps — **CONFIRMED**
Synthesis findings S1–S10 = **10** (≤10); synthesis "Cannot decide" experiments
numbered 1–5 = **5** (≤6). Lane findings: estimands F1–F3 = 3; dgp F1–F6 = 6;
statistics F1–F4 = 4 (each ≤10). Rebuttal attacks: `phase3-rebuttals/statistics.md`
A1–A3, `estimands.md` A1–A3, `dgp.md` A1–A3 = **3 each** (≤5). All caps respected.

### 8. Packet resolves no VALUE item — **CONFIRMED**
Packet §5 lists families-vs-CATE with options/trade-offs and closes "The review
takes **no position** on these" (line 82); §6 recommends "foundational scientific
repairs first" (neither families nor CATE) and routes the choice to maintainer
priority via flip conditions. Families-vs-CATE, the "stable"-semantics bar, and
scope-extension are all reserved, not decided. Synthesis §"Value items reserved"
concurs.

## (c) Discrepancy found

**One material discrepancy (claim 3, source sub-claim).** `cs_summarise_runs`
*does* expose a Monte-Carlo dispersion column, `sd_error = stats::sd(att_error)`
at `R/cs-summary.R:62` — squarely inside the 58–66 range the reviewers cite.
Lane3 F2's enumerated column list omits it and asserts "no SE / MC uncertainty
column"; synthesis S3 ("no SE") and packet C4 ("no MC-SE column") repeat the
overstatement. The accurate statement is narrower: no *standard-error-of-the-mean*
column, no CI, and no rank-stability/seed-noise warning are surfaced, but a
per-cell error SD (from which the MC-SE is one division away) **is** reported.
Because S3/C4 is billed as "the single most important repair," the overstatement
inflates the strongest negative finding. It does not invalidate the empirical
ranking-flip (that is independently confirmed), but the remediation should be
scoped as "surface MC-SE/CI + rank-stability guard," not "add MC uncertainty that
is wholly absent."

No other discrepancies: the other five load-bearing claims and the two structural
checks reproduce exactly at source.

## (d) Fair-representation judgment

**The packet fairly represents the underlying lane reports and checks, with the
single exception above.** Numbers, thresholds, and file:line citations in the
synthesis and human-review packet track the Phase-2 reports and the captured
check outputs faithfully (oracle-exactness, gatekeeper mapping, hd_sparse shared
noise, placebo sharp null, tail MC-SE, ranking flip, Spearman 0.80, naive-bias
ladder, coupling −0.65, tilt<baseline selection). The packet is appropriately
disciplined about evidence tiers: it *demotes* both defective agent checks (the
τ=0.95 match() artifact and the survivorship API misuse) instead of laundering
them into positive claims, correctly caveats "passed ≠ calibrated," and keeps
VALUE decisions with the maintainer. The one systematic weakness is that the
`sd_error` mis-statement propagated unchallenged from lane report → synthesis →
packet (see (e)).

## (e) Effect of the compromised Phase-2/3 isolation

The compromise is **disclosed prominently and honestly** in all three layers
(synthesis ¶"Process limitation"; each Phase-2 report's authorship note; packet
§"Review limitation"): the isolated lane agents were credit-terminated after
writing their check scripts, and the orchestrator authored the lane reports, the
Phase-3 rebuttals, and ran the checks. So Phases 2–3 were single-author, not
adversarially independent.

**Detectable bias effect: yes, exactly one, and it is the `sd_error` error.**
Weak isolation predicts *correlated blind spots* — a mistake made once by the sole
author survives because no independent adversary re-derives it. That is precisely
what happened: the "no MC uncertainty column" mis-read of `cs-summary.R` appears in
lane3 F2, is inherited by synthesis S3 and packet C4, and the Phase-3 rebuttals
(estimands A1 *concedes* to statistics F1) never challenge it — a genuinely
independent statistics adversary reading line 62 would likely have caught
`sd_error`. So the lost isolation did not fabricate findings or move the empirical
conclusions (all empirical results rest on rerunnable seeded scripts whose outputs
I re-verified at source), but it did let one factual overstatement ride
uncorrected into the most important repair recommendation. **Net:** the empirical
core is unbiased and reproducible; the single-author pipeline's cost shows up as
one uncaught source-reading error, consistent with the disclosed limitation rather
than any directional slanting of the review.

---
*Meta-review complete. Write boundary honored: only this file created; nothing
else modified, staged, or committed.*
