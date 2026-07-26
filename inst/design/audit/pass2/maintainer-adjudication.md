# PASS 2 — Maintainer Adjudication & Disposition Record

**Date:** 2026-07-26 · **HEAD:** `9c16cd2` · **Verdict on the pass-2 audit:**
**REQUEST CHANGES — synthesis corrections, not production-code logic changes.**
Accepted and applied to the audit below; production-doc changes are **routed**
(exact wording in §6), not silently applied to a governed release. **No DGP
demotion, no constitutional amendment.**

## 1. `synth_heavytail` is an intentional estimand-boundary DGP (KEEP STABLE)

Paper 3's purpose is to exhibit that a conventional mean-based ATT *fails* under
heavy tails; the pathology **is** the scientific result, so the DGP stays
`stable` — it is stable precisely because it reliably reaches the regime where a
conventional mean-based ATT ceases to be well-defined. Governing framing for
v0.2.0:

> **`synth_heavytail` is an intentional estimand-boundary DGP.** Its
> Cauchy-mixture outcomes have no finite mean, so conventional mean-based ATT
> interpretation — bias, RMSE, coverage, and estimator rankings — is **not
> valid**. CausalStress retains a *governed structural signal anchor*
> (`ATT_true = mean of the deterministic τ(X)`, Const. §1.3 l.43) for diagnostics,
> while **QST remains a well-defined distributional estimand**. The "structural
> ATT" reported here is the governed signal contrast, **not** an ordinary mean
> potential-outcome estimand.

**Correction to the audit's own wording:** it is imprecise to say "the ATT truth
is well-posed" without this qualifier. The *governed signal target* τ(X) is finite;
the *conventional mean potential-outcome ATT* is not (independent Cauchy noise ⇒
the conditional outcome means do not exist). heavytail dossier l.145 acknowledges
the missing outcome mean; Const. l.43 defines ATT via the noise-free signal —
internally intentional, scientifically **nonstandard**, and must be stated.

**Operating rule (do / don't):** keep executing ATT estimators (their breakdown is
the evidence); keep raw estimates and deviation-from-signal-anchor; **do not** run
an ATT estimator shootout / winner-ranking here; **do not** call the deviation
"ATT bias/RMSE"; report non-convergence, seed instability, extreme sensitivity, and
rank instability as *the result*; use QST for valid distributional comparisons.

## 2. REJECT atomic score-row suppression; guard at the AGGREGATION layer

The audit (statistics F1 / synthesis S1) floated wiring `metric_invalid_for_regime`
into `cs_build_score_surface` — i.e. marking heavytail ATT rows `non_comparable`.
**Rejected.** The atomic score uses `metric_id="point_error"`: a realized estimate
vs. the declared structural-signal truth is a legitimate comparison and **is the
breakdown evidence** the DGP exists to produce. Suppressing it erases the finding.
**Architectural rule: invalid comparison must not mean skipped execution.**

The invalidity arises when **aggregating**: population mean error may not exist;
RMSE / error variance do not exist; `sd_error/√n_runs` is not a meaningful MCSE
without a finite second moment; rankings can stay seed-unstable by design.
**Corrected design (routes to the families program):**
- keep atomic ATT `point_error` rows **scored**;
- add explicit **moment-regime metadata** (data-model root, dgp F4/S5);
- at the **aggregation layer**, mark mean/RMSE/SEM/ranking summaries **invalid /
  interpretation-limited** beyond the mean-existence boundary; provide **robust**
  summaries (median and quantiles of absolute error);
- reserve `metric_invalid_for_regime` for the actual invalid **aggregate metric**,
  not the whole estimand target;
- families state = **`diagnostic_only`**, not `ineligible`;
- add a `cs_summarise_runs()` roxygen warning **now** (mean/SD summaries require
  suitable moments).

## 3. Three-regime taxonomy (for Paper 3 / families)

| Moment regime | Valid interpretation |
|---|---|
| Finite variance | ATT comparison, bias, variance, RMSE all valid |
| Finite mean, infinite variance | ATT exists; RMSE / standard MCSE fail; robust summaries required |
| No finite mean (`synth_heavytail`) | Conventional ATT shootout **stops**; structural-anchor diagnostics + QST remain reportable — the "estimand death zone" Paper 3 exposes |

## 4. Oracle MC-uncertainty finding — CARRIED (pass 2 wrongly dropped it)

Pass 1 estimated the QST oracle's own Monte-Carlo SE at **~4.16×10⁻³ at τ=0.01**
(`checks/lane1_check2_..._output.txt`) — i.e. the N=10⁶ oracle QST *tail* truth
carries ~10⁻³ sampling uncertainty, not <10⁻⁵. Pass 2's "oracle error = 0"
(`lane1_check1`) checks an oracle **estimator against the same cached truth** —
**scorer consistency, not population-truth accuracy** — and does **not** supersede
this. **No constitutional violation:** Const. §1.4 "<10⁻⁵" reads as the tolerance
for an *analytic derivation matching the governed MC oracle*, not a requirement that
the MC oracle's own SE be <10⁻⁵. Action: **carry the finding** (documentation /
future QST-MCSE reporting); **no amendment**.

**False-wording correction (real doc defect):** six v1.6.0 dossiers and
`R/dgp-synth-heavytail.R:144` state CRN "**Eliminates Monte Carlo noise**" (resp.
"eliminate MC variance"). CRN **reduces** Monte-Carlo variance of the *contrast*;
it **cannot eliminate** empirical-quantile sampling uncertainty. Route the wording
fix (§6).

## 5. Language softening

The audit's "all 12 DGPs are verified / deliver their declared stress" is softened
to: **"the targeted declared signatures were reproduced under a fixed seed"** —
this is signature reproduction, not full scientific validation; **10 of 12 DGPs
remain `experimental` / "pending human validation."**

## 6. Disposition table + routed production-doc changes

**Findings (accepted / deferred / rejected):**

| Item | Disposition | Action |
|---|---|---|
| heavytail instability = intended "L2 break" | **ACCEPTED (sound-by-intent)** | keep stable; reframe per §1 |
| hd_sparse constant-shift shared noise | **ACCEPTED (sound-by-intent)** | Registry §1.2 exception doc-note |
| S1 metric-regime guard | **ACCEPTED, RE-SCOPED** | aggregation-layer, not atomic suppression (§2); families `diagnostic_only` |
| Atomic `non_comparable` for heavytail ATT | **REJECTED** | keep rows scored (§2) |
| Oracle QST tail MC-SE ~4e-3 | **ACCEPTED, CARRIED** | doc / future QST-MCSE (§4); no amendment |
| "Eliminates MC noise" wording | **ACCEPTED (defect)** | wording fix, §6 routed |
| Survivorship denominators (S2) | **DEFERRED (already)** | horizon.md:535-542; before family kill-plots |
| SEM / rank-stability signal (S3) | **DEFERRED (enhancement)** | horizon.md:285 |
| Registry 1.4.0 stale (S4) | **ACCEPTED (doc defect)** | registry-version bump |
| Moment-regime-blind metadata (S5) | **ACCEPTED** | add `moment_order`/`att_mean_illposed` sidecar flag |
| Selection-on-observables scope (S7) | **ACCEPTED (scope)** | document taxonomy; future hidden-bias family |
| "all 12 verified" language | **ACCEPTED** | soften per §5 |

**Routed production-documentation changes (ACCEPTED; application pending your call
in the closing question — direct now vs. a new correction ticket, e.g. CS-1229):**

1. **`inst/dgp_meta/synth_heavytail.Rmd`** — distinguish the conventional
   (mean potential-outcome) ATT, which does not exist here, from the governed
   structural signal anchor τ(X); state the do/don't operating rule (§1).
2. **`R/cs-summary.R` roxygen (`cs_summarise_runs`)** — add: *"`mean_error`,
   `sd_error`, `mean_abs_error`, `max_abs_error` require the estimator error to have
   finite mean/variance. On infinite-variance or no-mean DGPs (e.g. `synth_heavytail`)
   these are not valid; prefer median / quantile summaries and QST."*
3. **`README`** — state that `synth_heavytail` is an estimand-boundary DGP, **not**
   an ATT-ranking scenario.
4. **Six v1.6.0 dossiers + `R/dgp-synth-heavytail.R:144`** — replace "Eliminates
   Monte Carlo noise" / "eliminate MC variance" with **"reduces Monte-Carlo variance
   of the QST contrast; does not eliminate empirical-quantile sampling uncertainty."**
5. **Families planning (`horizon.md`/families spec)** — record the three-regime
   taxonomy (§3) and the `diagnostic_only` state beyond the mean-existence boundary.

All of the above are **documentation** — no DGP, truth, RNG, or scoring-logic change,
no constitutional amendment.
