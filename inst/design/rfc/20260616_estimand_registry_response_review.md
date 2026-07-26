# RFC Response-Review: Estimand Registry Seed

**Status:** Response-review stage (adjudication of the response; precedes seed-v2)
**Date:** 2026-06-16
**Reviews:** `20260616_estimand_registry_response.md` (Codex) against `20260616_estimand_registry_seed.md`
**Author:** Max Thomasberger (maintainer) + assistant (seed author)

## Governance role and conflict-of-interest disclosure

This is the **response-review** stage. The short-form `rfc_cycle.md` does not list it explicitly (it goes seed → response → seed-v2 → synthesis); the richer lineage this process descends from had it as a distinct stage, and recorded that skipping it was costly. It is added here deliberately and should be folded into `rfc_cycle.md` as a recognized optional stage (tracked separately).

Codex's `_response.md` is preserved **verbatim** — it is the antithesis in the disagreement trail and is not edited. This file is a separate artifact.

**Conflict disclosure (load-bearing).** The response-review was performed by the *seed author*. Where I **downgraded** Codex's findings, I was defending my own seed — a conflict that was **not self-certified**. Those four contested downgrades (F1–F4) were adjudicated externally:

- **F1, F2, F4** — by the maintainer directly (rulings recorded below).
- **F3** — by an independent external prior-art pass (ChatGPT), whose six load-bearing citations I then verified against source. Provenance and the verified verdict are recorded in §3.

Findings where I *agreed with Codex against the seed* (F5, F9, F10, F13) carry no such conflict and flow into seed-v2 directly.

---

## 1. Verdict table (Codex findings F1–F15)

| Codex finding | Codex severity | Review verdict | Evidence / basis |
| --- | --- | --- | --- |
| F1 CATE train/test | BLOCKER | **DOWNGRADE→ required-MAJOR** (maintainer-ruled) | In-sample PEHE is a standard benchmark, but maintainer ruling = follow SOTA = support held-out PEHE (IHDP/Shalit et al.; CATENets). Net: a required design commitment, Codex-leaning. |
| F2 keyed `unit_id` | BLOCKER | **DOWNGRADE→ required-MAJOR** (maintainer-delegated) | Airlock preserves row order (`R/cs-airlock.R`), so an order-contract *could* suffice — but F1's held-out ruling forces keyed alignment. Net: keyed `unit_id` required. |
| F3 CATE placebo gate | BLOCKER | **RESTATE** (seed wrong; my downgrade reversed) | Prior-art pass: no per-unit placebo standard exists; the principled form is a heterogeneity-*detection* test. My "same as all gates" defense and "more discriminating" rebuttal were both wrong. See §3. |
| F4 PEHE heavy tails | BLOCKER | **DOWNGRADE→ MAJOR** (maintainer-ruled) | Seed *asked* this (Q5), didn't defer; the ATT infinite-variance analogy doesn't transfer (CATE preds are bounded averages). Ruling: PEHE + robust companion (median-abs CATE error), not a blocking prerequisite. |
| F5 version bump | BLOCKER | **CONFIRMED** | Preamble: no semantic change to an article without a major bump; Art. IV §4.2.5 says "exclusively ATT and QST." Removing exclusivity is semantic. (Caveat: Codex's own additive-restructure counter enables a minor path — decision required, not "default 2.0.0".) |
| F6 GRF/BART overstated | MAJOR | **CONFIRMED** | `est-grf-dr.R:128-150` discards the forest after extracting the scalar; `capabilities=c("att")`. Exposing CATE is real adapter work, not a trivial "un-collapse." |
| F7 registry "just supports_qst" | MAJOR | **CONFIRMED (incomplete)** | Registry also carries `oracle_columns`/`version`/`source`/`requires_pkgs`. Both seed and Codex missed the output-meta `capabilities`/`target_level` fields (see N1). |
| F8 estimand_target too small | MAJOR | **CONFIRMED** | The triple can't represent QST's tau grid or CATE's eval population. |
| F9 fit vs score identity | MAJOR | **UPGRADE→ BLOCKER** | `cs-fingerprint.R` identifies one result per task; multi-estimand-per-fit needs the `fit_fingerprint`/`score_fingerprint` split. Codex under-rated its own strongest finding. |
| F10 schema-4 blast radius | MAJOR | **CONFIRMED (UPGRADE→ blocker-grade)** | `cs-runner.R:644-660` branches schema NA/1/2/3 then hard-errors; schema-4 touches three resume dispatchers + consolidation. (Minor gap: omits `cs-run-one-seed.R`, `cs-plan-campaign` stamping.) |
| F11 missed surfaces | MAJOR | **CONFIRMED** | `cs_science_payload`/`cs_meta_flatten`/`cs_audit`/`cs_summarise_runs` hardcode att/qst. |
| F12 ATE finite vs superpop | MAJOR | **CONFIRMED** | `cs-truth.R` has only `cs_true_att`; no `cs_true_ate`. |
| F13 real DGPs / 3-way join | MAJOR | **CONFIRMED (UPGRADE→ blocker-grade)** | Real DGPs have `structural_te=NULL` (Constitution) → scoring join must be DGP∩estimator∩truth. Correctness hole in the whole model. |
| F14 airlock two channels | MINOR | **CONFIRMED** | Fine. |
| F15 RFC ordering | MINOR | **CONFIRMED (under-developed)** | True; underweights the RFC-2 freeze-shape risk (see N3). |

---

## 2. Maintainer tie-break rulings (F1, F2, F4)

- **F1 — CATE evaluation protocol → RESOLVED: adopt published SOTA.** The CATE-specific convention (IHDP as standardized by Shalit, Johansson & Sontag 2017; carried through CATENets/Curth) reports PEHE on a **held-out** test split. ACIC is in-sample SATT and a weaker precedent. **Ruling:** the runner must support a held-out CATE evaluation sample; in-sample PEHE is a legitimate secondary number. This moves F1 back toward Codex — my "in-sample is fine, just document it" was too lax for the CATE audience.
- **F2 — per-unit alignment → RESOLVED: keyed `unit_id`.** Maintainer had no strong opinion; F1's held-out ruling forces it (position-alignment across a train/test split is fragile). **Ruling:** every CATE-capable run carries a runner-issued immutable `unit_id`; the runner joins predictions to `structural_te` by key and hard-errors on missing/duplicate/extra ids.
- **F4 — PEHE under heavy tails → RESOLVED: PEHE + robust companion, not a blocker.** PEHE primary; add a robust companion (median-absolute CATE error or a trimmed/quantile loss) for heavy-tail regimes. The "infinite variance / circular dependency" framing was overstated — a CATE prediction is a bounded local average, not a Cauchy-inheriting sample mean. Metric *vocabulary* belongs in RFC-1; per-regime *validity envelopes* attach in RFC-3.

---

## 3. F3 resolution (independent prior-art pass; verified)

**Maintainer's framing (correct):** placebo tests are well-posed for *scalar* estimands (ATT; QST at each τ on a fixed grid). For an *individual/unit-level* estimand they run into the very issues raised — n sample-dependent pointwise checks, no per-unit uncertainty, a flat-zero estimator passing trivially. "Do placebo tests even make sense here?"

**Verified prior-art verdict:** No published standard evaluates CATE estimators by testing each `tau_hat(X_i) ≈ 0` under a sharp null. A "CATE placebo gate" is defensible **only** as a **false-positive test for heterogeneity *detection* (or calibration)** — i.e. "did the estimator hallucinate heterogeneity that isn't there?" — operationalized via the BLP slope / GATES / the Imai–Li grouped-homogeneity test / RATE, and scored as **Type-I error across placebo replications**. CATE *accuracy* is scored on **non-null** DGPs via PEHE (+ robust companion) and/or calibration. One correction to our prior understanding: the BLP slope is **score-contingent** (β₂ = 0 under no heterogeneity, but non-rejection ≠ homogeneity — it can also mean the estimator's score doesn't align); the *omnibus* options are Crump et al. (estimator-agnostic) or Imai–Li (grouped). Therefore:

- **Drop the seed's naive per-unit placebo gate** (no prior art; ill-posed). My F3 downgrade is reversed.
- **Recommended v0.2.0 form:** CATE scored on non-null DGPs; under placebo, **marked "Unverified" for hallucination** (Art. IV §4.2.3) unless a heterogeneity-detection test is implemented. Park the BLP/GATES detection gate as a future enhancement (now that we know it's the principled form and its cost: sample-splitting, per-estimator score extraction).
- **Diagnostic, not gate:** a variance/norm-of-`tau_hat(X)` "spread" statistic is reasonable but is **synthesis, not a literature standard** — label it diagnostic only.

This finding also generalizes: the maintainer separately observed the *existing* ATT/QST gate is too harsh (nobody survives Kang–Schafer, by design), and the binary `Non-Robust` label conflates "failed a deliberate trap" with "unusable." That is parked as the **Gatekeeper recalibration** entry in `horizon.md` (a separate RFC + Art. IV amendment). **RFC-1 must keep the new CATE/ATE gates pluggable and must NOT bake the current whole-suite-or-`Non-Robust` policy into them** — structure now, policy deferred to the recalibration RFC.

**Verified load-bearing citations** (checked against source 2026-06-16):

1. Chernozhukov, Demirer, Duflo & Fernández-Val (2018), *Generic Machine Learning Inference on Heterogeneous Treatment Effects in Randomized Experiments*, NBER w24678 / arXiv:1712.04802.
2. Imai & Li (2025), *Statistical Inference for Heterogeneous Treatment Effects Discovered by Generic Machine Learning in Randomized Experiments*, J. Business & Economic Statistics 43(1):256–268 / arXiv:2203.14511.
3. Crump, Hotz, Imbens & Mitnik (2008), *Nonparametric Tests for Treatment Effect Heterogeneity*, Review of Economics and Statistics 90(3):389–405.
4. Hahn, Dorie & Murray (2019), *ACIC 2017 Data Analysis Challenge*, arXiv:1905.09515 *(paper confirmed; the "null effects excluded / all DGPs covariate-dependent" sub-claim is from the full text, not independently re-verified)*.
5. Yadlowsky, Fleming, Shah, Brunskill & Wager, *Evaluating Treatment Prioritization Rules via Rank-Weighted Average Treatment Effects* (RATE), arXiv:2111.07966 → JASA 2025.
6. Yu & Sun (2025), *Do Contemporary Causal Inference Models Capture Real-World Heterogeneity? Findings from a Large-Scale Benchmark*, ICLR 2025 / arXiv:2410.07021 (62% of CATE estimates beaten by a zero-effect predictor; 80% by a constant-effect model).

**Supporting calibration cluster** (cited by the external pass, **not individually verified here** — spot-check before formal use): van Klaveren et al. 2018 (c-for-benefit); Maas et al. 2023 (E-for-benefit); Hoogland et al. 2024 (calibration intercept/slope); Xu & Yadlowsky 2022 (HTE calibration error); van der Laan et al. 2023 (causal isotonic calibration); Leng & Dimmery 2024; Dwivedi et al. 2020; Fink et al. 2014; Imai & Ratkovic 2013; Curth & van der Schaar 2021; Crabbé et al. 2022; Hill 2011; Dorie et al. 2019; Parikh et al. 2022.

**Provenance:** external ChatGPT prior-art pass, 2026-06-16; six load-bearing citations verified against source via web search; raw output **not retained in the repo** (per the standing rule that external unverified passes contribute verified citations, not standalone artifacts — distinct from `research/`, which holds workflow-verified notes).

---

## 4. New findings Codex missed

- **N1 (MAJOR) — capability representation is already fragmented.** Beyond the registry's `supports_qst`, the estimator *output meta* already declares `capabilities=c("att")` and `target_level="population"` (`R/est-grf-dr.R:211-212`). Three unsynchronized capability surfaces exist today; the estimand registry must **consolidate**, not add a fourth — and should cite these as existing half-implementations of the abstraction.
- **N2 (BLOCKER) — the amendment scope is wider than Art. I + IV.** Art. III §3.1 mandates the estimator output signature `list(att, qst, meta)`. Adding a `cate` slot changes that constitutional contract → the amendment touches **Art. III** too. Strengthens F5's version-bump argument.
- **N3 (MAJOR — sharpens F15) — RFC-2 freeze feasibility.** CATE's natural tidy shape (n predictions per run) is unlike today's scalar `est_att` + single QST list-column (`cs-result-to-row.R`). "Freeze the API over the estimand contract" may be infeasible without first redesigning the tidy/collect/plot shape to host per-unit data.
- **N4 (MINOR) — F1's held-out-sample counter has an uncosted RNG surface.** A second DGP draw for the eval set is new random draws under the Art. II mandate (seeding, RNG isolation, fingerprint inclusion). Must be costed in seed-v2.

---

## 5. Corrected severity ranking for seed-v2

**Must-resolve (blocker-grade):**
- F5 + N2 — version bump + Art. III scope (the amendment is Art. I + III + IV).
- F9 — `fit_fingerprint` / `score_fingerprint` split.
- F10 — schema-4 migration matrix (resume × consolidation × audit).
- F13 — three-way DGP∩estimator∩truth scoring join.
- F1 — held-out CATE evaluation protocol (maintainer-ruled).
- F2 — keyed `unit_id` alignment (maintainer-ruled).
- F3 — drop the per-unit gate; detection-test-or-"Unverified"; keep gates pluggable, defer calibration policy.

**Major (address in seed-v2):** F4 (PEHE + robust companion), F6, F7, F8, F11, F12, N1, N3.

**Minor:** F14, F15, N4.

**Reversed from Codex's framing:** F3 demoted from "CATE-specific defect" to "the per-unit gate is wrong, replace with detection" (the recovery-gate idea is a valid enhancement, not a blocker against the amendment); F1/F2/F4 down from BLOCKER but two of them (F1/F2) became *required* via maintainer ruling.

---

## 6. Disposition

**NEEDS-SEED-V2** (concur with Codex's verdict; corrected basis above). Seed-v2 must: settle the Art. I + III + IV amendment text and version-bump decision (F5/N2); define `fit_fingerprint`/`score_fingerprint` (F9) and the schema-4 migration matrix (F10); make the scoring join three-way (F13); commit to held-out CATE evaluation (F1) with keyed `unit_id` (F2); replace the per-unit placebo gate with a detection-test-or-"Unverified" design that stays pluggable and defers calibration policy to the recalibration RFC (F3); define the CATE metric vocabulary incl. a robust companion (F4); consolidate the existing capability surfaces (N1); and name the full blast-radius surface set (F11/N3). The seed author owns architectural intent in v2; per `rfc_cycle.md`, the synthesis should be authored by someone other than the v2 author.
