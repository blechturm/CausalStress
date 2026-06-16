# RFC Synthesis: Estimand Registry, Typed Scoring, ATE, and CATE

**Status:** ACCEPTED by maintainer on 2026-06-16 (final review complete; binding per `rfc_cycle.md`, incorporating Revision 1 and Revision 2 fixes). Gating next act: ratify Constitution 2.0.0.
**Date:** 2026-06-16
**RFC:** RFC-1 of the v0.2.0 estimand / UX / families program
**Synthesis author:** Claude (assistant) — distinct from the seed-v2 author (Codex), per the rotation rule
**Synthesizes:** `..._seed.md` (v1) → `..._response.md` (Codex) → `..._response_review.md` (adjudication) → `..._seed_v2.md` (Codex)

**Revision 1 (final review, 2026-06-16):** Patched in place per `rfc_cycle.md` (post-synthesis fixes caught during final review). Five findings accepted: **(B1)** the Article I/III amendment must *preserve* the existing real-DGP external-truth clauses (Art. I §1.3 "Real Data"; Art. III §3.2.B) and trim only the *generalized* external tier — §1.10/§2; **(M1)** eval-sample identity (`seed_eval`/`n_eval`) belongs in the score/prediction fingerprint, not `fit_fingerprint`, except for declared transductive estimators — §1.5/§1.7; **(M2)** the CATE predict-input airlock is "covariates + runner-issued `unit_id`", not "covariates-only" — §1.7; **(M3)** score records are keyed by `(estimand_target_id, metric_id)` / `score_fingerprint`, not `estimand_target_id` alone — §1.2/§1.5; **(M4)** RFC-2 sequencing clarified (RFC-2a scalar after Wave 1; CATE UX experimental until Wave 2) — §1.13/§4. B1 was the only blocking item; it is resolved here, so the synthesis is now accept-ready.

**Revision 2 (final review, second pass, 2026-06-16):** A second final-review pass found that Rev 1 fixed the body but left §4 (the acceptance record) with stale wording, and surfaced two real clarifications. Fixes: **(B)** §4 item 4 now states the eval predict airlock as "covariates + runner-issued `unit_id`" (was the stale "covariates-only") — matching §1.7; **(minor)** §4 item 2 now mirrors the precise body wording (preserve existing real-DGP clauses; trim only the generalized tier); **(major)** output keying clarified — one prediction population per (task, target) in v0.2.0, with `prediction_fingerprint` as the documented future extension (§1.2); **(major)** added the `target_not_implemented` non-comparable reason and a two-wave interim rule so CATE never appears scoreable before Wave 2 (§1.4, §1.11). With §4 corrected, the acceptance record and body agree.

## Rotation and verification note

Authorship rotation across this cycle: seed v1 = Claude; response = Codex; response-review = Claude (seed-author adjudication, with the four contested downgrades tie-broken by the maintainer and a verified prior-art pass); seed v2 = Codex; **synthesis = Claude** (≠ v2 author). Final review is the maintainer's and is the only binding act.

I re-confirmed seed-v2's "Verified Baseline Facts" against source during this cycle (GRF discards the forest after the scalar extract; `cs_true_att` exists but no `cs_true_ate`; resume dispatch branches missing/1/2/3 then hard-errors; capability representation is split across `supports_qst`, oracle metadata, and output-meta `capabilities`/`target_level`; row/tidy/science surfaces are ATT/QST-shaped). They hold.

Seed-v2 resolves every blocker from the response-review (F5+N2, F9, F10, F13) and every required ruling (F1, F2, F3, F4). This synthesis therefore **does not send it back for another cycle**; it decides the points seed-v2 left open and recommends acceptance with a bounded set of amendments.

---

## 1. Recommended decisions

Accept seed-v2's architecture. The following are the synthesis's binding-candidate decisions; items marked **[decided]** resolve a seed-v2 open option, **[amend]** modifies seed-v2, **[accept]** endorses it.

1. **Estimand set — [accept].** ATT, ATE, QST, CATE as defined in seed-v2 §3. ATE and CATE truth reuse `structural_te` (mean over all / per-unit); QST keeps the oracle. CATE truth = conditional-mean structural τ(X), not realized Y¹−Y⁰.

2. **Output contract shape — [decided], and the single highest-blast-radius choice.** Seed-v2 §13 proposes `function(df, tau, config) -> list(outputs, meta)` with `outputs` a typed collection, vs. the implicit alternative of additive named slots (`list(att, qst, ate, cate, meta)`). **Recommend the generic `outputs` collection keyed by `estimand_target_id`.** Reason: the entire point of RFC-1 is a *typed, extensible* estimand registry; named slots recreate the "one slot per estimand" pattern that does not scale and cannot host CATE's per-unit keyed shape in a scalar slot anyway. The legacy `att`/`qst` slots survive as runner-normalized compatibility shims (seed-v2 §13), which makes the break tolerable for existing estimators. **This is the decision most worth the maintainer's explicit sign-off** — it changes the contract every estimator implements. **Keying (Rev 1, M3):** estimator *outputs* are keyed by `estimand_target_id` (one prediction per target); *score records* are keyed by `(estimand_target_id, metric_id)` / `score_fingerprint`, because one target (notably CATE) carries multiple metrics (PEHE + median-absolute error). Do not key score records by target id alone. **Prediction population (Rev 2, M from final review #2):** in v0.2.0, **a task produces one prediction population per target** — so output keying by `estimand_target_id` is unambiguous within a task, and the held-out prediction population is recorded in `score_fingerprint` (scoring population id). If a later release needs multiple eval populations per fit (e.g. several held-out draws of one fit), introduce a `prediction_fingerprint` and key CATE outputs by `(estimand_target_id, prediction_fingerprint)`; that extension is out of v0.2.0 scope.

3. **`estimand_target` descriptor (O1) — [decided]: Option A.** Canonical R list is authoritative; a derived `estimand_target_id` string is used for filenames/pins/compact tables. The descriptor must not be reduced to a bare string (prevents the false-equivalence failure mode F8).

4. **Three-way scoring join — [accept], with one added reason (Rev 2).** `scoreable = requested ∩ estimator-produced ∩ DGP-truth-available`; explicit machine-readable non-comparable records (`estimator_not_produced`, `truth_unavailable`, `metric_invalid_for_regime`, `ci_unavailable`, `gate_unimplemented`, `not_requested`, **`target_not_implemented`**). This structurally closes the C4 defect class and handles real DGPs (F13). The added `target_not_implemented` reason covers an estimand the Constitution declares valid but the runner has not yet implemented — see the two-wave interim rule (§1.11).

5. **Fit/score identity — [accept] with one correction (Rev 1, M1).** `fit_fingerprint` (data/model/config) + `score_fingerprint` (fit + estimand_target + metric_id + truth version + scoring population + CATE prediction digest). This is the cleanest part of v2; it enables one fit → multiple scored estimands and score-layer recompute without re-fitting (F9). **Correction:** eval-sample identity (`seed_eval`, `n_eval`, derivation rule) must live in `score_fingerprint` (the prediction layer), **not** `fit_fingerprint` — a changed held-out sample must not invalidate the model fit. The *only* exception is an estimator that explicitly declares **transductive** fitting (consumes eval covariates at fit time), for which eval identity is genuinely part of the fit and enters `fit_fingerprint`.

6. **Schema-4 migration matrix — [accept] with two additions.** Adopt seed-v2 §7 verbatim, plus: **(a)** `unit_id` assignment must be **deterministic** (e.g. generation-order index under the mandated RNG) and enter the eval-sample identity/fingerprint — otherwise CATE scoring is not bitwise-reproducible (Art. II); **(b)** see §1.7 for the eval-set airlock gap that the migration/schema work must also cover.

7. **CATE protocol — [accept] with two corrections (Rev 1).** Held-out evaluation by default (F1 ruling, SOTA), keyed `unit_id` (F2), `seed_eval` derived from `(seed_train, dgp_id, dgp_version, "cate_eval")`. **(Rev 1, M1)** eval-sample identity enters the **score/prediction fingerprint, not `fit_fingerprint`** (per §1.5), except for declared transductive estimators. **(Rev 1, M2)** the predict-input airlock is **covariates + the runner-issued `unit_id`** — the synthetic key is not truth and is required for keyed output — excluding `y`, `w`, `y0`, `y1`, `p`, `structural_te`, and distinct from the training airlock. Seed-v2 §9 specified the scorer-truth channel but not the predict-input columns; this closes it.

8. **Metric vocabulary — [accept].** PEHE primary + required robust companion (`cate_median_abs_error`); trimmed RMSE / quantile loss / calibration optional. Per-regime validity envelopes delegated to RFC-3 (F4).

9. **Gatekeeper — [accept].** Pluggable per-estimand structure now; ATT/QST gates remain the legacy baseline; ATE gets a scalar gate *slot*; **CATE placebo = "Unverified" unless a heterogeneity-detection test is implemented** (the verified F3 posture). Calibration policy, difficulty tiers, and the `Non-Robust` label are deferred to the Gatekeeper-recalibration RFC (`horizon.md`). Seed-v2 must not bake the current too-harsh whole-suite policy into the new gates — it does not.

10. **Constitution 2.0.0 — [accept the bump], [amend the scope].** Endorse a **major** bump: the Article III §3.1 output-signature change is semantic and cannot be made additive even by restructuring (it changes the contract every estimator implements), and Article IV's "exclusively ATT and QST" scope changes. Article I/III/IV amendment text per seed-v2 §13 is the right direction. **Amendment (Rev 1, B1 — corrected for precision):** do **not** introduce a *generalized* external truth tier spanning all estimands yet — **but preserve the existing real-DGP external-truth clauses verbatim**: Art. I §1.3 "Real Data" (truth defined externally, never regenerated, the Stabilized Experimental Estimate) and Art. III §3.2.B (real DGPs return `true_att` numeric-or-`NA`, `true_qst = NULL`, `structural_te = NULL`). The Article I/III amendment MUST NOT narrow or delete those. Real-DGP truth *absence for the new estimands* (ATE/CATE) is handled by the three-way join's `truth_unavailable` non-comparable records; the generalized external tier for those estimands defers to a future real-data RFC that builds it.

11. **Implementation staging (O3) — [decided]: Option A (two waves), one amendment.** Ratify the full Constitution 2.0.0 amendment once, but implement in two waves: **Wave 1** — typed-scoring contract + `outputs` shape + fit/score split + schema-4 migration + **ATE** (the near-free scalar that exercises all the new machinery at low risk). **Wave 2** — **CATE** (held-out + keyed + new metrics + per-unit storage), on the proven Wave-1 infrastructure. Reason: CATE is the high-risk surface; landing it on already-shipped typed-scoring is far safer than co-shipping. Do **not** ship CATE on the current scalar-shaped infrastructure (seed-v2 §15 O3 agrees). **Interim rule (Rev 2):** because the full 2.0.0 amendment (which *declares* CATE a valid estimand) ratifies before CATE is implemented, Wave 1 must not let CATE appear scoreable — a CATE request before Wave 2 is emitted as a non-comparable record with reason `target_not_implemented` (§1.4), or hard-rejected. This prevents the "Constitution says valid, runner can't score" gap from silently producing empty or mis-handled results.

12. **Multi-estimand requests (O2) — [decided]: Option A, staged.** One task may request multiple estimands sharing a `fit_fingerprint`. Implementation may first normalize legacy one-target tasks into the fit/score shape (Wave 1), then enable true multi-target emission once the score-record plumbing is proven.

13. **RFC-2 freeze interface (O4) — [decided], staged (Rev 1, M4).** Split RFC-2 into **RFC-2a** (the scalar/typed-scoring collector/plot UX), which **may freeze after Wave 1**, and the **CATE UX portion (RFC-2b)**, which stays **experimental until Wave 2** fixes the per-unit score-record shape. The "converge then freeze" happens in two stages — safer, and consistent with the API-stability goal. This is the consistent reading of the ratification path in §4.

---

## 2. Rejected alternatives

- **Additive Constitution 1.9.0 (preserve old text, add sections).** Rejected: the Art. III §3.1 output-signature change is intrinsically semantic; no restructuring makes "every estimator's return contract changed" a clarification. 2.0.0 is honest.
- **Additive named output slots (`list(att, qst, ate, cate, meta)`).** Rejected in favor of the generic `outputs` collection (§1.2): named slots don't scale and can't host CATE's keyed per-unit shape.
- **In-sample PEHE as the headline CATE metric.** Rejected per the F1 maintainer ruling and SOTA (IHDP/Shalit et al.): held-out is the convention; in-sample is a secondary diagnostic only.
- **Per-unit placebo gate (τ̂(Xᵢ)≈0 for all i).** Rejected (F3, verified): ill-posed for a unit-level estimand; the principled form is a heterogeneity-detection test, deferred.
- **A *generalized* external truth tier spanning all estimands now.** Rejected/deferred (§1.10) — **without touching the existing real-DGP external-truth clauses** (Art. I §1.3, Art. III §3.2.B), which are preserved verbatim. The generalized tier for ATE/CATE waits for a real-data RFC; real-DGP truth absence is handled by `truth_unavailable` records.

---

## 3. Open risks

- **Output-contract break magnitude.** The generic `outputs` shape is the biggest single change; the compatibility shim mitigates but does not eliminate adapter churn. Mitigation: Wave 1 ships the shim + ATE; every existing estimator is migrated/validated before Wave 2.
- **Held-out CATE cost.** A second DGP draw per CATE task doubles generation cost and adds the `seed_eval`/`n_eval` fingerprint surface. Accepted per ruling; flagged for the implementation packet to keep eval samples bounded and digests-by-default.
- **Eval-set airlock (newly surfaced, §1.7).** If the predict-input columns aren't explicitly airlocked, CATE evaluation could leak truth. Must be specified in the implementation packet; treat as a contract test.
- **Deterministic `unit_id` (§1.6a).** Non-deterministic keys would break Art. II same-substrate reproducibility for CATE. Must be pinned and fingerprinted.
- **Two-stage RFC-2 freeze.** The API isn't fully frozen until Wave 2; document that the CATE surface is experimental in the interim so users don't build on an unfrozen shape.
- **Gatekeeper deferral leaves CATE "Unverified."** Acceptable and honest (it's the verified posture), but means CATE estimators carry no placebo verdict until the recalibration RFC + a detection test land. State this plainly in user docs so "Unverified" reads as "by design," not "broken."

---

## 4. Maintainer decision

**Recommendation: ACCEPT WITH AMENDMENTS.** Seed-v2 is a sound, complete resolution of the blocker set; it does not need another full cycle. The amendments to record at acceptance:

1. **Output contract = generic `outputs` collection** (with legacy-slot shim) — the one item meriting explicit sign-off (§1.2). Outputs keyed by `estimand_target_id`; score records by `(estimand_target_id, metric_id)` / `score_fingerprint`; **one prediction population per (task, target) in v0.2.0** (Rev 2, §1.2).
2. **Constitution 2.0.0 — trim only the *generalized* external truth tier (spanning all estimands), and *preserve verbatim* the existing real-DGP external-truth clauses** (Art. I §1.3, Art. III §3.2.B). Generalized tier deferred to a future real-data RFC (§1.10).
3. **Two-wave implementation** — scalar typed-scoring + ATE first, CATE second; one ratified amendment (§1.11). **Interim rule (Rev 2):** before Wave 2, CATE requests are non-comparable with reason `target_not_implemented` (or hard-rejected) — never scoreable.
4. **Close two spec gaps** in the implementation packet: the CATE predict-input airlock = **covariates + runner-issued `unit_id`** (exclude `y`/`w`/`y0`/`y1`/`p`/`structural_te`) (§1.7), and **deterministic `unit_id`** (§1.6a).

**Ratification path.** Per `rfc_cycle.md`, the synthesis is binding only on maintainer acceptance, and the Constitution amendment requires its own ratification (preamble process; major bump to 2.0.0). The gating sequence: (a) maintainer accepts this synthesis (the four §4 amendments + the five Rev-1 final-review fixes, now folded into the text); (b) the Constitution 2.0.0 amendment (Articles I, III, IV — preserving the real-DGP external-truth clauses per B1) is ratified; (c) Wave 1 packet is cut (typed-scoring + `outputs` shape + fit/score split + schema-4 + ATE); **RFC-2a may freeze the scalar/typed-scoring UX after Wave 1**; (d) Wave 2 (CATE — held-out + keyed + metrics + storage); then the **CATE UX freeze (RFC-2b) and RFC-3 (families)** unblock against the now-frozen CATE contract.

## 5. Acceptance criteria met (audit against seed-v2 §17)

This synthesis satisfies the seed-v2 acceptance criteria: it accepts the 2.0.0 bump with amended scope (1); fixes the descriptor serialization to Option A (2); accepts the fit/score split (3); accepts the schema-4 matrix with two additions (4); accepts the three-way join + non-comparable schema (5); accepts held-out CATE + keyed `unit_id`, surfacing the eval-airlock gap (6); fixes the metric vocabulary (7); accepts the deferred-calibration gatekeeper interface (8); chooses two-wave staging (9); and defines the staged RFC-2 freeze surface (10). No implementation begins until acceptance + ratification.
