# PASS 2 — Phase 2 Report — Lane ESTIMANDS (full repo)

Reviewer: isolated ESTIMANDS lane. HEAD per phase0-triage
(`9c16cd2…`, working tree v0.2.0). Evidence labels: EMPIRICAL (seeded check),
DEFINITIONAL (source-derived fact), VALUE (governing-clause reading). Checks
under `inst/design/audit/pass2/checks/`, prefix `p2_lane1_`.

Checks run this phase (both ALL PASS):
- `p2_lane1_metric_regime.R` → `.txt` — metric-regime enforcement (central Q),
  no-cross-scoring on heavytail, truth-well-posedness. 0 failures.
- `p2_lane1_reconfirm_typed_scoring.txt` — re-run of salvaged
  `inst/design/audit/checks/lane1_check1_typed_scoring.R` at this HEAD (oracle
  exactness / no-cross-scoring / airlock). 0 failures.

---

## 1. Reconstructed claim (carried; one revision)

**Carried.** CausalStress identifies where estimators succeed, degrade, and fail
under controlled *synthetic* stress **without misleading comparisons across
estimands, populations, failures, or DGPs**, via typed, no-cross-scoring over a
governed estimand registry (Const. §1.1/§1.7; `R/cs-contracts.R`). Truth is the
noise-free structural τ(X), never the sample mean of realized y1−y0 (§1.3/§1.5).
Tiers: reproducible ≠ scientifically valid ≠ externally representative.

**Revision to my Phase-1 §4 (sharpened, not reversed).** Phase 1 said the
machine-readable `stress_profile.target` is `"both"` and nothing structurally
prevents naively ranking a no-mean ATT. Phase 2 confirms this empirically AND
adds a second machine layer I had not traced: a `dgp_noise_family` tag *does*
exist in provenance (`R/cs-runner.R:508`, `R/cs-accessors.R:141`), but it labels
both `synth_heavytail` (Cauchy, no mean) and `synth_qte1` (Student-t df=4, finite
mean) as `"heavy"` (`R/cs-dgp-executable-meta.R:28-32`; `R/cs-dgp-registry.R:95`),
so it is **not** a valid mean-estimability signal. Net: the "use QST here"
steering is prose-only on the scoring path, and the nearest machine proxy is
disconnected from scoring and non-authoritative. (See F1–F3.)

---

## 2. What is scientifically sound (each with check / citation)

**S1 — Oracle exactness (EMPIRICAL).** `est_oracle_att` scores ATT and ATE with
error `0.000e+00` vs runner-recorded truth over 5 seeds on `synth_baseline`
(`p2_lane1_reconfirm_typed_scoring.txt`), and `max|err| = 0.00e+00` over 20 seeds
on `synth_heavytail` (`p2_lane1_metric_regime.txt`, Part 3). The truth layer is
exact on both the well-posed and the heavy-tailed DGP.

**S2 — No-cross-scoring holds on both DGPs (EMPIRICAL).** Unproduced targets
become `non_comparable`/`estimator_not_produced` with NA estimate/truth/error;
`cate` becomes `non_comparable`/`target_not_implemented`; a CATE-only task
hard-rejects with `causalstress_target_not_implemented_error`
(`p2_lane1_reconfirm_typed_scoring.txt`, Parts C2/C3). On `synth_heavytail` the
full-target run emits exactly `{estimator_not_produced, target_not_implemented}`
and no scored row for an unproduced target (`p2_lane1_metric_regime.txt`,
Part 2). Mechanism is DGP-agnostic: `cs_build_score_surface`
(`R/cs-contracts.R:373-491`) branches only on produced/truth-available, and
`cs_make_score_row`→`cs_check_non_comparable_reason` (`:337-339`, `:111-126`)
rejects any reason outside the fixed vocabulary. ATT-truth ≠ ATE-truth on every
baseline seed (finite-sample gap ≈ +0.10; population separation recorded via
`scoring_population_id` `treated` vs `full_generated_run_sample`).

**S3 — Airlock (EMPIRICAL).** Default (no-grant) airlock drops
`y0/y1/p/structural_te`, keeping only `y,w,X1..X5`
(`p2_lane1_reconfirm_typed_scoring.txt`, Part D).

**S4 — Truth definitions & conditioning populations (DEFINITIONAL).** ATT =
`mean(structural_te[w==1])` on `treated` (`R/cs-truth.R:9-11`;
`R/cs-contracts.R:18-27`); ATE = `mean(structural_te)` on
`full_generated_run_sample` (`R/cs-truth.R:21-23`; `:28-36`); QST distributional
on `treated` via runner-tau-grid (`:37-45`); CATE unit-level on held-out-eval
(`:46-54`). Populations are carried on every score row and recorded distinctly
(reconfirm C2).

**S5 — The heavytail design is SOUND, not a defect (intent-first).** The DGP's
declared purpose is *"Penalize estimators that rely on L2 loss under heavy-tailed
noise … any estimator that minimizes squared error is theoretically ill-posed and
will suffer variance explosion"* (`synth_heavytail.Rmd` §2, §4 "Moment note",
§8). The observed instability of the mean-based `lm_att` (|error| median 0.354,
**max 38.6** across 20 seeds vs baseline max 0.065; `p2_lane1_metric_regime.txt`
Part 3) **is that demonstration succeeding**, while the oracle stays exact. This
is the pass-1 error class; it is **not** re-filed here.

**S6 — Structural TRUTH is well-posed on heavytail (DEFINITIONAL + EMPIRICAL).**
`τ(X)=1+0.5·X1` is deterministic; the Cauchy mixture enters only `y0/y1/y`
(`R/dgp-synth-heavytail.R:52-68,85`). Hence `true_att=mean(τ[w==1])` and
`true_ate=mean(τ)` are finite by construction — confirmed finite on all 20 seeds,
range [1.0695, 1.1449] (`p2_lane1_metric_regime.txt`, Part 3), matching
`synth_heavytail.Rmd` §5 ("well-defined even when E[Y] is not").
`cs_truth_available_targets(heavytail)` = `{att, qst, ate}`.

**S7 — QST interpretation (DEFINITIONAL).** QST is the distributional,
treated-population, superpopulation-oracle target (`R/cs-contracts.R:37-45`;
`synth_heavytail.Rmd` §5, oracle MC via `cs_get_oracle_qst`). It is the estimand
whose truth remains defined under heavy tails and is the intended robust
comparison target for this DGP.

**S8 — CATE design (DEFINITIONAL).** Registered but Wave-1
`target_not_implemented`; unit-level, held-out-eval population
(`R/cs-contracts.R:46-54`); CATE-only tasks abort pre-execution via
`cs_abort_target_not_implemented` (`:147-159`; reconfirm C3). Non-comparable, not
silently mis-scored.

---

## 3. Findings (4; within the 3–7 typical band)

### F1 — Metric-regime inappropriateness of mean-ATT on no-mean DGPs is prose-only, never machine-flagged (CENTRAL)
- **Claim (file/func/clause).** On `synth_heavytail`, a mean-targeting estimator's
  ATT score row is `score_status="scored"`, `non_comparable_reason=NA`, and
  **semantically byte-identical** to the Gaussian baseline row (only numeric
  value fields and identity fingerprints differ). The vocabulary term
  `metric_invalid_for_regime` (`R/cs-contracts.R:99-109`,
  `cs_non_comparable_reasons`) is **never emitted anywhere in R/** — the only
  hit is its own definition (repo-wide grep). `cs_build_score_surface`
  (`R/cs-contracts.R:373-491`) has no moment-regime branch. The "use QST, not
  mean-ATT, here" steering lives only in prose (`synth_heavytail.Rmd` §2/§4/§8;
  Registry §2.2 "Penalize L2/MSE"; Const. §1.3 prohibition).
- **Type + evidence.** EMPIRICAL — `p2_lane1_metric_regime.R` /
  `.txt`, Parts 1–2: ATT rows identical status across the two DGPs; differing
  columns = `{estimate, error, abs_error, fingerprints, truth_version, dgp_id}`
  only; no regime/difficulty column exists in the 35-col surface; emitted reasons
  = `{estimator_not_produced, target_not_implemented}`;
  `metric_invalid_for_regime` absent. A user ranking mean-ATT with |error| up to
  38.6 gets a clean `"scored"` row and zero machine signal.
- **Tier threatened.** Scientifically-valid tier / the claim's "without misleading
  comparisons across estimands." (Reproducibility tier is intact.)
- **v0.3.0 bearing.** Wire an authoritative moment-regime gate so mean-target
  rows on no-mean DGPs emit `metric_invalid_for_regime` (the vocabulary slot
  already exists), or otherwise make the doc-steering machine-checkable.
- **Action class.** RFC (design the `moment_regime.mean_exists/att_estimable` →
  `metric_invalid_for_regime` wiring; the enabling Families-spec machinery is
  unshipped/deferred per README). Escalate to constitutional amendment only if
  §1.3's prohibition is to be *enforced* rather than documented.

### F2 — DGP metadata does not single out QST as the intended robust target on heavytail
- **Claim (file/clause).** `synth_heavytail.yml` declares
  `stress_profile.target: "both"` (line 11), identical to `synth_baseline.yml`
  (line 16). No per-(dgp,version) field marks ATT/ATE as inappropriate or QST as
  the required comparison target. The intended estimand is stated only in prose
  (`synth_heavytail.Rmd` §5/§8).
- **Type + evidence.** DEFINITIONAL — sidecar values (`synth_heavytail.yml:11`)
  cross-read against the prose intent; consistent with F1's empirical result that
  nothing on the scoring path narrows the estimand.
- **Tier threatened.** Externally-representative / valid tier for a *machine*
  consumer that filters on sidecar `target`.
- **v0.3.0 bearing.** Add an authoritative estimand-appropriateness field to the
  sidecar/registry (e.g. `target: "qst"` or an explicit `mean_estimable: false`)
  for no-mean DGPs.
- **Action class.** DGP scientific validation (+ future spec ticket to define the
  schema field).

### F3 — The only shipped machine noise tag (`dgp_noise_family`) is a non-authoritative estimability proxy and sits off the score surface
- **Claim (file/func).** `dgp_noise_family` is attached to provenance
  (`R/cs-runner.R:508`) and exposed via `R/cs-accessors.R:141`, but
  `R/cs-dgp-executable-meta.R` maps **both** `synth_heavytail` (Cauchy → no mean,
  line 28-29) and `synth_qte1` (Student-t df=4 → finite mean; `R/cs-dgp-registry.R:95`)
  to `"heavy"` (lines 28-32). So `noise_family=="heavy"` does **not** imply
  "mean undefined" and cannot gate ATT estimability; and it is absent from the
  score surface (F1 column list), so it is not part of any scoring verdict.
- **Type + evidence.** DEFINITIONAL/VALUE — source mapping (`cs-dgp-executable-meta.R:28-32`,
  `cs-dgp-registry.R:95`); supported by `p2_lane1_metric_regime.txt` Part 1
  (surface column list) and Part 3 (heavytail truth available for att/ate).
- **Tier threatened.** Valid / externally-representative — a machine consumer
  filtering on the existing tag would wrongly exclude qte1's *valid* mean-ATT (or
  gain no regime information on the scoring path).
- **v0.3.0 bearing.** Introduce an explicit `mean_exists` / `att_estimable`
  moment-regime flag distinct from noise family, and (if used for scoring) put it
  on the score surface. Same RFC as F1.
- **Action class.** Future spec ticket (moment-regime schema), converging with F1's RFC.

### F4 — Audit prose conflates ill-posed ESTIMATOR with well-posed TRUTH (audit-hygiene, not a product defect)
- **Claim (file/clause).** The pass-1/triage shorthand "mean-based ATT is
  deliberately ill-posed" (phase0-triage §Why-a-second-pass ¶1) risks reading as
  the *target* being undefined. Precisely: on heavytail the structural **truth**
  ATT/ATE is finite and well-posed (S6); only the mean-based **estimator** is
  ill-posed (infinite variance). The product itself states this correctly
  (`synth_heavytail.Rmd` §5), so there is **no code defect** — the imprecision is
  in audit narrative that could propagate to v0.3.0 reviewers.
- **Type + evidence.** DEFINITIONAL — `R/cs-truth.R:9-23`; `R/dgp-synth-heavytail.R:52-85`;
  `p2_lane1_metric_regime.txt` Part 3 (truth finite; estimator explodes).
- **Tier threatened.** None of the product tiers; threatens audit correctness.
- **v0.3.0 bearing.** Keep the target/estimator distinction explicit wherever the
  heavytail case is cited (it is the precise framing for F1).
- **Action class.** Documentation only.

---

## 4. No material finding (checked and clean)

- **Finite-vs-superpopulation coherence.** ATT/ATE are declared finite-sample
  over the observed run sample (`evaluation_policy="observed-run-sample"`), QST
  superpopulation-oracle, CATE held-out-eval — each carried per row with no
  cross-population leakage observed (reconfirm C2; contracts `:18-54`). No finding.
- **Truth recomputation integrity.** Independently recomputed ATT/ATE truths
  match the scored truths **bitwise** on all 5 baseline seeds
  (`p2_lane1_reconfirm_typed_scoring.txt`, Part B). No silent truth substitution.
- **No silent cross-scoring on any DGP, incl. heavytail.** Every requested-but-
  unproduced/unimplemented target resolves to a typed `non_comparable` row with
  NA value fields; no `"scored"` row ever appears for an unproduced target
  (Parts C, and Part 2 on heavytail). No finding.
- **Vocabulary integrity.** `cs_check_non_comparable_reason` (`R/cs-contracts.R:111-126`)
  hard-rejects any reason outside the fixed set, so no ad-hoc reason can leak;
  the only issue is a *declared-but-unused* slot (F1), not an unvalidated one.

---

## Packet-level caveat (stated once, not a finding)

All 12 DGP IDs / 24 versions are synthetic; the benchmark measures behavior under
*controlled synthetic stress*, so external representativeness is a tier caveat for
the whole packet, not a lane-specific estimand finding.
