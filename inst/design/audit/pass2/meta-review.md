# PASS 2 — Phase 5 Meta-Review (fresh, independent, sidecar-aware)

**Reviewer:** independent Phase-5 control, not involved in Phases 0–3. Full repo
access. **Method:** spot-check load-bearing claims AT SOURCE (generators, sidecars,
`.Rmd` narratives, governance docs, captured check outputs); no findings applied;
nothing staged or committed. **Scope of authority (self-imposed per charge):** I may
only (a) confirm, (b) flag deliverable-vs-source discrepancies, (c) judge fair
representation / over-correction. I add no new scientific findings. Where I could not
verify, I say "unverifiable."

Deliverables read first: `synthesis.md`, `human-review-packet.md`, the three
`phase2-reports/*.md`, `phase0-triage.md`.

---

## (a) Confirmation rate

**8 / 8 load-bearing source claims CONFIRMED at source.** One collateral
**DISCREPANCY** surfaced *inside* the S1 downgrade rationale (heavytail is
mischaracterized as "experimental"; it is `status: "stable"`). The discrepancy does
not overturn any finding but does weaken one leg of the S1 severity-downgrade
argument (details in (c)).

The special charge — did pass 2 get the **intent-vs-defect** calls right, and did it
**over-read intent** to launder a genuine problem? — **PASS.** The two genuinely
intended designs (heavytail L2-break, hd_sparse constant shift) are SOUND-by-intent
at source, and the real residual (metric-regime enforcement gap) was **kept as a live
finding**, not waved away.

---

## (b) Item-by-item verdicts (with source lines)

### 1. heavytail is SOUND-by-intent, not a defect — **CONFIRMED**
- Noise mixture at source: `R/dgp-synth-heavytail.R:52-65` (v1.3.0) and `:136-152`
  (v1.6.0) — `eps ~ ifelse(Bernoulli(0.8), N(0, sd=0.5), Cauchy(0,1))` for both
  potential outcomes → `0.8·N + 0.2·Cauchy`. Matches the claim exactly.
- Declared intent: `synth_heavytail.Rmd:101-106` ("the Cauchy component makes the
  population mean of observed outcomes undefined, so any estimator that minimizes
  squared error is theoretically ill-posed") and the §4 **Moment note** `:145-147`
  ("any L2/MSE estimator is not well-posed in the population"). The mean-based
  instability is the *declared* demonstration ("L2 break").
- Empirics: `checks/p2_lane2_dgp_intent_output.txt:20-24` — across-seed sd of naive
  mean-ATT is **non-shrinking** (9.129 → 1.878 → 9.620 at n=1k/5k/20k) vs baseline
  ~1/√n (0.1051 → 0.0352 → 0.0241); structural `true_att=1.1188 finite=TRUE`; QST has
  **no NA**. So the mean-ATT does not converge while the ATT truth and QST truth stay
  finite/well-posed — exactly as the verdict requires. **Verdict justified.**

### 2. hd_sparse is SOUND-by-intent — **CONFIRMED**
- Design at source: `R/dgp-synth-hd-sparse-plm.R` — `tau <- rep(1, n)` (`:50,:127,:201`),
  `y1 <- y0 + 1` (`:51,:128,:209`), single shared `eps` used for `y0` only
  (`:47-48,:124-125,:198-199`). Roxygen `:16` declares `Y_1 = Y_0 + 1` as the design.
- Registry agreement: `CAUSAL_STRESS_DGP_REGISTRY_1.4.0.md:238` "Y_1 = Y_0 + 1"
  (the charge's §2 l.237-239). **CONFIRMED.**
- Immateriality to scoring — **CONFIRMED**: `structural_te ≡ 1`, so ATT =
  `mean(tau[w==1]) = 1` is noise-independent by construction; QST is a *marginal*
  treated-population target and the marginal of `y1` equals the marginal of `y0`
  shifted by +1 under **both** shared and independent noise, so QST = +1 identically.
  Only the *paired* (unit-level/CATE) difference differs, and that quantity is barred
  as truth (Const. §1.3, sample-mean-of-differences prohibition) and CATE is
  `target_not_implemented`. So the shared-vs-independent distinction touches **no
  scored truth**.
- Residual is documentation only — **CONFIRMED**: the sole issue is Registry §1.2's
  global "ε₀,ε₁ independent" (`REGISTRY:48`) not being reconciled with its own
  hd_sparse block (`:238`); Const. §1.4 already grants "unless explicitly specified."
  A doc-consistency note (S6/dgp-F2), not a scientific defect.

### 3. Live finding S1 (metric-regime prose-only) — **CONFIRMED** (with one discrepancy in the downgrade rationale)
- `metric_invalid_for_regime` is **defined** at `R/cs-contracts.R:103` (in
  `cs_non_comparable_reasons()`). **CONFIRMED.**
- Emitted **nowhere**: repo-wide grep of `R/` returns the single hit at `:103` (its
  definition). `cs_build_score_surface` (`R/cs-contracts.R:373-491`) branches only on
  `failure_status`, `cate → target_not_implemented`, `not-produced →
  estimator_not_produced`, `no-truth → truth_unavailable`, then att/ate/qst scoring —
  **no moment-regime branch**. **CONFIRMED at source.**
- Scored identically to baseline: `checks/p2_lane1_metric_regime.txt:23,24,61,62` —
  heavytail and baseline ATT rows both `score_status="scored"`,
  `non_comparable_reason=NA`; only value/fingerprint/`dgp_id` cols differ (`:88-90`).
  `checks/p2_lane3_metric_regime_output.txt:10-18,27-35` — both DGPs: `scored=20`,
  reason `NA=20`, `metric_invalid_for_regime` count `= 0`. **CONFIRMED.**
- **Downgrade fairness — mostly fair, but one leg is factually wrong.** The
  downgrade to "sound-with-boundary / SPEC-DECISION" rests on three legs in
  `synthesis.md:54-56`: (i) *DGPs are maintainer-gated* — **CONFIRMED**, Const. §1.2
  "Only the core maintainers may define new DGP IDs" (`CONSTITUTION.md:40`); (ii) *the
  Families spec designs the fix* — **CONFIRMED** (`moment_regime$rmse_valid`,
  `cs_summarise_stress`, cited by all three lanes); (iii) *heavy-tail DGPs are
  `experimental`* — **DISCREPANCY: FALSE.** `inst/dgp_meta/synth_heavytail.yml:3`
  declares `status: "stable"`, and Const. §5 (`CONSTITUTION.md:333`) defines *stable*
  as "validated, correct, and **recommended**." heavytail is the *exact* no-mean DGP
  where the gap bites, and it is stable/recommended, not experimental. The same error
  is repeated in `human-review-packet.md:69` ("the current maintainer-only,
  **experimental-status**, prose-documented state"). **Net:** the severity call
  ("missing guardrail, not a wrong number" — the truth layer is exact, no false number
  is produced against a valid truth) survives on legs (i) and (ii) and on the verified
  exactness of the oracle; but the "experimental" mitigation must be struck. Because
  S1 remains a *listed live finding with a SPEC-DECISION action*, this is a softened
  rationale, **not** a laundered defect.

### 4. Survivorship S2 empirically demonstrated — **CONFIRMED**
- `checks/p2_lane3_survivorship_output.txt:4-14` — `cs_run_grid` with seeds 1:8 →
  4 success / 4 fail (per-seed flags shown); `:26-27` summary reports `n_runs = 8`
  with **no** success/failure column; `:29` `mean_error = NA` (a single failed seed
  NA-poisons it). Source corroboration: `R/cs-summary.R:58` `n_runs = dplyr::n()`
  (counts attempted rows), `:61-64` error columns carry **no `na.rm`**, `:65-66`
  coverage/width use `na.rm=TRUE`; there is no `n_success`/`n_failed` column.
  **CONFIRMED at source.**
- Deferral: `horizon.md:535-542` (item 6, "extend the public summaries … report
  planned/attempted/succeeded/failed/timeout/missing denominators instead of silent
  `na.rm = TRUE` means — the survivorship gap"); also `:308-311`. **CONFIRMED.** The
  pass-1 probe was indeed defective; this one exercises `cs_run_grid` correctly.

### 5. sd_error correction held — **CONFIRMED**
- `R/cs-summary.R:62` `sd_error = stats::sd(att_error)`. Pass-2 docs correctly
  withdraw pass-1's "no MC uncertainty" (`synthesis.md:36-38`, `statistics.md:73-77`,
  `estimands`/`statistics` S5), narrowing the residual to the SEM/CI/rank-stability
  *inferential* signal only (S3/statistics-F3). **CONFIRMED.**

### 6. Registry staleness S4 (overlap coefficient) — **CONFIRMED**
- Live code (latest v1.6.0): `R/dgp-synth-overlap-stressed.R:202`
  `p = plogis(9.0*X1 + 9.0*X2)`. Registry: `CAUSAL_STRESS_DGP_REGISTRY_1.4.0.md:140`
  `p(X) = plogis(3.0 X_1 + 3.0 X_2)`. Code **9**, Registry **3** — stale.
  **CONFIRMED.** (Spot-checked the one requested; the other three drifts —
  hd_sparse p50/ρ0.5 → 100/0.95, placebo_tilted 1.0/1.2 → 0.6/0.8 — are also
  source-consistent per `REGISTRY:216,219,284` vs the generators.)

### 7. Caps — **CONFIRMED**
- Synthesis: **7** findings (S1–S7) ≤ 10; **2** "cannot-decide" experiments ≤ 6.
- Phase-2 reports: estimands **4** (F1–F4), statistics **3** (F1–F3), dgp **5**
  (F1–F5) — each ≤ 10. **CONFIRMED.**

### 8. Packet resolves NO value item — **CONFIRMED**
- `human-review-packet.md:77` "The review takes **no position** on these"; families
  vs. CATE, enforce-now-vs-defer, and scope-extension are all reserved to the
  maintainer (`:62-77`, `synthesis.md:140-142`). **CONFIRMED.**

---

## (c) Discrepancies

**D1 (material to a rationale, not to a verdict).** `synthesis.md:55` and
`human-review-packet.md:69` assert heavy-tail DGPs are **experimental**;
`inst/dgp_meta/synth_heavytail.yml:3` says `status: "stable"` and Const. §5
(`CONSTITUTION.md:333`) defines *stable* = "validated, correct, and recommended."
This is the DGP on which the S1 metric-regime gap actually manifests. Effect: one of
three legs of the S1 severity-downgrade is false; the downgrade should be re-stated on
its surviving legs (maintainer-gated authorship; families-designed fix; exact truth
layer / no wrong number). It does not resurrect S1 as a correctness defect, but a
"stable, recommended" DGP with an unguarded no-mean mean-ATT is a modestly *stronger*
case for wiring the token now than the synthesis's gloss implies.

**Minor wording note (not a defect):** the human-review-packet's check index names
`p2_lane1_metric_regime_output.txt`; the file on disk is
`checks/p2_lane1_metric_regime.txt`. Content matches; only the filename cite is
imprecise.

No other deliverable-vs-source discrepancies found. All numeric empirics quoted in
the synthesis and lane reports that I spot-checked (non-convergence sds, scored-row
identity, `metric_invalid_for_regime` count 0, survivorship 8/4, overlap 9-vs-3) match
their captured outputs and the source.

---

## (d) Fair-representation & over-correction judgment

**Fair representation: YES.** The synthesis S1–S7 map cleanly onto the three lane
reports (S1 = estimands-F1 = statistics-F1 = dgp-F4; S2 = statistics-F2; S3 =
statistics-F3; S4 = dgp-F1; S5 = dgp-F4; S6 = dgp-F2; S7 = dgp-F5), and the CATE
carry-forward is represented as a reserved value item, not a defect. Empirical claims
are faithful to the captured `*_output.txt` files and to source. The convergence of
three isolated sidecar-fed lanes on the intent-aware reading is real, not manufactured.

**Over-correction: NO (with one caveat).** The critical risk this pass was charged to
avoid — over-reading intent to dismiss a *real* problem — did **not** occur:
- The genuinely intended behaviors (heavytail non-convergence; hd_sparse constant
  shift) are SOUND-by-intent **at source**, so re-classifying them as non-defects is
  correct, not laundering.
- The real residual — that no machine guardrail stops a naive mean-ATT ranking on the
  no-mean DGP — was **retained as a live finding (S1) with a SPEC-DECISION action**,
  not waved into "intended." Pass 2 explicitly separated the *intended* instability
  (sound) from the *unenforced steer* (a finding). That is precisely the correct
  intent-vs-defect partition.
- Caveat: the severity of S1 was softened partly with a false "experimental" premise
  (D1). This is a rationale blemish, not a dismissal — the finding still stands and
  still routes to action.

## (e) Was the intent-first discipline applied soundly, or used to wave away anything real?

**Applied soundly.** Intent-first was used exactly where the sidecars/narratives
license it (Cauchy → no population mean → non-converging mean-ATT is the *declared*
demonstration; `Y1=Y0+1` shared noise is the *declared* design and is immaterial to
every scored truth), and it was **not** extended to excuse the one unguarded path.
The discipline corrected pass-1's under-reading (two false defects withdrawn on
verified grounds) without tipping into pass-2 over-reading. The only correction I
would require before human sign-off is to strike the "experimental" characterization
of heavytail wherever it appears (synthesis S1; packet §5) and re-anchor the S1
severity on its true legs — a stable, recommended, maintainer-authored DGP whose
metric-regime steer is prose-only until the families machinery (already designed)
wires it in.

**Bottom line:** the pass-2 scientific conclusions are supportable at source; the
intent-vs-defect calls are correct; the sole substantive fix is the heavytail
status mislabel in the S1 downgrade rationale.
