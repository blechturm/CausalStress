# CausalStress Horizon

**Status:** Non-binding parking lot

Items here are deliberately outside the active v0.1.10 scope unless promoted by
an accepted RFC or a future spec packet.

## Deferred Scientific Work

- New DGP families.
- Sensitivity analysis as a DGP stress axis.
- Additional estimators after ATT/QST contract repairs.
- Registry-spec version bump for known stale narrative entries.

### Estimand expansion (parked 2026-06-12; requires an RFC and Article I/IV amendment)

Candidate scope for a post-v0.1.10 "estimand registry" RFC:

- **ATE as a declared secondary scalar estimand.** Truth is nearly free
  (`mean(structural_te)` over all units vs. over treated), and the existing
  DGPs deliberately create ATT/ATE gaps (heterogeneous tau plus selection), so
  the gap is itself informative. Motivated directly by audit C4: the tmle
  wrapper natively targets ATE but the package has only ATT truth to score
  against.
- **Estimand-aware scoring join.** Make `estimand_target` a first-class
  declared estimator field that the runner joins on when scoring: an estimator
  is scored against its declared estimand's truth or marked non-comparable,
  never silently scored against a different estimand. This makes the C4 defect
  class structurally impossible rather than one-line-fixable.
- **CATE / unit-level tau(X) with PEHE.** The truth layer already carries
  unit-level CATE truth (`meta$structural_te`, Constitution Art. I). Missing
  pieces: a `target_level = "unit"` output schema (sketched in the archived
  v0.3.0 design's deferred list), PEHE/RMSE-over-tau metrics, and a CATE
  gatekeeper. Key asset: the sharp-null placebo suite (tau identically 0) is a
  natural CATE gatekeeper — any estimated heterogeneity on a placebo is
  hallucination — and is arguably more discriminating for CATE than for ATT.
- **Explicitly rejected for now:** marginal/unconditional QTE (QST already
  covers the distributional tier for the treated; document its QTT flavor
  instead); distribution of treatment effects, i.e. quantiles of `Y1 - Y0`
  (not identified without rank-invariance assumptions — incompatible with the
  unambiguous-truth principle); LATE/IV, mediation, and survival estimands
  (each needs new DGP families and identification machinery; a different
  package era).

Constitutional note: Article I defines exactly two truth tiers and Article IV
scopes the gatekeeper exclusively to ATT and QST, so any addition is a
constitutional amendment fed by an accepted RFC synthesis, not a feature
ticket.

## Deferred Inference Work

### Bootstrap CI validity for GenGC QST (parked 2026-06-14; routes through an RFC + GenGC's bootstrap RFC)

`est_gengc()` currently computes percentile CIs via an iid row bootstrap that
refits `GenGC::gengc()` per replicate (`cs_bootstrap_ci()`), gating each
dimension at 90% replicate success. The engineering is sound; the **statistical
status is not**, and this must be treated as experimental, not as valid
inference:

- **No theorem, structurally suspect — *not* "known invalid".** No result
  establishes *or* refutes nominal coverage for forest-based QST bootstrap; the
  honest status is **unvalidated and structurally suspect**. The structural
  doubt is bias-blindness: the percentile interval is centred on the forest's
  *biased* QST estimate, and every resample/jackknife replicate shares that
  bias, so the interval measures spread around the wrong centre. The
  adaptive-nearest-neighbour analogy — naive bootstrap fails for *fixed*-k
  matching (Abadie & Imbens 2008) — is a reason for caution **by analogy, not a
  theorem about forests** (corrected per Codex review 2026-06-14). The
  matching-bootstrap failure is now understood to hinge on fixed vs. diverging
  neighbour counts (arXiv:2410.23525); forests have *diverging* effective
  neighbours (growing leaves, many trees), so the pathology may not transfer.
  Treat as caution, never as proof of invalidity.
- **Heavy tails: keep the quantile-vs-mean distinction straight.** The QST
  *estimand* is heavy-tail-robust — that is the kill-plot result; the median is
  bootstrappable even for Cauchy, and Athreya's (1987) infinite-variance
  pathology applies to the *mean* estimators, not to GenGC QST. The bootstrap
  concern here is the *narrower* pair — forest bias and extreme-τ instability —
  not heavy tails killing the quantile. The families campaign runs exactly where
  both bite: expect high `qst_n_boot_fail` → `low_boot_success` NA CIs at extreme
  τ (honest behaviour, not a bug), and poor coverage-against-oracle where the
  bootstrap "succeeds".
- **Gatekeeper interaction (Art. IV).** If GenGC reports QST CIs it is judged by
  the placebo gatekeeper and can be marked **Non-Robust** in the registry on no
  theoretical basis. Art. IV §4.2.3 gives the clean posture: estimators without
  CIs are **"Unverified"**, not failed. **Decision for the heavy-tail robustness
  campaign: run GenGC with `ci_method = "none"`** so the kill plot is a clean
  point-estimation result with no inference asterisk.
- **The CI degradation is itself a separate result, not a robustness claim.** A
  dedicated *coverage* study (ci on, coverage scored against oracle across the
  tail-index grid) is expected to show every naive bootstrap degrading — mean
  estimators' CIs collapse at the variance boundary (Athreya 1987), GenGC's
  QST bootstrap degrades via bias-blindness. That figure motivates the GenGC
  DR-QST orthogonal-inference theory (Paper 2) and belongs in the stress-testing
  paper (Paper 3), explicitly framed as "naive bootstrap inference is unreliable
  here," never as a GenGC capability claim.
- **CausalStress is the empirical-calibration instrument — produce a coverage
  *map*, not a calibration *patch*.** Because the suite carries oracle truth, it
  can measure actual coverage and document a *validity envelope* (e.g. "GenGC QST
  bootstrap holds nominal coverage for df > 4, central τ, n ≥ 1000"). Three
  guardrails: (1) a coverage map is evidence conditional on the DGPs resembling
  reality, not a theorem — report "documented coverage on the suite," never
  "valid CIs"; (2) do NOT recalibrate intervals to hit nominal in the
  bias-dominated regime — the interval is mis-*centred*, not merely narrow, so
  widening (incl. BCa, which corrects bootstrap-distribution skew, not
  estimator-vs-truth bias) cannot reach the truth, and tuning a correction to
  this suite is teaching-to-the-test; (3) the map's job is to *locate and later
  confirm* the DR-QST theorem (show DR-QST CIs hitting nominal exactly where the
  plug-in fails), not to substitute for it. The envelope is a legitimate
  deliverable; a fudge-factor correction is not.

Cross-reference: `GenGC/inst/design/roadmap.md` bootstrap RFC queue and
`GenGC/inst/design/research/cfm_2013_positioning.md` (Route A, the resolved
"why consistency does not license the bootstrap" entry). Any move from
experimental to claimed inference here requires both an RFC in this repo and the
GenGC bootstrap theorem; it is not a feature ticket.

### DR-QST direction (GenGC RFC accepted 2026-06-14; narrow spike authorized)

Carried forward from the accepted synthesis
(`GenGC/inst/design/rfc/20260614_dr_qst_orthogonal_architecture_synthesis.md`),
parked here so the decisive campaign and Paper-2 work inherit them:

- **CFM comparator is required** in the decisive 200-seed campaign **if** the narrow
  GenGC spike (`GenGC/inst/design/spikes/dr_qst_shootout/`) shows signal: CRAN `Counterfactual`
  (Chen-Chernozhukov-Fernández-Val-Melly, the QST incumbent) or a threshold-regression
  baseline. Without it there is no answer to "why not the incumbent?" This resolves the
  earlier open question on adding `Counterfactual` to the benchmark: **yes, as the
  comparator, gated on spike signal.**
- **The CDF-nuisance construction is itself a research variable.** If forest-weight
  CDF extraction loses the spike, do **not** kill orthogonalization — try threshold
  (binary) models over a small y-grid or distributional regression before concluding
  option D. The bottleneck may be the CDF estimator, not the AIPW score.
- **Stabilizer/target drift.** Propensity clipping + CDF clip+rearrange target a
  stabilized object if not asymptotically negligible — fine for the package if
  documented, not for a theorem if hidden. Track in Paper 2.

## Deferred Tooling Work

### Spike: evaluate mirai (+ mori) as the parallel backend (parked 2026-06-12)

Candidate replacement for the current `future`/`furrr` execution layer:

- `mirai` / `mirai_map()` — minimalist async evaluation over NNG, now the
  backend behind `purrr::in_parallel()`; lower dispatch overhead than
  `future::multisession`, structured error values, built-in progress, and
  daemon-based workers that fit the campaign worker model.
  Reference: <https://mirai.r-lib.org/articles/mirai-map.html>
- `mori` — OS-level shared memory for R objects (zero-copy ALTREP via
  `share()`), integrating with `mirai`/`parallel`/`callr`; workers hold a
  ~1 KB reference instead of a per-worker copy. Same-machine only.
  Reference: <https://shikokuchuo.net/mori/>

Spike questions (each maps to a constitutional or audit constraint):

1. **RNG determinism (Art. II):** task results must be identical under serial
   and parallel dispatch, because workers re-seed per task via `cs_set_rng()`.
   Verify mirai daemons cannot perturb in-task RNG state, and that plan/task
   identity is backend-independent (interacts with the schema-3/RNG design,
   CS-1105–CS-1107).
2. **Wide & Shallow (Art. V):** can thread caps (`cs_enforce_threads`-style,
   scoped not permanent) be applied per daemon, and does mirai respect
   single-threaded worker discipline?
3. **Worker isolation (Art. VI):** daemons must write staging only, never
   pins; confirm the staging+consolidate flow works unchanged under
   `mirai_map()` and that structured errors map cleanly onto the batch error
   schema (`error_class`, fingerprints, count reconciliation).
4. **Governance (audit M4):** the experimental-parallel gate, loud warning,
   and provenance fields (`parallel_backend`, thread caps) must wrap any new
   backend exactly as they wrap `future`.
5. **Payoff measurement:** benchmark dispatch overhead and memory for a
   representative campaign (e.g. 1,000+ tasks across the registry estimators);
   `mori` is only worth adopting if shared inputs (plan objects, oracle truth
   tables) are actually large enough to dominate serialization cost — DGPs
   generate per-task data, so this needs measuring, not assuming.
6. **Footprint:** dependency cost (`nanonext`/NNG system requirements) vs.
   dropping `future` + `furrr` from Imports; Windows behavior for both.

Sequencing: run the spike only after CS-1106/CS-1107 (schema-3 + RNG
isolation) and CS-1111 (parallel governance) land, so it evaluates against the
repaired baseline rather than the audited defects. Outcome routes through an
RFC before any backend change — execution-backend swaps touch Art. V/VI and
are an `rfc_cycle.md` trigger.

- GitHub Actions CI for install, tests, and release-gate smoke checks.
- Coverage measurement and coverage-regression reporting.
- Migrate DGP documentation into a pkgdown site structure.
- Write user-facing vignettes for DGP families, estimator contracts, runner
  workflows, and audit/reproducibility practices.
- Python spoke / interoperability layer.
- Expanded documentation architecture and articles.
- CRAN release hardening beyond the v0.1.10 release gate.

## Promotion Rule

To leave the horizon, an item must enter an accepted RFC synthesis, the roadmap,
or an active spec packet. Direct implementation from this file is not authorized.
