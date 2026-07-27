# CausalStress Horizon

**Status:** Non-binding parking lot

Items here are deliberately outside the v0.2.0 public-release boundary unless
promoted by an accepted RFC or a future spec packet.

## Routed v0.2.0 Deferrals (2026-07-24)

- **Future synthetic-extension names.** User-defined synthetic DGPs remain bound
  to uppercase, consecutive `X1...Xk` covariates under Constitution v2.0.1. A
  public extension API that permits semantic feature names would require
  explicit constitutional reconsideration, likely a major amendment, and is not
  authorized by the v0.2.0 correction packet.
- **Feature roster and real-data DGPs.** Real-data DGP support is planned but not
  implemented. The future real-data RFC must define a runner-supplied
  authoritative feature roster before semantic real-data covariates are exposed
  to estimators. New code must not generalize `grep("^X")` or an exclusion list
  into a real-data contract; those are current synthetic implementation details.
  The machine discriminator remains `type = "real"`, with no registered rows.
- **Parameterized families and CATE.** Both remain candidates for the post-v0.2.0
  science program. Parameterized families are the more immediate scientific need;
  CATE could proceed in a bounded parallel track to exercise unit-level contracts.
  A dedicated v0.3.0 planning session must decide the sequence and dependencies.
- **Full documentation release.** A separately versioned release will own the
  pkgdown site, reports for all DGPs, the canonical-workflow vignette, and
  user-defined estimator/DGP contract vignettes. v0.2.0 repairs only its README
  and generated function reference.
- **Persistence boundary follow-ons.** The minimum base-R RDS migration is now a
  v0.2.0 release blocker under CS-1228. Legacy conversion, optional codecs, and
  generalized persistence remain outside that ticket. The internal boundary
  must stay encoding-neutral at its logical-identity edge so later family,
  CATE, and evidence-lake designs are not coupled to RDS bytes.

## Deferred Scientific Work

- New DGP families.
- Sensitivity analysis as a DGP stress axis.
- Additional estimators after ATT/QST contract repairs.
- Registry-spec version bump for known stale narrative entries.

### Moment-regime interpretation for Paper 3 families (parked 2026-07-26)

The accepted pass-2 scientific-review adjudication keeps `synth_heavytail`
stable as an intentional estimand-boundary DGP. Its no-mean Cauchy mixture is
the pathology Paper 3 is meant to expose: conventional superpopulation mean
potential-outcome ATT interpretation stops, while the governed finite-sample
structural signal anchor remains available for breakdown diagnostics and QST
remains well-defined.

| Moment regime | Valid interpretation |
| --- | --- |
| Finite variance | ATT comparison, bias, variance, and RMSE are valid. |
| Finite mean, infinite variance | ATT exists; RMSE and standard Monte Carlo standard errors fail; robust summaries are required. |
| No finite mean | Conventional ATT shootouts stop; structural-anchor diagnostics and QST remain reportable. |

The families specification must preserve execution and atomic point-error
evidence across all three regimes. Beyond the mean-existence boundary, planning
status is `diagnostic_only`, not `ineligible`: invalid comparison must not mean
skipped execution. Moment-regime metadata governs aggregation, where invalid
mean/RMSE/SEM/ranking summaries are marked interpretation-limited and robust
median/quantile summaries are provided. `metric_invalid_for_regime` applies to
the invalid aggregate metric, not to the ATT target or its atomic score rows.
Future QST reporting should also expose the governed oracle's finite Monte Carlo
uncertainty. This entry records the accepted design constraint but authorizes no
families implementation before the v0.3.0 planning gate and its own packet.

### Estimand expansion (parked 2026-06-12; requires an RFC and Article I/IV amendment)

**Status (updated 2026-07-24):** ATE, the estimand-aware scoring join, and the
CATE descriptor were promoted via RFC-1 and ratified in Constitution 2.0.0. ATE
is implemented in v0.2.0; CATE execution is explicitly deferred and must not be
presented as v0.2.0 or automatic v0.3.0 scope. The residual horizon content is
the CATE implementation decision, **quantile-axis population split**, and
**registry generalization** — see the entries below.

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
  v0.3.0 design's deferred list), PEHE/RMSE-over-tau metrics, and the CATE
  null/recovery evaluation. Note: the naive "any estimated heterogeneity on a
  placebo is hallucination" per-unit gate was *superseded* by the F3 prior-art
  pass (2026-06-16) — it is ill-posed for a unit-level estimand; the principled
  form is a heterogeneity-*detection* false-positive test (BLP/GATES/Imai-Li/
  RATE) scored as Type-I error under the sharp null, with CATE accuracy scored
  on non-null DGPs. See the RFC-1 estimand-registry trail and the Gatekeeper
  recalibration entry below.
- **Reopened (was "rejected for now"):** marginal/unconditional (population) QTE
  was previously dismissed here on the grounds that "QST already covers the
  distributional tier for the treated." That under-weighted the
  population-vs-treated **cross-scoring hazard** and is now reopened — see the
  "Quantile-axis population conditioning" entry below.
- **Still rejected:** distribution of treatment effects, i.e. quantiles of
  `Y1 - Y0` (not identified without rank-invariance assumptions — incompatible
  with the unambiguous-truth principle); LATE/IV, mediation, and survival
  estimands (each needs new DGP families and identification machinery; a
  different package era) — all to be reweighed by the estimand-registry deep
  research below, not silently carried as permanent exclusions.

Constitutional note: Article I defines exactly two truth tiers and Article IV
scopes the gatekeeper exclusively to ATT and QST, so any addition is a
constitutional amendment fed by an accepted RFC synthesis, not a feature
ticket.

### Quantile-axis population conditioning: QTE (population) vs QST/QTT (treated) (parked 2026-06-17; requires an RFC + §1.7 amendment)

The mean axis is split by conditioning population — **ATT** (treated, §1.3) and
**ATE** (all, §1.5) are distinct registered targets, so no-cross-scoring keeps
them apart structurally. The quantile axis is **not** yet split: the only
registered quantile target is `qst`, which is treated-target
(`target_population = "treated"`, `scoring_population_id = "treated"`).
Population-target quantile estimators exist in the field and will arrive:

- **DoubleML** QTE is population by default: `Q_{Y(1)}(τ) − Q_{Y(0)}(τ)` over all
  units, not treated-target.
- `unc_qte` ships both `qtt` (treated) and `qte` (population) in one package.
- **RQR** (Borgen–Haupt–Wiborg 2026, *Sociological Methodology*): two-step
  OLS-residualized bivariate CQR targeting the **population QTE** for binary
  and continuous treatments; **no identification theorem** (verbal FWL-style
  argument + simulations only) and no published independent evaluation as of
  2026-07 — a first-order stress-test specimen. Stata-only (`rqr`, SSC); an R
  adapter is a trivial hand-implementation that must be validated against the
  authors' Stata output before any comparison. A GenGC population-QTE
  DR-CDF-band variant (`gengc_dr_qte`) is parked GenGC-side (roadmap RFC
  Queue #5) as the candidate DR representative for this panel once the `qte`
  target exists.
- CFM `Counterfactual`, GenGC, Firpo `ci.qtet` are treated-target.

**Hazard.** With only `qst` registered, a population-QTE output either (a) is
mislabeled `qst` → silently cross-scored against treated-target QST truth (right
family, wrong conditioning population — exactly the failure the typed system
exists to prevent), or (b) uses an unregistered `qte` id → hard abort. The fix
mirrors ATT/ATE: register **`qte` (population)** as a target distinct from
`qst`/`qtt` (treated), each with its own truth (`cs_true_qst` over treated vs a
`cs_true_qte` over all units). Confirmed 2026-06-17: the conditioning population
is already carried in the **target descriptor** (`qst$target_population` /
`scoring_population_id`), not just `meta` — so this is additive. But
no-cross-scoring keys on `estimand_target_id`, so the **distinct id** is what
makes the separation structural; the descriptor field alone does not.

**Interim guardrail (no code change):** do **not** register any population-quantile
estimator (DoubleML QTE, `unc_qte` qte mode, …) until the `qte` target exists.
While every registered estimator is treated-target, the single `qst` target is
honest. This is the line that decides whether the headline quantile comparison is
honest the moment a population-quantile estimator joins. Folded into the registry
generalization below; do not solve it as a one-off `qte` patch.

### Composition-derivative estimand axis (UQPE) and RIF stress testing: a fourth consumer for parametrized families (parked 2026-07-26; routes through the estimand-registry RFC and the families packet)

**Context for a fresh agent.** The applied world's most-used "quantile effect"
tool — RIF/unconditional quantile regression (Firpo–Fortin–Lemieux 2009) — does
not estimate the QTE or the QST/QTT. For a binary treatment `W` with treatment
prevalence `s = Pr[W = 1]` (written `s`, never `p` — `p` is the unit-level
propensity column in the DGP contract), the saturated dummy-only RIF
coefficient has the local prevalence-derivative geometry commonly called the
**UQPE**:

```text
UQPE(tau) = dq_tau(s)/ds = [F_{Y|W=0}(q_tau) - F_{Y|W=1}(q_tau)] / f_Y(q_tau)
```

— the derivative of the **pooled** marginal quantile with respect to
**treatment prevalence**, built from **observational** conditionals. Ordinary
covariate-adjusted RIF regressions add projection and density-estimation layers,
so their fitted coefficient, this observational mixture-share derivative, and a
causal distributional policy effect must not be treated as synonyms. The
mixture-share derivative is a distinct **contrast/intervention type**: not
treated-target (`qst`/`qtt`), not population-target (the reopened `qte`), but a
**composition derivative**. It differs from QTT through stacked wedges
(vertical-CDF-gap-over-density geometry vs horizontal quantile gaps; derivative
per unit share vs discrete per-person switch; pooled vs treated population;
selection built into the observational conditionals), and the wedges close only
for infinitesimal effects on linear functionals — the **mean** is the degenerate
case where the derivative equals the discrete gap exactly, which is why the
"coefficient = effect" intuition formed there and silently breaks on quantiles.
The mismatch generalizes: **every marginal functional ν** (quantile, variance,
Gini, tail share) spawns the triple {population effect, treated effect,
share-derivative}, so this entry is an instance of the registry-generalization
question below, not a one-off.

**Prior-art status (provisional 2026-07-26 reconnaissance; reverify before a
specification):**
the divergence *theory* is fully crowded — FFL 2009 state the share-derivative
reading themselves (p. 954; FFL 2007 NBER t0339 Corollary 3 for the dummy
case); Rothe (2012, *Econometrica*) calls FFL's binary parameter "substantially
different" from the unconditional share effect and shows the discrete case is
only set-identified; Martínez-Iriarte & Sun (2024, *J. Econometrics*) give an
Apparent/Bias decomposition and prove RIF/UQR can be **inconsistent even under
exogenous treatment**. Borgen, Haupt & Wiborg (2026, *Sociological
Methodology* 56(2), DOI 10.1177/00811750261450139) already provide
truth-anchored simulations under both randomized assignment and observed
confounding (single-binary-covariate selection; normal/right-skew outcomes
only — heavy tails and overlap stress not simulated). **Residual open cell:** systematic overlap
and heavy-tail stress, continuous breakdown surfaces, same-τ sign disagreement
against known truth, and a deployable diagnostic/gate beyond the existing
UQR-versus-QTE simulations. Full working bibliography,
must-cite list, and residual uncertainties:
[`phd-KB/syntheses/rif-uqpe-vs-qtt-prior-art.md`](../../../phd-KB/syntheses/rif-uqpe-vs-qtt-prior-art.md).
Audience evidence (RIF ≈ 4k-citation applied user base vs ≈ 12 named-QTT works;
documented misinterpretation literature):
[`phd-KB/syntheses/qte-estimand-empirical-usage.md`](../../../phd-KB/syntheses/qte-estimand-empirical-usage.md).

**First oracle evidence (2026-07-26, exploratory, inside the accepted A2 R 4.6
image, no estimators):** UQPE overstates QTT by 14–27% through the body of
`synth_baseline` and flips to ~50% understatement at τ = 0.99; at the
`synth_qte1` median the three objects triple-diverge (QTE ≈ 0.005, QTT = 0.646,
UQPE = 0.736); and a kernel-density denominator produced a **10× artifact in
the exploratory calculation** on `synth_heavytail` before a kernel-free method
replaced it. A candidate oracle is therefore the **mixture-quantile central
finite difference** (pure ECDF inversion of
`s·F_{Y|W=1} + (1−s)·F_{Y|W=0}` at `s ± δ`; script preserved in the prior-art
KB note). It is not yet governed truth: the RFC must specify population versus
finite-Monte-Carlo semantics, δ sensitivity/convergence, Monte Carlo
uncertainty, and algorithm/version identity. The same 1/f_Y(q_τ)
fragility afflicts real RIF software and density-scaled Wald quantile inference
generally; CDF-band inversion (the GenGC DR-QST design) is the density-free
alternative — a cross-cutting inference-design contrast worth making explicit
in any study.

**Candidate package mechanisms (all RFC-gated, none authorized):**

1. **Register a composition-derivative target** (working id `uqpe_share`) with its own
   governed mixture-derivative truth algorithm and a distinct
   `estimand_target_id`, so no-cross-scoring keeps it
   structurally apart from `qst`/`qtt`/`qte` — the exact mechanism that
   separates ATT/ATE and motivates the quantile-axis split above. Article I
   note: UQPE truth is a functional of the *observational* joint (selection
   included), oracle-computable from any DGP without identification
   assumptions. The RFC should first try to express it within the existing
   distributional truth tier rather than inventing a third tier, and should add
   a `contrast_type` or `intervention_spec` identity axis so a potential-outcome
   contrast cannot be confused with a mixture-share derivative.
2. **RIF-OLS / RIF-logit as registered estimator arms** declaring
   `estimand_target = uqpe_share`, enabling **two-layer scoring**: (a)
   own-estimand validity — does RIF even estimate its own UQPE well, given the
   linear-projection and density-estimation layers; (b) misreading damage —
   divergence from `qst`/`qte` truths reported only through an explicit,
   labeled mismatch analysis, never silent cross-scoring.
3. **Parametrized-families requirements — and the full consumer matrix
   (updated 2026-07-26).** The families packet has **four estimand-axis
   consumers**, not two: (i) **ATT** kill-plots (the original motivation; its
   unique demands — moment-regime dial endpoints and `diagnostic_only`
   planning — are recorded in the moment-regime entry above); (ii) the
   **QST/QTE** validity-envelope phase diagram (thesis flagship); (iii)
   **CATE**, whose execution stays deferred but whose stress axis is a
   *surface*, uniquely demanding heterogeneity-structure dials (amplitude,
   modifier sparsity, τ(X) smoothness) plus sharp-null endpoints for the
   heterogeneity-detection Type-I test (see the estimand-expansion entry);
   and (iv) **UQPE/composition** (this entry). Each consumer surfaces requirements
   the others would not — the families spec intake must collect all four
   demand sets before freezing dial vocabulary. Only the DGP side (registry
   entries, versioning, truth machinery) amortizes across all four; estimator
   compute does not, and the consumer count reorders no gates ("families
   before CATE" stands). This entry's consumer imposes design requirements
   the others would not surface:
   - a **treatment-prevalence dial** named `treatment_prevalence` or
     `treated_share`, not `p` (which already denotes unit-level propensity in
     CausalStress). The design must distinguish configured prevalence, realized
     finite-sample share, and the local perturbation `δ`; changing a propensity
     intercept across family members is not automatically the same intervention
     as reweighting fixed observational conditionals;
   - **scorer-only oracle access to observational conditionals plus a
     share-shift computation**, so functional-triple truths {population,
     treated, share-derivative} can be governed without entering estimator
     inputs or weakening the Airlock;
   - the tail-index, selection/overlap, and heterogeneity/reranking dials
     (shared with the other consumers) plus per-dial-point DGP identity and
     versioning (already the families packet's core question);
   - an **assignment-model misspecification dial** (functional-form wrongness
     of the treatment equation — omitted interactions/quadratics dialed 0 →
     severe; a *different axis* than selection strength). Motivating
     consumers: the QST/QTE panel and any single-robust-vs-orthogonal
     contrast — e.g. the parked RQR orthogonality-gap experiment (first-order
     vs second-order nuisance-error propagation); see phd-KB
     `syntheses/rif-uqpe-vs-qtt-prior-art.md` §"RQR as benchmark specimen".
4. **Why families matter here specifically:** twelve discrete DGPs yield a
   mismatch *table*; continuous dials yield **breakdown curves** — UQPE/QTT
   distortion as a surface in (tail index × τ), (selection × τ), (share × τ) —
   and a diagnostic threshold can only be **calibrated** on a curve, not on
   twelve points. Same argument as the estimator kill-plot flagship, fourth
   consumer.

**Hazard and interim guardrail (no code change):** do **not** register any
RIF/UQR estimator until the composition-derivative target exists — a RIF arm labeled `qst`
would be silently cross-scored against treated-target truth, the exact defect
class the typed system exists to prevent. Consistent with this, the R QTE
shootout preregistration
([`thomasberger-phd-research/campaigns/specifications/qte-shootout/PREREGISTRATION.md`](../../../thomasberger-phd-research/campaigns/specifications/qte-shootout/PREREGISTRATION.md))
excludes RIF from its fair core as a category error rather than a competitor.

**Activation gate:** authorizes nothing. Sequenced behind WP-01/G1 and WP-02
seals and the families planning gate; the estimand-target addition routes
through the estimand-registry RFC below (do not solve as a one-off), and the
programme-side study is parked as candidate WP-05 in
[`thomasberger-phd-research/META_RESEARCH_MEMORY.md`](../../../thomasberger-phd-research/META_RESEARCH_MEMORY.md)
§2A. Before any spec freeze, re-verify the prior-art must-cite list — the
Martínez-Iriarte/Sun and Borgen/RQR lines are active. (The RQR manuscript's
April-2024 SocArXiv revision was read in full on 2026-07-26 and matches the
published abstract/content; confirm the *Sociological Methodology* version of
record at freeze.)

**Release treatment (recorded from the 2026-07-26 external review):** v0.2.1
batches are untouched. v0.2.1 documentation may describe the four currently
governed targets and the generic no-cross-scoring rule but must **not**
advertise UQPE or QTE support. v0.3.0 families planning carries
`treatment_prevalence`, selection/overlap, tails, and heterogeneity/reranking
into the requirements intake; the initial families implementation stays
**target-neutral and serves ATT/QST first**. The UQPE target, its
truth-algorithm promotion, RIF arms, and the diagnostic gate each require the
estimand-registry RFC below, constitutional adjudication, and a **named
consuming study**.

### Selection/missingness DGP class and MAR-standardization estimands (Phase-S0 PARK / NO-GO 2026-07-27; no implementation authority)

**Context for a fresh agent.** Programme candidate WP-06
([`thomasberger-phd-research/META_RESEARCH_MEMORY.md`](../../../thomasberger-phd-research/META_RESEARCH_MEMORY.md)
§2A) is an applied-methods paper on sequential full-time/part-time/
non-employment selection for distributional gender-gap estimands, with an
Austrian application. The complete evidence record — corrected prior-art
verdict (the 2026-07-27 recon found no exact published match for the five-part
  conjunction of sequential FT/PT/NE states, state-specific distributional
  potential outcomes, gender-gap decomposition, MNAR sensitivity, and
truth-anchored simulation; closest priors Pereda-Fernández JBES 2025,
Fernández-Val & Hong JoE 251, Kim 2026; this is a dated search result, not an
unconditional novelty claim), the
external peer-review disposition, the Phase-S0 triage mandate/output contract
with its decision rule, and the split between S0 baseline constraints and
promotion-stage DGP extensions — lives in
[`phd-KB/syntheses/selection-distributional-decompositions-prior-art.md`](../../../phd-KB/syntheses/selection-distributional-decompositions-prior-art.md).
**Phase-S0 is mandated to run OUTSIDE CausalStress** (external review
2026-07-27: no CausalStress DGP, campaign, adapter, or package feature —
oracle scripts only). This entry records solely what package support WOULD be
demanded IF WP-06 is promoted, so the estimand-registry RFC sees the demand
signal early instead of absorbing it as a one-off later:

**Outcome update.** Phase-S0 completed with technical `PASS`, exact replay, and
scientific `PARK / NO-GO`. All ten `rho=0.5` pointwise truth targets failed,
one PT endpoint was unstable, and no frozen sign-contrast gate passed. WP-06
was not promoted. The requirements below remain a historical demand sketch,
not a backlog: no selection DGP, estimand, adapter, family, or package work is
authorized. Any reconsideration must begin as a distinct externally reviewed
research candidate, not as an S0 repair or CausalStress feature request.

- **A new DGP contract shape.** Multi-state status S ∈ {NE, PT, FT},
  state-specific potential outcomes (Y\*\_FT, Y\*\_PT — the review killed the
  single-latent-wage form: one w\* plus a shift imposes common ranks across
  states by construction), and observed-data masking (wages missing for NE).
  The current contract assumes complete observation of one outcome plus
  y0/y1/p; masking generalizes the existing airlock/oracle separation
  (observed table vs latent truths) rather than violating it, but it is a
  contract change and therefore Article VII + registry-RFC territory.
- **New truth objects — with a four-way type distinction the registry must
  keep.** The trimmed S0 registry contains within-state observed gaps;
  population distributions of Y\*\_FT and Y\*\_PT; **ONE** precisely defined
  status-standardized counterfactual distribution; and **ONE** fixed-ordering
  decomposition. No additional counterfactual or decomposition variants are
  authorized. (i) *Latent target/truth objects* are the state-specific latent
  distributions. The observed gaps and decomposition are separately typed
  descriptive/decomposition outputs; the status-standardized counterfactual is
  the MAR-standardization estimand. (ii) *Identification regime*: whether MAR
  standardization
  identifies a given functional of those truths is a property of the
  DGP-assumption pair, not of the truth object. (iii) *Estimands*: the
  identified functionals a method targets under a declared regime. (iv)
  *Sensitivity parameters* (next bullet). Latent Y\*-distributions do not
  become "MAR estimands" merely because MAR identifies certain functionals
  under maintained assumptions. This is a selection-regime/
  population-descriptor axis for the registry, adjacent to but distinct from
  the composition-derivative entry's contrast_type axis.
- **Sensitivity parameters are not estimands.** The Gaussian `rho` is a known
  selection–outcome dependence parameter in the S0 oracle DGP, while the v0.7
  analysis varies four observable-law exponential-tilt `gamma` parameters as
  an explicitly imposed pattern-mixture relaxation, evaluated through weighted
  empirical CDFs and compared with collapsed binary missingness. Nested widths
  are descriptive; the decision contrast requires `collapsed_upper<=-0.01`
  versus `two_pattern_upper>=0.01`, under the
  same equal-range independently varying relaxation. Neither object
  is an estimand, and the `gamma` values are not inferred from `rho`. Pointwise
  oracle-gap coverage is not model-family containment. Empirical work
  must report over a declared relaxation set (robustness curves/breakdown
  frontiers, Masten–Poirier structure) rather than treating `rho` or `gamma` as
  estimated or using oracle truth as a deployable gate. The review explicitly
  rejected "bias vs true rho" as a gate.
- **Families note (weight zero until promotion).** If promoted, selection
  strength, instrument strength (two instruments Z_P/Z_F in the reviewed
  design), and exclusion-violation dials would make this a further consumer
  of the parametrized-families packet — recorded for the requirements intake
  only; the initial families implementation still serves ATT/QST first.

**Activation gate:** closed for the current WP-06 route because Phase-S0 did
not reach `CONTINUE TO PROMOTION CHECK`. This entry authorizes nothing. A
future, distinct candidate would still require its own prior-art review, the
estimand-registry RFC below, and the programme WIP rule before any spec or
implementation work.

### Estimand registry generalization: §1.7 list → governed schema (parked 2026-06-17; requires deep research, an RFC, and a §1.7 amendment)

**Problem — amendment-per-estimand.** Constitution §1.7 enumerates a *closed* set
("the estimands governed in v2.x are ATT, ATE, QST, CATE"). Every new estimand
(QTE above, then GATE, LATE, distributional-CDF effects, …) therefore needs its
own Article I amendment — a stream of small constitutional crises. §1.7 already
calls the set "a governed, versioned vocabulary," so the intent is registry-like;
the hardcoded list is what makes it closed in practice.

**Direction (for the RFC, not decided here).** Convert §1.7 from a member list
into a governed **schema + registry**, exactly as Article VII does for DGPs (the
Constitution governs the rules; a registry holds the members; maintainers add
members without amending). The Constitution would govern: the estimand **identity
axes** (truth tier × target level × conditioning population × functional/metric),
the **truth-definition obligation** (every governed estimand declares structural
or distributional truth + no truth regeneration for real DGPs), no-cross-scoring
and the non-comparable vocabulary (already present), and the **authority to
register** a target (maintainer, mirroring §1.2). The founding definitions
§1.3–1.6 stay; new members ride the schema as additive registry entries. After
this, adding `qte`/`gate`/etc. is a registry bump, not an amendment.

**Why deep research first.** Designing the schema and amending §1.7 *once* with
the full estimand space in view avoids discovering QTE, then LATE, then
distributional effects one painful amendment at a time. A read-only deep-research
pass (runnable in parallel with Waves 1–2; touches no code) should enumerate the
estimand space the field actually benchmarks, filtered by: well-defined
synthetic-DGP truth, production by the SOTA estimators we target (DoubleML,
EconML, grf, causalml, `unc_qte`, `Counterfactual`), each estimator's **default
target population** (the cross-scoring trap), and compatibility with the
unambiguous-truth principle (quantiles of `Y1 − Y0` stay excluded — not identified
without rank invariance). Output: a prioritized taxonomy to **inform the schema** —
but it is **not** adopted wholesale as the vocabulary; see the scoped position
below (schema yes, catalog no).

**Sequencing.** The RFC + single §1.7 amendment remains a separate future
program. The v0.3.0 planning session must place it relative to CATE and families;
if CATE is selected first, its unit-level mechanics should inform the
generalization. This entry does not authorize a fixed Wave 2 sequence. It
subsumes the quantile-axis entry above and the residual estimand-expansion items.

**Scoped position after the deep-research pass (returned 2026-06-18).** The
research over-answered and, taken literally, is a scope-creep trap. It conflated
two separable things; the RFC keeps the first and rejects the second.

- **Adopt the axes *schema* (the scope-creep *cure*, not cause).** A schema lets
  the registry *express* an estimand without anyone *building* it. Identity =
  `functional` (mean / quantile / distribution) × `contrast` (≈always difference) ×
  `target_population` (all / treated / control / overlap / subgroup) ×
  `conditioning_level` (population-scalar / curve-by-τ / unit) × `index_spec` ×
  `truth_requirements`. **Trim the research's ~10-field schema hard:** drop
  `treatment_regime`, `identification_regime`, and every IV / continuous /
  survival / dynamic / mediation primitive — those are for the families we
  *exclude*, so they don't belong in a binary-treatment, static, synthetic-DGP
  instrument's schema until/unless we go there.
- **Reject the "v1 vocabulary" build-list.** The research's "govern
  apo/atc/ato/gate/ite/policy_value/policy_regret/late/lqte/adrf/… in v1" is
  academic completeness, not product scope. Each needs DGP truth + scorer +
  gatekeeper component + a producing estimator, or it is a dead registry row.
  **v1 governed estimands stay exactly the shipped four: ATT, ATE, QST (treated
  quantile), CATE.** Add members **lazily** — only when a real estimator *and* a
  study consume one. Next genuine addition is `qte` (population) when a
  population-quantile estimator (DoubleML) is registered: **one row**, per the
  quantile-axis entry above — not a catalog.
- **The one early behavioral rule worth lifting now:** every registered estimator
  MUST declare its `(estimand, target_population)` explicitly, and no-cross-scoring
  keys on the **full identity, never a bare name**. This is the cross-scoring trap
  generalized — package defaults differ (`grf::average_treatment_effect` defaults
  `target.sample="all"` = ATE; DoubleML QTE is population unless `score="LPQ"`;
  `qte::unc_qte` has `qtt`/`qte` modes; `Counterfactual` defaults to a *composition*
  effect, not a treatment effect; IV forests are complier-local ACLATE). Mislabel
  any of these and the benchmark silently corrupts.
- **Keep the exclusion *reasoning* as justification-not-to-build:** quantiles of
  `Y1 − Y0` need rank invariance; mediation needs nested cross-world counterfactuals;
  survival needs event-time POs + censoring (and hazard ratios are non-collapsible);
  dynamic regimes need sequential POs. These stay out of v1 by design.
- **RFC scope, therefore:** constitutionalize the trimmed schema + the
  explicit-target-population rule, and **migrate the existing four estimands into
  it** — *not* implement the estimand zoo. One-time design move, not an open-ended
  build.
- **Process:** the raw deep-research doc is **not** committed (consistent with the
  prior-art-pass rule). Its `citeturn…` markers are ChatGPT artifacts that do not
  resolve — when this feeds the RFC, re-verify the package-default-population claims
  against the actual package docs, not the tokens.

### Gatekeeper recalibration (parked 2026-06-16; requires a dedicated RFC and Article IV amendment)

**Problem.** The current placebo gatekeeper (Article IV §4.2) is miscalibrated:
it applies a monolithic pass/fail across the *entire* placebo suite and marks an
estimator **Non-Robust in the registry** (§4.2.4) on systematic deviation. But
the suite deliberately includes adversarial worst-case traps — e.g.
`synth_placebo_kangschafer`, whose *entire point* is dual outcome-regression +
propensity misspecification that naive estimators cannot survive. Failing such a
trap is **diagnostic** (it locates where an estimator breaks), not
**disqualifying** (the estimator remains usable elsewhere). The gate conflates
"failed a known brutal trap" with "is non-robust/unusable," and the binary
`Non-Robust` label is too blunt for what is really a per-DGP vulnerability
profile.

**Candidate redesign direction (for the RFC, not decided here):**

- Replace the monolithic suite-level verdict with a **per-DGP robustness
  profile**: which placebo failure modes (overlap, dual misspecification,
  heavy tails, …) each estimator is vulnerable to — a breakdown profile, not a
  pass/fail stamp. This is the same "profile, not binary verdict" philosophy as
  the kill-plots, so it may share machinery with the families/kill-plot work
  (the future RFC-3 lineage).
- Introduce **difficulty tiers** for placebo DGPs (baseline-expectation vs
  adversarial-trap), so the `Non-Robust` consequence — if retained at all —
  attaches only to failure on the baseline tier, while trap failures are
  recorded as characterization.
- Reconsider what registry consequence, if any, a gate failure should carry,
  and whether "Unverified" (Art. IV §4.2.3, for estimators without CIs) is the
  better default than "Non-Robust."

**Coupling — important for RFC-1 (Estimand Registry).** RFC-1 generalizes the
gatekeeper *structure* (per-estimand pluggable; the CATE detection-test
decision from the F3 prior-art pass). This entry is about the gate's *policy /
calibration / labeling* — a distinct, deeper question. **RFC-1 must therefore
NOT bake the current too-harsh "whole-suite-or-Non-Robust" policy into any new
(CATE/ATE) gate**; it should make the new gates pluggable and explicitly defer
their pass/fail *calibration* to this recalibration RFC. Structure now, policy
later.

Governance note: §4.2 is constitutional, so recalibration is an Article IV
amendment via the RFC cycle, not a feature ticket.

### ACIC 2026 comparator, interoperability, and multi-arm horizon (parked 2026-07-21; no current implementation authority)

**Evidence and decision record.** The focused prior-art audit
`research/prior_art_acic_2026_benchmark_audit.md` finds that ACIC 2026 is the
stronger scientific challenge today (9,000 distinct populations, five randomized
arms, iCATE/sCATE/subCATE/PATE, best-treatment outputs, uncertainty and
independent participation), while CausalStress is the stronger publicly visible
reusable integrity substrate (typed targets, no-cross-scoring, versioned truth,
airlock, fingerprints, persistence, and explicit non-comparability). This is a
**complementarity result**, not an instruction to chase ACIC feature parity.
The official site calls 2026 a return after a multiyear hiatus: the comparator
landscape changed during CausalStress development, so update the forward claim
without rewriting the project's origin story as an avoidable mistake.

**Programme decision.** ACIC 2026 does **not** interrupt the current campaign or
v0.2.0 closure, and it does not decide the order of CATE and RFC-3 families. It
changes the prior-art baseline and supplies requirements for later
interoperability. Current Paper 3 positioning becomes: *integrity-first
laboratory for governed stress mechanisms and estimator failure boundaries,
complementary to blinded challenges* — never “broader than ACIC 2026.”

**Immediate research/coordination actions (no package code):**

1. **Organizer inquiry.** Ask the ACIC 2026 organizers whether and when the
   datasets, generator, truth, scorer, exact aggregation rules, checksums, and
   version history can be released; request the applicable license and explicit
   written permission for a CausalStress external adapter. Record the response as
   provenance. The public repository had no detected generator/scorer/data or
   license at the 2026-07-21 cutoff; do not infer reuse rights from visibility.
2. **Related-work correction.** Cite ACIC 2026 wherever CausalStress breadth,
   heterogeneous-effect benchmarking, multi-estimand comparison, or uncertainty
   evaluation is discussed. Do not claim novelty for those ingredients alone.
3. **Public/gated evidence split.** Treat public challenge claims separately from
   any participant-only data or documentation. Unknown public scoring details
   (failure denominators, tie handling, exact metric aggregation, Monte Carlo
   uncertainty, containers/checksums) remain unknown until evidenced.
4. **Estimand-schema input, not build list.** Carry the ACIC compatibility map
   into the future estimand-registry-generalization RFC. It informs identity
   axes and no-cross-scoring; it does not populate speculative registry rows.

**Core work that remains, subject to the v0.3.0 planning gate:**

5. **Binary CATE candidate.** If selected, implement only the already-ratified
   held-out unit-keyed CATE target, PEHE/RMSE plus robust companion, and
   heterogeneity-detection gate. Do not smuggle multi-arm, GATE, PATE, or policy
   learning into that packet. Parameterized families may be selected first or
   developed in a deliberately bounded parallel track.
6. **Preserve sample/population identity.** ACIC's sCATE/PATE split reinforces the
   current rule: finite-sample and superpopulation truth are distinct identities
   and may never be cross-scored. When target descriptors are generalized, the
   scoring population must stay in the score key, not merely narrative metadata.
7. **Inference metrics.** When interval-producing estimators are compared, plan
   pointwise coverage/width, simultaneous or family-wise calibration where the
   claim requires it, and Monte Carlo uncertainty. These are metric/inference
   obligations, not new estimands.
8. **Earn the Paper 3 differentiator.** Promote and scientifically validate at
   least one parameterized family through RFC-3 and ship explicit
   planned/attempted/succeeded/failed/timeout/missing denominators before claiming
   continuous validity envelopes or survivorship-honest kill plots. This couples
   to the runner-integrity migration below; neither design document alone counts
   as implementation evidence.

**Conditional external-ACIC integration path:**

9. **Preferred form — official generator wrapper.** If a licensed generator is
   released, wrap it externally and pin its commit/container digest, seed and
   population identity, truth/scorer version, and golden checksums. Do not copy
   upstream logic unless the license and scientific maintenance plan justify it.
10. **Fallback — bring-your-own-data adapter.** If only gated static datasets can
    be used, require user-supplied inputs, dataset IDs and checksums; keep truth on
    a scorer-only channel; label the object a static external benchmark, not a
    native reproducible DGP; emit `truth_unavailable` without lawful truth.
11. **Reject slide reconstruction as reproduction.** A generator independently
    reconstructed from public descriptions must be labelled `ACIC-inspired`; it
    cannot claim ACIC 2026 identity or result reproduction.
12. **Five promotion gates.** Any adapter/generator work requires: (a) written
    rights, (b) version/checksum identity, (c) golden-output fidelity, (d) truth
    separation, and (e) a named consuming study. Failure of any applicable gate
    keeps the integration parked.

**Estimand and treatment-domain scope:**

- **Core now:** implemented ATT, finite-sample ATE, and treated QST/QTT, plus the
  already-ratified but unimplemented binary CATE descriptor. QST/QTT remains a
  genuine distributional differentiator absent from the ACIC 2026 requested
  targets.
- **Defer:** subgroup CATE/GATE (group inference is not automatically valid by
  averaging predictions); PATE/superpopulation ATE (add only for a consuming
  study); best-treatment/policy targets (eventually score tie-aware policy value
  or regret, not accuracy alone); population QTE remains the likely next binary
  target only when an actual producer and study arrive.
- **Reject:** a wholesale estimand catalog; quantiles of individual
  `Y(1) - Y(0)` without a governed coupling/rank-invariance assumption; calling
  best-arm classification a complete policy-learning target.
- **Multi-arm is a future major era.** Faithful support requires treatment-domain
  and reference-arm metadata, arm-indexed propensity/potential-outcome/truth/
  output shapes, contrast-indexed score identity, joint uncertainty, tie-aware
  decisions, and corresponding airlock/gatekeeper/persistence changes. Four
  silent one-vs-control loops lose dependence and multiplicity and do not count
  as ACIC-compatible support. Open a multi-arm RFC only after both CATE and the
  relevant family work have settled, and only for a named study; do not
  pre-generalize current code speculatively.

**Adoption action.** Before CausalStress claims general infrastructure value,
seek at least one independent reproducer, estimator adapter, or external study.
Packaging, documentation, tests, and a bring-your-own-estimator path are part of
the scientific contribution, but community use is evidence that the instrument
escapes its originating dissertation.

**Promotion rule.** This entry records evidence, scope, and gates only. No item
authorizes implementation. External-adapter work requires its own RFC/spec after
the rights and identity gates; multi-arm support requires a major-era RFC and any
constitutional amendments identified by that process.

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

### Legacy `.qs` recovery after runtime retirement

**Promoted portion.** Removal of the archived `qs` runtime dependency, base-R
RDS staging/cache writes, strict legacy-staging refusal, and fresh R 4.6 CI are
no longer horizon work. They are the narrow v0.2.0 release blocker specified by
CS-1228 after branch CI proved that `qs` 0.27.3 itself cannot compile against
the current R headers.

**Still deferred.** CausalStress will not ship a converter, codec plug-in layer,
`qs2` dependency, dual-writing, or general storage platform in v0.2.0. The
maintainer is willing to rerun the small current pre-release corpus. If a future
evidence corpus makes recovery necessary, a separately governed utility may run
outside the package in a frozen archived-`qs` environment, preserve each source
byte-for-byte, write validated derivatives, and emit lineage receipts. That is
a future need, not a current commitment.

RDS remains an R-native runtime bridge, not a language-neutral archive.
Normalized Parquet plus canonical JSON remains the later cross-language
evidence-lake direction. The optional tiny-file `qs2` benchmark is non-blocking
research only and cannot change the RDS default without a reviewed spec. The
complete campaign OCI image and any campaign-local dynamic arms remain campaign
acceptance work, not CS-1228 scope.

**Relationship to the entries below.** The v0.2.0 persistence boundary should
keep logical identity independent of RDS bytes so it does not foreclose
runner-integrity or evidence-lake work. It must not implement their catalog,
reuse/admissibility model, cross-language protocol, or broader policy.

### Runner-integrity migration from the QCB campaign capsule (parked 2026-07-21; requires an RFC; touches Articles II/III/V/VI and the planner/summary contracts)

**Provenance.** The `qcb-2026-07-a2` campaign capsule
(`thomasberger-phd-research/campaigns/`, capsule spec v0.1.1) had to build a set
of integrity mechanisms *around* the CausalStress runner because the runner does
not provide them. Each mechanism is now implemented, adversarially reviewed
(clean-context Batch 1/2/3-4 reviews under
`thomasberger-phd-research/reviews/programme/` and
`campaigns/qcb-2026-07-a2/evidence/implementation/`), and field-proven on the
full 4,800-task plan. Organizing principle for the migration: **the capsule owns
policy (authorization, evidence classes, sealing, human gates); CausalStress
owns mechanism (what it means to run one benchmark task with integrity).**
Mechanisms proven generic migrate here; policy stays in the capsule. The
motivating lesson is the Batch 3/4 BLOCKER F1: protections living in wrapper
layers must be remembered by every caller; mechanisms native to the runner
cannot be forgotten.

**Candidate migrations (each cites the defect that earned it):**

1. **Full post-airlock input fingerprinting.** The June 2026 QCB campaign's
   `run_key` hashed `y`/`w`/`n`/`tau`/seed/estimator/interval-method but never
   the covariate matrix, leaving cell identity resting on the versioned-DGP
   contract alone. The capsule's canonical serialization
   (`qcb-post-airlock-xdr-v1`: column names/order, row identity/order, storage
   types, factor schema, NA/NaN representation, numeric serialization, UTF-8;
   R-version-pinned serialization header) fingerprints the complete
   estimator-visible frame per cell. Natural home: the airlock layer — the
   component that sanitizes what estimators see should certify *what* they saw.
   Article II/III material; interacts with `cs_fingerprint.R`.
2. **Runtime input receipts in the Runner contract.** Per-task verification that
   the materialized post-airlock frame matches the plan's expected input hash
   *before* any estimator fits, with fail-closed mismatch handling. Currently
   cloned into each estimator wrapper (Batch 2 review flagged the
   clone-divergence risk); as a Runner obligation every estimator inherits it.
   Article III.
3. **Resume integrity.** `cs_run_campaign` currently skips staged/pinned batch
   IDs by identity alone. The capsule's resume validation (schema check,
   expected task count, task-fingerprint match, result/error reconciliation,
   retained-artifact hash, rejection of ambiguous duplicates) is the missing
   half of Article VI crash resilience: atomic persistence guarantees writes,
   resume validation must guarantee *reads*. Article VI.
4. **Plan-identity semantics.** Order-invariant task-*set* hash (sorted
   `task_id`/fingerprint records) recorded separately from the order-binding
   execution-*plan* hash (deterministic order + batch membership), so plan
   identity is invariant to worker count and scheduling while execution order
   remains auditable. Belongs with `cs-plan-campaign`/`cs-fingerprint`.
5. **DGP-status honesty in the planner.** The June plans declared all twelve
   panel DGPs `stable` while the installed registry marked ten `experimental` —
   a planner-side metadata defect (verified: IDs/versions/data identical; only
   status labels wrong). The capsule's repair: statuses are stamped from the
   installed registry at plan time, declared≠installed fails closed, and
   `dgp_status` is carried into task identity. This one is arguably a
   defect-repair spec-packet ticket rather than constitutional work.
6. **Failure-accounting vocabulary and survivorship-honest summaries.** Adopt
   the capsule's terminal-status vocabulary (including
   `not_attempted_due_to_abort` and `non_finite_output`) in the run/batch
   contracts, and extend the public summaries (`cs-summary-qst.R` and
   successors) to report planned/attempted/succeeded/failed/timeout/missing
   denominators instead of silent `na.rm = TRUE` means — the survivorship gap
   identified in the 2026-07-20 external review. Interacts with the typed
   scoring layer's non-comparable vocabulary.

**Explicitly NOT migrating (stays capsule-side):** campaign lifecycle,
authorization records, sealing/deviation machinery, evidence classes and
reproducibility statuses, source/environment closure (git snapshots, ACL
staging, native-environment capture), retention topology, and any
preregistration lookup. A benchmark library must not own a research programme's
approval workflow.

**Sequencing and governance.** Parked until WP-01/G1 closes in the programme
repo **and the linked WP-02 calibration campaigns (tree-stability
`qcb-2026-08-trees` and learning-curve `qcb-2026-08-lcurve`) complete** — per
the programme decision of 2026-07-21, additional campaigns precede any
CausalStress development resumption (the capsule campaigns correctly run on
their reviewed wrapper-level protections meanwhile). Promotion route: one
"runner integrity" RFC covering items 1–4 and 6 (Articles II/III/V/VI
touchpoints), with item 5 routed as an ordinary defect ticket in the next spec
packet. Natural slot: alongside or
before the RFC-3 families program, since the G5 confirmation campaign — and the
JSS software paper's integrity claims — should inherit these as native
guarantees rather than harness add-ons. Per the promotion rule below, nothing
in this entry authorizes implementation.

### Evidence lake, declarative reuse, and cross-language hub protocol (parked 2026-07-21; requires a separate deep-design session and RFC)

**Vision.** CausalStress should eventually maintain an immutable, queryable
evidence base in which a campaign declares the complete scientific comparison it
wants, resolves already-computed cells against verified retained evidence, and
executes only cells that are genuinely missing and scientifically admissible.
The campaign remains a complete requested design: reused, rescored, failed,
incompatible, and newly executed cells are all explicit in its frozen evidence
manifest. This is **declarative evidence resolution**, not a more permissive
`skip_existing` cache.

**Why this must constrain the architecture early.** The schema determines what
"the same experiment" means. Retrofitting it after a large pathology atlas exists
would require re-identifying old results and could make cross-language evidence
impossible to compare. The lake design must therefore inform the runner-integrity
RFC, persistence identities, CATE output retention, parameterized-family
instances, and the external protocol before any of those surfaces are treated as
finally frozen. It does **not** follow that the database or client libraries
should be built now.

**Minimum identity separation for the design session:**

1. **Scientific-cell identity:** the conceptual question — governed DGP/version/
   family parameters, target and scoring population, treatment contrast, sample
   and evaluation design, estimator specification, and requested inference.
2. **Dataset-draw identity:** exact estimator-visible data, unit/order/schema,
   generation contract, seed, and both byte-level and logical-content digests.
3. **Fit identity:** dataset draw plus estimator implementation/configuration,
   fit RNG, source/environment identity, and declared transductive behavior.
4. **Prediction/output identity:** fit plus evaluation draw, requested coordinates
   (tau/unit/contrast), output schema, and inference payload.
5. **Score identity:** output plus truth, metric definition/version, target
   population, contrast, and scoring coordinates. New metrics should rescore
   retained sufficient outputs rather than force refitting.
6. **Attempt identity:** execution event, substrate, backend, worker/threads,
   timestamps, logs, status, warnings, resources, and retry lineage. Attempts do
   not overwrite scientific or artifact history.
7. **Derivation and release identity:** parent artifacts, transformation code/
   configuration, frozen query, and the exact artifact set supporting a table,
   kill plot, pathology report, or atlas release.

The design must distinguish **scientific equivalence** from **artifact
equivalence** and from **campaign admissibility**. An exact old artifact may be
computationally reusable but inadmissible for a confirmation campaign because it
was exploratory, previously exposed, tuned against, outside the frozen comparator
policy, or required to be a fresh held-out draw. The campaign capsule retains
authority over evidence class, authorization, preregistration, sealing, and
admissibility; CausalStress supplies identity, verification, resolution, and
execution mechanisms.

**Required resolution vocabulary (candidate, not yet frozen):**
`exact_reusable`, `derivable_without_refit`,
`scientifically_comparable_not_reusable`, `prior_failure_evidence`,
`artifact_missing_or_corrupt`, `inadmissible_for_campaign`, and `new_cell`.
Failures remain evidence. A resolver must never reuse successes while silently
retrying failures until they disappear; retry policy and all attempts remain
visible.

**Storage direction to evaluate, not a technology decision:**

- immutable content-addressed files or object storage for large evidence;
- partitioned Parquet for tabular tasks, predictions, scores, failures, and
  provenance;
- an embedded/rebuildable analytical catalog (initial candidate: DuckDB);
- pins retained as the publication/distribution layer for sealed campaigns,
  reports, and atlas releases rather than necessarily one pin per task;
- classed local, second-device, and off-machine retention inherited from the
  campaign capsule.

Workers continue writing immutable staging shards; a validating consolidator
updates the catalog. Parallel workers must not mutate one shared database file.
The catalog is an index and query surface, not the sole copy of scientific
evidence.

**Cross-language requirement from the beginning.** The deferred Hub & Spoke idea
in `archive/CAUSAL_STRESS_PYTHON.md` remains directionally valuable but predates
typed targets, fit/score separation, CATE, the capsule, and this evidence model.
Its modernized principle is:

> A conforming CausalStress hub — initially implemented in R — owns certified
> generation, truth separation, scoring, and evidence registration. R, Python,
> Julia, or other estimator spokes consume the same sanitized task protocol and
> return the same typed output protocol. Scientific authority resides in the
> governed protocol and conformance evidence, not permanently in an implementation
> language.

The first schema must therefore avoid R-only identity: canonical JSON manifests,
Parquet/Arrow tables, explicit types/categorical/missingness/unit-order semantics,
protocol versions, estimator ecosystem/package/source/environment fields, typed
scalar/curve/unit/contrast outputs, and structured status/log/resource records.
Store both file SHA-256 (byte integrity) and a governed logical-table digest
(cross-writer equivalence); different Parquet writers can encode the same logical
table into different bytes.

**Activation and sequence:**

1. **Now through WP-02:** parking only. WP-01 evidence closure and both WP-02
   calibration campaigns run on the reviewed capsule/pins architecture. Measure
   artifact counts/sizes, duplication, list/read/consolidation times, query needs,
   minimum retained outputs for rescoring, and actual resume/reuse friction.
2. **After WP-02 and programme G2:** run one bounded, separate deep-design session
   (`EL-0`) before promoting the runner-integrity/persistence RFC and before Wave
   2/RFC-3 exchange and evidence shapes are treated as frozen. Deliver a reviewed
   schema, canonicalization/hash rules, lineage/reuse/admissibility model,
   retention tiers, query use cases, migration plan, and explicit non-goals. No
   database or SDK implementation is authorized by the session.
3. **Protocol conformance before protocol freeze:** after typed CATE/family shapes
   are specified, prove one tiny blind round trip: R hub exports one sanitized
   task; a minimal Python process reads it and returns a trivial typed output; the
   hub verifies, ingests, and scores it. This tests language neutrality without
   building the Python product.
4. **Minimum implementation (`EL-1`):** only the subset required by the next real
   governed family/atlas campaign — immutable artifacts, catalog, exact evidence
   resolution, frozen dependency manifest, and uniform collection of reused/new
   cells. Complete before the first broad post-RFC-3 pathology-atlas campaign, not
   before WP-01/WP-02 science.
5. **Full product later:** Python client/estimator adapters, richer compatible-
   evidence queries, remote/object-store backends, public atlas UI, and large-scale
   compaction follow only after the R end-to-end path and at least one kill-plot
   study demonstrate value. Community demand, not architectural completeness,
   controls expansion.

**Anti-infrastructure guard.** EL-0 is a design-and-review work package with a
fixed timebox and no code. EL-1 must name the next consuming campaign and may not
attempt a universal data lake. The research programme does not wait for a full
atlas platform, Python SDK, remote service, or generalized policy engine before
producing the first family-level scientific result.

**Promotion rule.** Evidence-lake implementation touches Articles II/III/V/VI,
the runner/airlock/fingerprint/persistence contracts, estimator interoperability,
and campaign summaries. It requires an accepted RFC (coordinated with the runner-
integrity RFC), then a ticketed packet. This entry records the design-session
agenda and dependency order only.

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

### Documentation experience follow-ons after v0.2.1 (parked 2026-07-27)

The v0.2.1 documentation release establishes a complete, governed, and
truthful Quarto/pkgdown surface. A later documentation-experience pass should
build on that baseline rather than reopening its contracts:

- add a genuine **60-second quickstart** that reaches one interpretable result
  without requiring readers to understand campaign internals first;
- add **two or three task-oriented how-to guides** organized around concrete
  research questions and informed by actual study experience, rather than by
  package subsystems;
- create a flagship **heavy-tail estimand-boundary page** that demonstrates the
  ATT failure/non-convergence under Cauchy noise, explains why the failure is
  the scientific result rather than a defective DGP, rules out an ordinary ATT
  shootout in that regime, and shows QST as the well-defined distributional
  alternative; and
- make the site more inviting and motivated while preserving the concise
  warning language, typed-target distinctions, and no-cross-scoring guarantees
  established in v0.2.1.

These are presentation and pedagogy improvements, not authority to change the
heavytail DGP, demote its structural ATT signal anchor, or introduce new
estimands. Promotion should name the consuming study material and include a
documentation-truthfulness review.

- GitHub Actions CI for install, tests, and release-gate smoke checks.
  Golden-value tests that depend on generated DGP data or fitted model output
  must distinguish same-substrate locks from cross-substrate CI tolerances:
  exact/tight checks are appropriate for schema-preservation identities
  (e.g. typed row equals the already-computed legacy value), while absolute
  DGP/model reference values must use documented cross-substrate tolerances or
  be explicitly limited to a declared reference substrate. This routes the
  v0.2.0 Batch 3 review note on ATT/QST golden regressions into the CI work.
- Coverage measurement and coverage-regression reporting.
- Build a pkgdown site containing a governed report for every DGP.
- Write user-facing vignettes for the canonical workflow and user-defined
  estimator and DGP contracts, plus audit/reproducibility practices.
- Python spoke / interoperability layer.
- Expanded documentation architecture and articles.
- CRAN release hardening beyond the v0.2.0 release gate.

## Promotion Rule

To leave the horizon, an item must enter an accepted RFC synthesis, the roadmap,
or an active spec packet. Direct implementation from this file is not authorized.
