# ACIC 2026 Prior-Art and CausalStress Compatibility Audit

**Status:** Informative research note; non-binding unless promoted through an
RFC or spec packet

**Date:** 2026-07-21

**Evidence cutoff:** 2026-07-21

**Scope:** The public ACIC 2026 challenge design, its position in causal
benchmarking, and its scientific, technical, and legal compatibility with the
current CausalStress programme

> **Reliability caveat.** This is a focused prior-art and compatibility audit,
> not a systematic review of every causal benchmark. ACIC claims were checked
> against the official challenge site, conference presentation, and public
> repository. CausalStress claims were checked against the authority chain and
> current source tree. The audit did not have access to the participant-gated
> ACIC data, generator, truth, or scorer. Absence from the public repository is
> therefore reported as a public-evidence limitation, not proof that the
> organizers do not possess the corresponding artifact.

## 1. Executive verdict

**ACIC 2026 is currently the stronger scientific challenge; CausalStress is
currently the stronger reusable benchmark substrate.**

ACIC 2026 has substantially greater demonstrated scientific breadth: 9,000
distinct populations, five randomized treatment arms, individual/sample/
subgroup/population targets, treatment-choice outputs, uncertainty evaluation,
hidden truth, and independent community participation. CausalStress has the
stronger implemented integrity contracts: typed estimands, no-cross-scoring,
versioned DGP truth, scorer-only truth handling, fit/score fingerprints, atomic
persistence, and explicit non-comparability.

Neither dominates the other:

- ACIC 2026 is a challenge and scientific comparison exercise, not currently a
  public, licensed, reusable DGP framework.
- CausalStress is not currently a comparably broad scientific benchmark. CATE
  is ratified but deliberately returns `target_not_implemented`; continuous
  families and complete survivorship-honest stress summaries remain designs or
  parked work; and only two of the 24 registered DGP-version rows are `stable`.

The correct strategic relationship is **complementarity**, not competition.
Paper 3 should position CausalStress as an integrity-first laboratory for
governed stress mechanisms and failure boundaries, with ACIC 2026 as an
important breadth/community comparator.

The timing matters. The official site describes ACIC 2026 as the challenge's
return after a multiyear hiatus. CausalStress therefore originated during a real
period of ACIC inactivity; its premise was not irrational. The external landscape
changed during development. What must change now is the positioning and evidence
burden, not the historical record or the existence of the project.

## 2. Audit method and claim classes

The review used two evidence groups:

1. **ACIC primary public evidence**
   - official challenge site and submission specification;
   - official ACIC 2026 conference presentation;
   - official public GitHub repository snapshot.
2. **CausalStress current evidence**
   - `inst/design/README.md` authority order;
   - Constitution v2.0.0 and `contracts.md`;
   - current DGP registry and target contracts;
   - active roadmap, `horizon.md`, and the unpromoted families specification.

Claims below use three implicit classes:

- **Verified:** explicit in public ACIC material or current CausalStress code/
  binding design.
- **Inference:** a design implication derived from verified facts and labelled
  as such.
- **Unknown:** not specified in the public ACIC evidence inspected.

## 3. What ACIC 2026 actually benchmarks

### 3.1 Design

The official materials describe:

- **9,000 datasets**, each drawn from a distinct large population;
- **sample size (n=1,000)** per dataset;
- a completely randomized **five-arm** treatment
  (Z \in \{a,b,c,d,e\}), with `a` as control;
- common marginal covariate distributions across populations, with continuous,
  binary, and nominal covariates;
- population-to-population variation in treatment probabilities, effect sizes,
  conditional-effect complexity, heterogeneity, and functional form;
- a lower-compute **18-dataset curated track** and a full 9,000-dataset track.

Because treatment is randomized within each population, the challenge focuses
on heterogeneous-effect learning, multiple treatments, aggregation, and
uncertainty rather than observational-confounding recovery. This is an
important difference from CausalStress's overlap, propensity, and
misspecification stress programme.

### 3.2 Targets

For each active arm (z \in \{b,c,d,e\}) versus control (a), ACIC requests:

1. **iCATE:** the conditional mean effect at every observed covariate vector;
2. **sCATE:** the sample average of those conditional effects;
3. **subCATE:** sample-average effects within two prespecified subgroups defined
   by (X_{12}\);
4. **PATE:** average treatment effects over the large source population;
5. the **best active treatment** under each of the four aggregation levels.

Point estimates and pointwise 95% intervals are requested for all four effect
targets. Best-treatment outputs are discrete argmax decisions. The conference
results additionally examine simultaneous behavior, including family-wise
error for sCATE comparisons.

The distinction among sCATE and PATE is methodologically valuable: the former is
tied to the realized sample covariates, while the latter is a source-population
quantity. CausalStress already distinguishes finite-sample from superpopulation
ATE in principle, but ACIC makes that distinction an operational benchmark
target.

### 3.3 Evaluation and participation

The public specification names bias, squared error, interval coverage, and
interval width as standard evaluation criteria. The organizers explicitly
decline to construct a single overall ranking, recognizing that methods can
trade performance across targets and metrics.

The conference presentation reports 32 registered teams, 18 submitting teams,
and 63 submissions. This constitutes independent community evidence that
CausalStress does not yet possess.

### 3.4 Public-evidence limitations

As of the evidence cutoff:

- data access required challenge registration and Society for Causal Inference
  membership;
- the public repository exposed the site source and presentation, but no DGP
  generator, datasets, truth store, scorer implementation, release, or detected
  license file;
- the public site describes metrics generically but does not fully freeze the
  aggregation rule for every metric, tie handling, incomplete-submission
  denominators, Monte Carlo uncertainty, or failure classification;
- no public checksums, container image, or oracle-validation report were found.

These are limitations on **external reproducibility from public artifacts**,
not allegations about the internal conduct of the challenge. Hidden truth is a
strength for blinded evaluation and, simultaneously, a constraint on later
independent reproduction unless artifacts are released.

## 4. What “state of the art” means here

There is no single state-of-the-art benchmark. At least four separable axes
matter:

1. **Scientific challenge design:** meaningful targets, difficult variation,
   hidden truth, uncertainty evaluation, and independent submissions. ACIC 2026
   is strong here.
2. **Benchmark breadth:** many populations, multiple treatments, and targets at
   different aggregation levels. ACIC 2026 is substantially ahead of current
   CausalStress here.
3. **Reusable experimental infrastructure:** executable generators, stable
   contracts, provenance, leakage control, persistence, and reproducible
   scoring. CausalStress is stronger than the publicly visible ACIC artifact
   here.
4. **Mechanism-oriented diagnosis:** controlled pathology axes, continuous
   severity, failure boundaries, and survivorship honesty. This is
   CausalStress's intended differentiator, but families/kill plots are not yet
   an implemented scientific result.

The earlier CausalStress prior-art audit remains relevant: ACIC 2026 does not,
on the public evidence, occupy the precise proposed union of continuous stress
dials, explicit estimator-death accounting, and mean-versus-distributional
estimability under heavy tails. It does, however, invalidate any suggestion
that contemporary causal benchmarks are confined to binary ATE/CATE point
estimation.

## 5. ACIC 2026 versus current and planned CausalStress

| Dimension | ACIC 2026 | CausalStress implemented now | CausalStress planned only |
| --- | --- | --- | --- |
| Primary object | Blinded scientific challenge | Reusable R benchmark instrument | Broader benchmark laboratory |
| Treatment domain | Five-arm randomized | Binary treatment | No governed multi-arm packet |
| Effect targets | iCATE, sCATE, subCATE, PATE | ATT, finite-sample ATE, treated QST/QTT | Held-out CATE |
| Decision targets | Best arm at four levels | None | None ratified |
| Distributional targets | None requested | Treated marginal quantile shift | Population QTE parked |
| Uncertainty | Pointwise intervals; multiplicity is central | Optional intervals; ATT/QST gate components | Calibrated ATE/CATE gates |
| DGP breadth | 9,000 populations; 18 representatives | 24 version rows across 12 IDs; two stable | Parameterized families |
| Stress emphasis | Heterogeneity, response-surface complexity, multiple treatments | Overlap, tails, nonlinearity, misspecification, high dimension, sharp nulls | Continuous severity/kill plots |
| Truth isolation | Withheld by organizers | Scorer-only truth/airlock contract | Held-out CATE prediction airlock |
| Target integrity | Submission schemas by target | Typed targets and no-cross-scoring | Generalized estimand schema parked |
| Provenance | Dataset/team/submission IDs; backend details not public | Versioned DGPs, RNG obligations, fit/score fingerprints | Full input receipts/resume strengthening parked |
| Failure accounting | Public rules incomplete | Error and non-comparable score states | Complete survivorship denominators parked |
| Adoption evidence | 18 submitting teams | No independent adoption demonstrated | Community-use ambition |
| Public DGP reuse | No public licensed generator found | Native executable DGP registry | External-truth/interoperability RFC parked |

### 5.1 What ACIC does better

- Demonstrated scale, breadth, and independent participation.
- Multiple treatments rather than repeated binary-only exercises.
- Explicit sample/subgroup/population target distinctions.
- Joint attention to effect estimation, uncertainty, multiplicity, and treatment
  choice.
- Hidden-truth evaluation that limits direct benchmark overfitting.
- A curated low-compute track that reduces entry barriers.
- Refusal to collapse multidimensional performance into one leaderboard number.

### 5.2 What CausalStress does better

- Estimand identity is executable, not a filename convention.
- Incompatible outputs become non-comparable rather than silently cross-scored.
- DGP versions, truth obligations, RNG behavior, and status are governed.
- The estimator-facing airlock and scorer-only truth channel reduce accidental
  leakage.
- Fit and score artifacts have separate identities.
- Atomic persistence and typed status records support recoverable campaigns.
- QST/QTT extends evaluation beyond conditional means.
- The intended stress-family design asks *where and why* estimators break, not
  merely which submission has lower average error.

The last two infrastructure claims must remain calibrated: complete
survivorship-honest public summaries and continuous families are not yet shipped
capabilities.

## 6. Can ACIC 2026 DGPs be incorporated?

**Potentially, but not by wholesale import under current evidence.** “Include
the ACIC DGPs” describes three different objects.

### 6.1 User-supplied static dataset adapter — conditionally feasible

CausalStress could accept participant-supplied ACIC datasets without
redistributing them. A governed adapter would:

- require dataset IDs and cryptographic checksums;
- record source and access provenance;
- keep organizer-supplied truth in a distinct scorer-only channel;
- distinguish a static benchmark dataset from an executable DGP;
- emit `truth_unavailable` where truth has not been legally supplied;
- never certify the adapter as “ACIC 2026” without a fidelity check against
  organizer documentation.

This is the lowest-risk path, but it permits reproduction only by users with
legitimate access to the same inputs.

### 6.2 Wrapper around an official released generator — preferred

If the organizers release a licensed generator, the preferred integration is an
external wrapper pinned by source commit or container digest. It would need:

- a declared seed and population-identity contract;
- exact generator/scorer versions;
- golden dataset and truth checksums;
- treatment-arm and contrast metadata;
- a license permitting execution, redistribution where desired, and derived
  adapters;
- explicit truth-loading boundaries.

External execution is preferable to copying the generator into CausalStress:
it preserves upstream identity and reduces accidental scientific divergence.

### 6.3 Independent reconstruction — not ACIC 2026

Reconstructing a generator from the website or slides would be scientifically
non-equivalent and may be legally unauthorized. Such work may be valuable as an
**ACIC-inspired multi-arm family**, but must not be labelled an ACIC 2026 DGP or
used to claim reproduction of challenge results.

### 6.4 Required promotion gates

No adapter or DGP integration should begin until all applicable gates pass:

1. **Rights gate:** written license/permission covers the proposed use.
2. **Identity gate:** dataset/generator/truth versions and checksums are known.
3. **Fidelity gate:** the wrapper reproduces organizer-provided golden outputs.
4. **Truth-separation gate:** estimator code cannot access held-out truth.
5. **Scientific-use gate:** a named study or comparator exercise consumes the
   integration; no dead registry rows.

## 7. Technical compatibility: multi-arm is not “CATE plus a loop”

CausalStress currently assumes a binary treatment and one treated-versus-control
contrast. Faithful ACIC support would require a new major package/constitutional
era, including:

- treatment-domain metadata and a declared reference arm;
- factor-valued treatment input rather than binary `w` assumptions;
- arm-indexed propensity and potential-outcome structures;
- contrast-indexed target truth and estimator outputs;
- score identities containing treatment and reference arms;
- arm-aware capability and non-comparability checks;
- joint covariance or simultaneous-inference representations;
- tie-aware treatment-choice truth and scoring;
- multi-arm airlock, DGP validation, gatekeeper, persistence, and summary tests.

Running four independent one-versus-control tasks would be useful only as an
explicit approximation. It discards cross-arm dependence, multiplicity, and the
joint decision problem that makes ACIC 2026 interesting.

## 8. Estimand and output implications

| ACIC object | Closest CausalStress concept | Classification | Decision |
| --- | --- | --- | --- |
| iCATE | Held-out unit-level CATE | Effect estimand | **Core:** implement Wave 2 binary CATE first |
| sCATE | Finite-sample ATE of conditional effects | Effect estimand | Preserve sample-scoring-population identity; multi-arm form deferred |
| subCATE | GATE/subgroup ATE | Effect estimand | **Defer:** valid group inference is not merely averaging CATE predictions |
| PATE | Superpopulation ATE | Effect estimand | **Defer:** add only for a consuming study; never cross-score as finite-sample ATE |
| Best treatment | Treatment rule/argmax | Decision target | **Defer:** eventually prefer value/regret and tie-aware scoring over accuracy alone |
| Coverage/width/FWER | Inference metrics | Not an estimand | Add to the metric/inference vocabulary where scientifically valid |
| QST/QTT | No direct ACIC target | Distributional estimand | Retain as a CausalStress differentiator |

The audit therefore answers “are we missing estimands besides CATE?” with
**yes, but none demands immediate implementation**. The important missing axes
are subgroup and source-population effects, multi-arm contrasts, and decision
targets. Building all of them now would be scope creep.

The current scoped registry position remains sound:

- v1 vocabulary stays ATT, ATE, treated QST/QTT, and CATE;
- targets are added lazily only with a producer and a consuming study;
- population QTE remains the likely next binary-treatment addition when an
  actual population-QTE estimator is registered;
- quantiles of individual (Y(1)-Y(0)) remain rejected without a governed
  coupling/rank-invariance assumption;
- treatment choice must not be misrepresented as an ordinary effect estimand.

## 9. Implications for Paper 3 and novelty

### 9.1 Claims weakened by ACIC 2026

CausalStress should not claim novelty merely for:

- benchmarking heterogeneous effects;
- evaluating several aggregation levels;
- considering estimator uncertainty;
- generating many causal datasets;
- providing multiple performance metrics without a single rank.

ACIC 2026 is strong prior art for all of these.

### 9.2 Claims that remain potentially defensible

Subject to implementation and scientific validation, the distinctive
contribution remains the combination of:

- governed, versioned oracle stress families;
- typed estimand/target-population identity and no-cross-scoring;
- continuous mechanism-severity profiles rather than a scenario leaderboard;
- explicit planned/attempted/succeeded/failed denominators;
- refusal to plot conditional-performance curves through estimator death zones;
- mean-versus-quantile/distributional estimability boundaries under heavy tails;
- reproducible campaign identity and retained evidence.

The strongest positioning is:

> CausalStress is an integrity-first, extensible laboratory for locating and
> explaining causal-estimator failure boundaries. It complements blinded data
> challenges such as ACIC 2026; it does not claim greater current breadth.

### 9.3 Evidence required before making the claim

- finish and freeze the current campaign evidence chain;
- implement and validate the already-ratified held-out CATE target;
- promote at least one parameterized family through RFC-3;
- ship survivorship-honest status denominators;
- run serious incumbent estimators under frozen tuning and inference rules;
- demonstrate at least one independent external use or reproduction path.

## 10. Actionable conclusions

### Immediate, non-code actions

1. Cite ACIC 2026 in all future benchmarking/prior-art narratives.
2. Contact the organizers for generator, data, truth/scorer, license, checksum,
   and version-history information, plus explicit permission for an external
   CausalStress adapter.
3. Preserve the distinction between public evidence and participant-gated
   artifacts in every claim.
4. Record ACIC's estimand map as an input to the post-Wave-2 estimand-schema RFC,
   not as an implementation backlog.

### Current programme actions

5. Do not interrupt the current campaign or v0.2.0 closure.
6. Implement Wave 2 CATE before expanding to GATE, PATE, policy targets, or
   multi-arm treatment.
7. Promote one continuous family and survivorship-honest reporting before Paper
   3 claims mechanism-level validity envelopes.
8. Treat simultaneous coverage/FWER and Monte Carlo uncertainty as metric-layer
   requirements where intervals are compared.

### Conditional future actions

9. If rights and artifacts are available, write a narrow external-ACIC adapter
   RFC/spec; do not bundle gated inputs by default.
10. Consider multi-arm support only as a major-era RFC after Wave 2 and RFC-3,
    and only with a named consuming study.
11. Design eventual best-treatment evaluation around policy value/regret and
    ties, not classification accuracy alone.
12. Seek an independent reproducer/contributor: community use is necessary if
    CausalStress is to claim infrastructure value rather than internal utility.

### Explicit non-actions

- Do not reverse-engineer ACIC 2026 from slides and call it reproduction.
- Do not represent 9,000 static datasets as 9,000 reusable DGPs.
- Do not implement an estimand catalog in anticipation of hypothetical users.
- Do not fake multi-arm support through silent repeated binary scoring.
- Do not claim that current CausalStress is scientifically broader than ACIC
  2026.

## 11. Sources

### ACIC 2026 primary sources

- ACIC 2026 Data Challenge. Official site, design, targets, submission format,
  FAQ, and preliminary-results link.
  https://acic2026datachallenge.github.io/
- ACIC 2026 Data Challenge conference presentation. Preliminary participation
  and performance reporting.
  https://acic2026datachallenge.github.io/acic_presentation.pdf
- ACIC 2026 public site repository.
  https://github.com/acic2026datachallenge/acic2026datachallenge.github.io

### Licensing interpretation

- GitHub Docs. “Licensing a repository.” A public repository without a license
  retains default copyright protections; public visibility alone does not grant
  reuse or derivative-work rights.
  https://docs.github.com/en/repositories/managing-your-repositorys-settings-and-features/customizing-your-repository/licensing-a-repository

### Local CausalStress evidence

- `../README.md` — authority order and implementation-versus-horizon reading.
- `../CAUSAL_STRESS_CONSTITUTION.md` — ATT/ATE/QST/CATE definitions, typed
  scoring, staged CATE, held-out evaluation, and airlock obligations.
- `../contracts.md` — Wave 1 output/scoring contract and non-comparability.
- `../../../R/cs-dgp-registry.R` — 24 DGP-version rows; two stable.
- `../roadmap.md` — Wave 2 CATE then RFC-3 families sequencing.
- `../CAUSAL_STRESS_FAMILIES_SPEC_v3_2_final.md` — unpromoted family and
  survivorship design.
- `prior_art_killplot_heavytail_benchmarking.md` — wider causal-benchmark and
  simulation-infrastructure prior-art audit.

## 12. Provenance

The first pass was a clean-context audit tasked to look for disconfirming
evidence rather than defend CausalStress. The primary agent then checked its
high-impact claims against the official ACIC site/public repository and the
CausalStress authority chain. No code was changed and no participant-gated ACIC
artifact was accessed. Findings were routed into `horizon.md`; that routing is a
parking-lot decision only and does not authorize implementation.
