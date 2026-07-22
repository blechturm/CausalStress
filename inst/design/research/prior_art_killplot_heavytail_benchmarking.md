# Prior-Art Assessment: Survivorship-Honest Heavy-Tail Breakdown Curves for Causal Estimators

**Status:** Research note (authority level 7 — informative, non-binding unless promoted via RFC/packet)
**Date:** 2026-06-14
**Author:** Max Thomasberger, with two model-assisted deep-research passes (see Provenance)
**Scope:** Causal inference / treatment-effect estimation and statistical-simulation methodology; literature and software through 2025 (plus two 2026 arXiv preprints verified against live pages).

> **Purpose.** Map the prior art for a proposed CausalStress/GenGC contribution: a *continuous-stress-dial, survivorship-honest, oracle-truth benchmark contrasting mean-based vs quantile/distributional estimability across SOTA causal estimators* ("kill-plots"). This note exists to let a future related-work section be framed **defensibly** rather than hopefully. It is descriptive, not normative — it does not authorize implementation.

> **Reliability caveat (read first).** This is a model-assisted, fan-out literature search with adversarial claim verification — **not** a systematic review. Confidence is bounded by retrieval coverage: a near-match could exist outside the ~42 sources retrieved (working papers, software vignettes, uplift-modeling venues). Every claim below is annotated with its verification vote (e.g. *3-0* = unanimous confirm; *2-1* = split). Author lists, page numbers, and titles flagged "[verify]" were not confirmed character-for-character against the source and must be checked before formal citation. Two sources dated 2026 are beyond the assistant's knowledge cutoff and were verified only against live arXiv pages.

---

## 1. Executive verdict

**The proposed combination is unoccupied in the confirmed prior art.** No retrieved work does all of: (i) a *continuous* stress-dial sweep of a DGP family, (ii) survivorship-honest estimator failure-**rate** reporting, and (iii) a mean-vs-quantile/distributional estimability contrast under heavy tails, across a broad SOTA causal-ML panel (TMLE, AIPW/DML, GRF, BART, IPW, plus a distributional method). Each ingredient exists **separately**; their union does not.

**But one ingredient is already published and must be cited as such.** Athey, Bickel, Chen, Imbens & Pollmann (2023, *JRSSB*) [S1] empirically demonstrate the heavy-tail mean-failure result that the GenGC "Heavy Tail Advantage" plot reproduces: under Cauchy noise the difference-in-means estimator has **SD ≈ 127.1** while their semiparametric-efficient estimator achieves **SD ≈ 0.02** (verified *2-1/3-0*). The CausalStress novelty therefore **cannot** be "we show mean estimation fails under heavy tails" — that is a top-journal result. The defensible novelty is the **lens and the instrument**: the continuous transition curve, the failure-rate honesty, the breadth of the estimator panel (Athey et al. compare only difference-in-means/median against their own estimators — no TMLE/AIPW/GRF/BART/IPW), and the reproducible oracle-backed families.

**Two framing corrections fall out of the evidence:**
1. Athey et al.'s estimand is the **ATE** (randomized setting), and their estimator uses quantile *weighting* to efficiently estimate a *location* effect — it does not switch to a distinct QTE estimand. Do not over-attribute a "QTE" result to it.
2. Quantiles are **not universally robust**: Li & Castro-Camilo (2026) [S10] find extreme-QTE methods themselves fail in extreme regimes due to data sparsity. This matches the GenGC QST plot's own tail blow-up. The honest claim is "**central/median** quantile targets are estimable where the mean is not," not "quantiles are robust."

---

## 2. Prior art by axis

### Axis A — Large-scale causal-ML benchmarks: discrete factorial, mean-only

- **ACIC 2016 / Dorie et al. (2019, *Statistical Science*)** [S2, S3]. Six discrete/categorical "knobs" (nonlinearity, % treated, overlap, alignment, treatment-effect heterogeneity, magnitude); 216 combinations → 77 "most interesting" scenarios × 100 reps = 7,700 realizations. **Discrete factorial, not a swept continuous dial** (*3-0*; one minor nuance — "magnitude" has some continuous variation). Estimand is **mean SATT only**; "quantile" appears solely for covariate selection and interval estimation, never as an estimand (*3-0*). Heavy-tailed outcome noise is a **fixed** structural property (Student-t coefficients), never varied as a knob (*3-0*). No performance-vs-severity curves; results reported as the IQR of biases across the 77 settings.
- **Curth, Svensson, Weatherall & van der Schaar (2021, NeurIPS Datasets & Benchmarks), "Really Doing Great at Estimating CATE?"** [S4]. The closest *benchmarking-critique* prior art — it argues semi-synthetic benchmarks (IHDP, ACIC2016) "can systematically favor some algorithms over others." But the DGP is varied only by a **discrete on/off toggle** (original vs modified-additive IHDP); **no** continuous stress sweep, **no** survivorship enforcement, **no** injected heavy-tail/infinite-variance noise (the phrase "heavy tail" there refers to the RMSE-score distribution, not outcome noise), and the estimator scope is CATENets CATE learners, not the broad ATT/ATE SOTA suite (*3-0*).

### Axis B — Heavy-tail mean-vs-quantile estimability: the key overlap

- **Athey, Bickel, Chen, Imbens & Pollmann (2023, *JRSSB* 85(5):1615) [verify page/issue]** [S1]. THE closest result. Table 1: difference-in-means SD = 127.149 under Cauchy vs ~0.020 for their efficient estimator; "the variance bound for the ATE is infinite because the moments of the Cauchy distribution do not exist"; the efficient estimator "has an interesting interpretation as a weighted average of quantile treatment effects." **However:** simulation uses exactly **three discrete families** (Normal, Laplace/double-exponential, Cauchy) — *no continuous tail-thickness sweep, no breakdown curve* (zero occurrences of "Student"/"degrees of freedom"/"tail index"/"breakdown"/"sweep") — and benchmarks **only** its own estimators vs difference-in-means/median (no TMLE/AIPW/GRF/BART/IPW) (*3-0*). Estimand is ATE, not ATT (verification nuance, *2-1* on the QTE-attribution framing).
- **Deuber, Li, Engelke & Maathuis (2024, *JASA*), extremal QTE for heavy-tailed distributions** [S5] (arXiv:2110.06627). Develops a "causal Hill estimator" for extremal QTE — i.e., quantile causal targets *estimable* under heavy tails. But a **method-introduction paper**: three discrete DGP models with fixed tail indices, single real-data application, **no** SOTA causal-ML panel (zero matches for TMLE/AIPW/DML/GRF/BART/IPW). Headline contrast is extremal-vs-ordinary quantiles, **not** an explicit mean-vs-quantile failure comparison (*2-1/3-0*).
- **"Assessment of Treatment Effect Estimators for Heavy-Tailed Data" (Tripuraneni et al., 2021/2022; Jordan group) [verify authors]** [S6] (arXiv:2112.07602). Establishes a bias-variance tradeoff — "procedures that aggressively downweight or truncate large values, while introducing bias, lower the variance enough to ensure that the treatment effect is more accurately estimated" — across ~699 Amazon supply-chain RCTs. A **selection/meta-analysis** method, not a df/Cauchy stress sweep, and uses difference-of-means as a noisy ground-truth label (*3-0*).
- **RDML (2024) [S7]; "Robust X-Learner" (2026) [S8, beyond cutoff]**. Each proposes a *single* new robust estimator and varies contamination only at a few **discrete** levels (RDML: mixing weight 0.1/0.2/0.3, t-df 1.5/3; compares only DML vs RDML variants). No continuous dial, no breakdown curve, no broad panel (*3-0*).
- **Counterpoint — Li & Castro-Camilo (2026), "Tail-Calibrated Estimation of Extreme QTE" [S10, beyond cutoff]**. "Standard QTE methods often fail in extreme regimes due to data sparsity." This is the honest counterweight to the quantile-robustness thesis: quantile targets are *not* magic at the extreme tails (consistent with the GenGC QST plot's boundary divergence) (*3-0*).

### Axis C — Survivorship / non-convergence honesty: generic standard, never instantiated in causal benchmarking

- **Morris, White & Crowther (2019, *Statistics in Medicine* 38(11):2074–2102, doi:10.1002/sim.8086)** [S11]. Establishes the standard: "The number of missing values… (for example due to nonconvergence) is the first performance measure to assess," and further metrics should be "tentative" when methods differ in missingness. **Domain-general, not causal-specific** (*3-0*).
- **Pawel et al. (2024), review of 482 simulation studies [verify authors/title]** [S12] (arXiv:2409.18527). Only **23%** mention missingness, **19%** report its frequency, **14%** report handling; recommends "always quantify and report missingness, even if none was observed" (*3-0*). I.e. the standard exists and is **routinely ignored**.
- **No causal-estimator benchmark in the confirmed evidence enforces it** (reports failure rates alongside conditional metrics; refuses to plot "death zones"). This is genuinely open white space.

### Axis D — Simulation-design methodology: factorial is the recommended default, not continuous sweeps

- **Morris–White–Crowther (2019)** [S11]: "Varying these factorially is likely to be more informative than one-by-one away from a base-case data-generating mechanism," recommending a fractional-factorial design when full grids are too expensive. No discussion of continuous severity sweeps or breakdown curves (*3-0*). So the continuous-breakdown-curve philosophy is **not** the established default — which cuts both ways: it's a differentiator, but reviewers steeped in MWC may need persuading it is more informative than a dense factorial grid.

### Axis E — Reproducible benchmark infrastructure: per-framework verdict

Criteria: **(a)** continuous stress-dial DGP families; **(b)** frozen/known oracle truth; **(c)** DGP/run provenance-versioning-fingerprinting; **(d)** survivorship failure-**rate** reporting; **(e)** heavy-tail noise **and** quantile/distributional estimands.

| Framework | (a) cont. dial | (b) oracle truth | (c) fingerprint | (d) failure-rate | (e) heavy-tail + quantile | Source |
| --- | --- | --- | --- | --- | --- | --- |
| **SimDesign** (Chalmers & Adkins 2020) | No — discrete factorial (`createDesign`) | Partial — **user-supplied** (`bias(…, parameter=)`) | No — seeds only (`SEED` col, L'Ecuyer); no DGP hash | Partial — catches/counts errors & warnings, halts after >50 consecutive; not a rate estimand | No | [S13–S15] |
| **simChef** (Yu Group) | No — discrete grid (`add_vary_across`, Cartesian) | No built-in store | No — caching compares tibble rows, **explicitly does not hash DGP source** (stale caches not invalidated) | No — catches errors (`.err`) but halts; no rate | No (Gaussian example; `dgpoix` preset-DGP sibling early-stage) | [S16, S17] |
| **rsimsum** (Gasparini 2018) | No — **post-hoc** results summariser, not a DGP runner | n/a (consumes user results) | No | **Unsettled** — `na.rm` silent-drop claim was *refuted* (0-3); exact behavior unverified | No | [S18] |
| **RealCause** (Neal, Huang & Raghupathi 2020) | No — objective is **realism** (model fit to real data) | Yes — **learned/model-derived** oracle | Not established (broad-negative claim *refuted* 0-3; unverified) | Not established (unverified) | Not established (unverified); estimates mean ATE/ITE | [S19] |
| **JustCause** (inovex) | No — discrete `simple`/`hard` toggle | Yes — simulated ITE | No | No | No — four mean ATE/ITE metrics (PEHE, ATE-bias, …); no quantile, no heavy-tail | [S20] |
| **CATENets** (Curth & van der Schaar 2021) | No — discrete structural counts (`simulate_treatment_setup`) | Benchmark datasets (IHDP/Twins/ACIC2016) | No | No | No — **mean CATE only** | [S21] |

**Net (Axis E):** every confirmed framework is discrete-grid at best; oracle truth is user-supplied (SimDesign), absent as a store (simChef), learned (RealCause), or simulated-mean (JustCause/CATENets); **provenance is seed-only with no DGP content-fingerprinting anywhere**; failure handling is error-catching, never a reported non-convergence **rate**; and **every estimand is a mean effect** (ATE/ITE/PEHE/SATT) — none ships heavy-tail noise support or quantile/distributional targets. CausalStress's combination (versioned/fingerprinted oracle families + airlock + failure-rate reporting + QST) is not matched by any confirmed framework.

---

## 3. The defensible white space (precise statement)

A reproducible benchmark that **simultaneously**:
1. sweeps a **continuous** pathology dial (e.g. t-degrees-of-freedom / Cauchy-contamination fraction) to produce **breakdown curves**, not discrete factorial buckets;
2. computes **frozen, fingerprinted, oracle** truth per stress level (CausalStress already does this post-v0.1.10);
3. **enforces survivorship honesty** — reports estimator failure/non-convergence **rates** and refuses to draw conditional-metric curves through "death zones";
4. **contrasts mean-based vs quantile/distributional estimability** across a **broad SOTA causal panel** (TMLE, AIPW/DML, GRF, BART, IPW, GenGC-QST).

Each of the four exists in isolation (Axes A–E). Their **union** is unoccupied in the confirmed prior art.

---

## 4. Framing implications for CausalStress / GenGC

- **Do not claim the heavy-tail mean-failure result as novel.** Cite Athey et al. (2023, JRSSB) [S1] and position against it: *"they established the endpoint for two estimators (difference-in-means vs an efficient quantile-weighted estimator) at discrete distributions; we map the continuous transition across the SOTA estimator suite, with failure-rate honesty and reproducible oracle-backed families."*
- **Claim the median, not the tails.** Frame robustness as central/median quantile estimability (Li & Castro-Camilo 2026 [S10] shows extreme quantiles also break). The GenGC QST plot's tail divergence is *consistent with the literature* — present it as honest breakdown, not a flaw.
- **The survivorship enforcement is the strongest standalone methods claim** (Axes C/D): a known-but-ignored standard (MWC 2019 [S11]; only ~14–23% compliance per Pawel et al. 2024 [S12]), operationalized as a software contract — not done in any causal benchmark.
- **The instrument/infrastructure claim is defensible** (Axis E): no confirmed framework provides fingerprinted oracle stress-dial families with failure-rate reporting and a distributional estimand.

---

## 5. Open questions / unverified (honest gaps)

These were **named in the queries but produced no surviving verified claim** — neither confirmed nor excluded. They are the residual risk to a novelty claim and should be checked manually before any formal related-work section:

1. **Infrastructure not verified:** `aciccomp`/`aciccomp2016` (Dorie), **EconML** (Microsoft) benchmarks, **CausalML** (Uber) benchmarks, **DoWhy**/**Ananke** tooling, `causaldata`. Sources were not confirmed; verdicts on (a)–(e) are unknown.
2. **ACIC 2017 / 2019 / 2022** competitions remain unverified. **ACIC 2026 is
   no longer an open gap:** it was separately audited in
   `prior_art_acic_2026_benchmark_audit.md`. Its five-arm, multi-level CATE/PATE,
   treatment-choice, and uncertainty design materially raises the scientific-
   breadth comparator. On the public evidence it still does not implement the
   proposed continuous pathology dial + survivorship-enforced breakdown curve +
   mean-versus-distributional heavy-tail contrast, so it narrows the framing but
   does not occupy the Section 3 union.
3. **Knaus, Lechner & Strittmatter (2021, *Econometrics Journal* 24(1):134), "Machine learning estimation of heterogeneous causal effects: Empirical Monte Carlo evidence"** [S22]: source located (arXiv:1810.13237; repo github.com/MCKnaus/CATEs) but **no claim survived verification** — does its EMCS sweep a continuous dial or report failures? Unresolved.
4. **Mahajan, Mitliagkas, Neal & Syrgkanis, "Empirical Analysis of Model Selection for Heterogeneous Causal Effect Estimation"** [S23] (arXiv:2211.01939): source located, no surviving claim. Continuous severity? Survivorship? Unresolved.
5. **rsimsum's** actual handling of failed/missing estimates is unsettled (the `na.rm` silent-drop claim was refuted 0-3).
6. Is the continuous-breakdown-curve design documented anywhere in the broader robust-statistics literature (e.g. breakdown-point analysis applied to causal estimators) under a different name — i.e. is the novelty in the *method* or only in its *application to SOTA causal ML*?

---

## 6. Sources

Quality and verification notes are from the deep-research passes. "[verify]" marks bibliographic details not confirmed character-for-character; "[beyond cutoff]" marks 2026 preprints verified only against live arXiv.

- **[S1]** Athey, S., Bickel, P. J., Chen, A., Imbens, G. W., & Pollmann, M. (2023). *Semiparametric estimation of treatment effects in randomized experiments.* Journal of the Royal Statistical Society Series B, 85(5):1615 [verify issue/pages]. arXiv:2109.02603. https://academic.oup.com/jrsssb/article/85/5/1615/7226451
- **[S2]** Dorie, V., Hill, J., Shalit, U., Scott, M., & Cervone, D. (2019). *Automated versus Do-It-Yourself Methods for Causal Inference.* Statistical Science 34(1):43–68. doi:10.1214/18-STS667. https://projecteuclid.org/journals/statistical-science/volume-34/issue-1/Automated-versus-Do-It-Yourself-Methods-for-Causal-Inference/10.1214/18-STS667.full ; arXiv:1707.02641
- **[S3]** ACIC 2016 competition design document ("testing grounds"), ERIC ED591944. https://files.eric.ed.gov/fulltext/ED591944.pdf
- **[S4]** Curth, A., Svensson, D., Weatherall, J., & van der Schaar, M. (2021). *Really Doing Great at Estimating CATE? A Critical Look at ML Benchmarking Practices in Treatment Effect Estimation.* NeurIPS 2021 Datasets & Benchmarks. arXiv:2107.13346. https://datasets-benchmarks-proceedings.neurips.cc/paper_files/paper/2021/hash/2a79ea27c279e471f4d180b08d62b00a-Abstract-round2.html
- **[S5]** Deuber, D., Li, J., Engelke, S., & Maathuis, M. H. (2024). *Estimation and Inference of Extremal Quantile Treatment Effects for Heavy-Tailed Distributions.* Journal of the American Statistical Association [verify vol/pages]. arXiv:2110.06627. https://arxiv.org/abs/2110.06627
- **[S6]** *Assessment of Treatment Effect Estimators for Heavy-Tailed Data* (Tripuraneni et al., 2021/2022) [verify authors]. arXiv:2112.07602. https://arxiv.org/abs/2112.07602 (also ResearchGate 357046735)
- **[S7]** Robust Double Machine Learning (RDML), 2024 [verify authors/title]. https://pmc.ncbi.nlm.nih.gov/articles/PMC11566156/
- **[S8]** Robust X-Learner, 2026 [beyond cutoff; verify authors/title]. arXiv:2601.15360. https://arxiv.org/abs/2601.15360
- **[S10]** Li & Castro-Camilo (2026). *Tail-Calibrated Estimation of Extreme Quantile Treatment Effects* [beyond cutoff; verify authors/title]. arXiv:2603.23309. https://arxiv.org/html/2603.23309
- **[S11]** Morris, T. P., White, I. R., & Crowther, M. J. (2019). *Using simulation studies to evaluate statistical methods.* Statistics in Medicine 38(11):2074–2102. doi:10.1002/sim.8086. https://onlinelibrary.wiley.com/doi/full/10.1002/sim.8086
- **[S12]** Pawel, S., et al. (2024) [verify authors/title]. Review of 482 simulation studies (missingness reporting). arXiv:2409.18527. https://arxiv.org/abs/2409.18527
- **[S13]** Chalmers, R. P., & Adkins, M. C. (2020). *Writing Effective and Reliable Monte Carlo Simulations with the SimDesign Package.* The Quantitative Methods for Psychology 16(4):248–280 [verify]. Docs: https://philchalmers.github.io/SimDesign/
- **[S14]** SimDesign CRAN + intro vignette. https://cran.r-project.org/web/packages/SimDesign/vignettes/SimDesign-intro.html ; https://search.r-project.org/CRAN/refmans/SimDesign/html/createDesign.html
- **[S15]** SimDesign source + "Catch errors" article. https://github.com/philchalmers/SimDesign ; https://philchalmers.github.io/SimDesign/articles/Catch_errors.html
- **[S16]** simChef (Yu Group). https://github.com/Yu-Group/simChef ; https://yu-group.github.io/simChef/
- **[S17]** simChef `vary_across` / `run_experiment` reference. https://yu-group.github.io/simChef/reference/vary_across.html ; https://yu-group.github.io/simChef/reference/run_experiment.html
- **[S18]** Gasparini, A. (2018). *rsimsum: Summarise results from Monte Carlo simulation studies.* Journal of Open Source Software 3(26):739 [verify]. https://github.com/ellessenne/rsimsum ; https://cran.r-project.org/web/packages/rsimsum/
- **[S19]** Neal, B., Huang, C.-W., & Raghupathi, S. (2020). *RealCause: Realistic Causal Inference Benchmarking.* arXiv:2011.15007. https://arxiv.org/abs/2011.15007 ; https://github.com/bradyneal/realcause
- **[S20]** JustCause (inovex). https://github.com/inovex/justcause ; https://justcause.readthedocs.io/
- **[S21]** Curth, A., & van der Schaar, M. (2021). *Nonparametric Estimation of Heterogeneous Treatment Effects: From Theory to Learning Algorithms.* AISTATS 2021. arXiv:2106.03765. CATENets: https://github.com/AliciaCurth/CATENets ; https://catenets.readthedocs.io/
- **[S22]** Knaus, M. C., Lechner, M., & Strittmatter, A. (2021). *Machine learning estimation of heterogeneous causal effects: Empirical Monte Carlo evidence.* Econometrics Journal 24(1):134 [verify pages]. arXiv:1810.13237. https://academic.oup.com/ectj/article-abstract/24/1/134/5854188 ; repo https://github.com/MCKnaus/CATEs — *(located; no verified claim — open)*
- **[S23]** Mahajan, D., Mitliagkas, I., Neal, B., & Syrgkanis, V. *Empirical Analysis of Model Selection for Heterogeneous Causal Effect Estimation* [verify authors/year]. arXiv:2211.01939. https://arxiv.org/abs/2211.01939 — *(located; no verified claim — open)*

---

## 7. Provenance

Derived from two background deep-research workflow runs on 2026-06-14:

- **Pass 1** (concept prior art) — run `wf_6e8ab84a-996`: 5 search angles, 21 sources fetched, 93 claims extracted, 25 verified (25 confirmed, 0 killed), 103 agents.
- **Pass 2** (infrastructure + named-but-unconfirmed literature) — run `wf_bc02a81a-953`: 5 angles, 21 sources fetched, 98 claims extracted, 25 verified (22 confirmed, 3 killed), 103 agents. Three refutations recorded: simChef-as-continuous-dial (1-2), rsimsum `na.rm` silent-drop (0-3), RealCause broad-negative (0-3).

Verification protocol per claim: 3 independent adversarial verifier votes; a claim is killed only on ≥2/3 refutes. Confidence is bounded by retrieval coverage — see the reliability caveat at the top. Promotion of any conclusion here into a roadmap item or paper claim requires manual confirmation of the Section 5 open questions and the "[verify]" bibliographic details.
