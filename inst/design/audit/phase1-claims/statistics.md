# Phase 1 Claim Reconstruction — STATISTICS lane

## 1. The claim

For any registered (dgp_id, version) and estimator, CausalStress supports the claim: "this estimator's error against pre-registered, versioned, estimand-typed truth is exactly reproducible and honestly comparable across estimators." It rests on: truth fixed before estimation in two tiers (Constitution Art. I); typed scoring with no cross-scoring — scoreable = requested ∩ produced ∩ truth-available, every unscoreable cell an explicit machine-readable non-comparable row, never a silent omission (Art. I §1.7; contracts.md Estimator/Runner Output Contracts); per-task failure accounting reconciling `length(results) + nrow(errors) == nrow(tasks)`; CI validity gating (<90% bootstrap replicate success ⇒ NA CI + `success = FALSE`); and a per-estimand placebo gatekeeper under a sharp null with zero estimand variance (Art. IV §4.1–4.2).

## 2. Assumptions the claim requires

- Truth correctness: τ(X) deterministic in X only; QST oracle N=10^6 or analytic <1e-5 (§1.3–1.4).
- Same-substrate bitwise identity of data and truth (Art. II §2.2), tolerance-level cross-substrate.
- Schema-4 identity keys (`estimand_target_id`, `metric_id`, truth version, `scoring_population_id`) uniquely separate scientific questions (contracts.md Fingerprints; Art. V §5.2).
- Estimators truthfully declare target population; cross-scoring prevention keys on target id, so a mislabeled population (e.g. population QTE tagged `qst`) corrupts silently — known open hazard (horizon.md, quantile-axis entry).
- Airlock removes `y0`/`y1`/`p`/`structural_te` for honest estimators only; no adversarial claim (contracts.md Security Scope).
- Aggregates are survivorship-honest: per-task failure rows exist, but planned/attempted/succeeded/failed/timeout/missing denominators in summaries are deferred (horizon.md, runner-integrity item 6) — current summaries risk `na.rm` means.
- Gatekeeper Monte Carlo has enough independent placebo runs for the 90% CI-coverage and 10% null-rejection thresholds; multiplicity via the 10/10 rule (§4.2.2). ATE/CATE gate calibration deferred (§4.2.5).
- Declared non-bootstrap `meta$ci_type` CIs are comparable; Monte Carlo uncertainty of benchmark aggregates is not yet guaranteed (horizon.md ACIC item 7).

## 3. Estimand targets

- **ATT** — Structural (noise-free) tier; population scalar; conditioning population = treated units of the generated run sample; finite-sample mean of τ(X_i) over in-sample treated (§1.3, §1.7).
- **ATE** — Structural tier; population scalar over all units of the declared scoring population; finite-sample by default, Wave 1 pinned to the full generated run sample; superpopulation only if a DGP declares it, with a distinct truth descriptor (§1.5; contracts.md; v0_2_0_spec.md line 120). Truth computed scorer-side from `structural_te`.
- **QST** — Distributional tier (signal + noise); curve on the invariant grid τ ∈ {0.01,…,0.99}; treated conditioning (Q_u(Y1|W=1) − Q_u(Y0|W=1)); truth is distribution-level via the N=10^6 oracle — not the run sample's realized quantiles (§1.4).
- **CATE** — Structural tier; unit level; conditional-mean τ(X_i), never realized Y1−Y0; scored on a held-out evaluation sample (second governed seeded draw, `unit_id`-keyed; §1.6, Art. II §2.2, RFC-1 §1.7). In v0.2.0 deterministically non-comparable, reason `target_not_implemented` (contracts.md; roadmap.md).
