# Phase 3 — Lane 1 (Estimands) adversarial rebuttal

Orchestrator-run adversarial pass (isolation limitation as noted). Target: the
**strongest** opposing conclusions from the DGP and Statistics reports.

**A1 → Statistics F1 (ranking instability). Classification: experiment-resolvable — CONCEDE with a boundary.**
Statistics claims heavy-tail rankings flip across seeds. This does **not** weaken
the estimands-lane "no-cross-scoring is exact" conclusion — they are different
layers: per-(target,truth) scoring is bitwise-correct (`lane1_check1`), while
cross-seed *aggregate* ranking is a separate, unquantified operation
(`lane3_statistics` §C). Concession: my "sound" verdict must be scoped to the
**atomic score row**, not to aggregate rankings. Named check to close: the
existing `lane3_statistics.R` §C extended to B≥200 bootstrap and 100 seeds with a
reported MC SE (cost ≈15 min).

**A2 → DGP F3 (hd_sparse degenerate QST). Classification: experiment-resolvable — REINFORCE.**
DGP says `hd_sparse_plm` QST truth is flat +1. This compounds my F1 (QST
tail-precision): on a degenerate-QST DGP, *any* QST "error" is either sampling
noise or a mislabeled location estimate. Check that closes it: score a correct
location-shift estimator's QST on `hd_sparse_plm` and confirm its non-zero
abs_error is entirely finite-sample (cost ≈5 min). Bearing: QST should be reported
as **non-informative** on constant-shift DGPs, not scored as if discriminating.

**A3 → DGP F5 (identification-failure absent). Classification: value.**
Whether the instrument *should* cover unmeasured confounding is a scope/value
decision for the maintainer, not a validity defect. I only add the definitional
constraint: the estimand definitions (§1.3–1.6) are all *structural*/conditional
on X, so they are **incoherent** to score under an identification-failure DGP
without a new truth tier — i.e., extending scope requires estimand-definition
work, not just a new DGP. Routed to maintainer.

No further attacks (cap respected; 3 raised).
