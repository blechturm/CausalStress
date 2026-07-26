# Phase 3 — Lane 2 (DGP) adversarial rebuttal

Orchestrator-run adversarial pass. Target: the strongest Estimands and Statistics
conclusions.

**A1 → Statistics "gatekeeper is sound / caution not punished". Classification: open-factual (scope limit), partial CONCEDE.**
The gatekeeper source is correct (I do not dispute `cs-gatekeeper.R:40-54`). But
its *power* is bounded by the placebo suite, and DGP F5 shows all placebos are
selection-on-observables with exact sharp nulls. So "passed the gatekeeper"
certifies only *non-hallucination under correctly-identified sharp nulls* — an
estimator can pass yet hallucinate under unmeasured confounding, which the suite
never presents. This does not refute the statistics verdict; it **bounds** it.
Not experiment-resolvable inside the current suite (no such DGP exists); routed as
an open-factual scope limitation.

**A2 → Estimands "oracle exactness ⇒ truth is sound". Classification: experiment-resolvable — REINFORCE, not refute.**
Oracle exactness (`lane1_check1`, error 0) is genuine, but it is exactness of
*ATT/ATE structural* truth. My F3 (hd_sparse independence violation, verified vs
Registry §4 line 48) shows a truth-*generation* defect the oracle check cannot
catch, because the oracle recomputes from the same generator. Check to close:
compare `sd(y1-y0-τ)` across all non-placebo DGPs (already run,
`lane2_pathology_v2` §E) — `hd_sparse_plm=0` is the outlier. Conclusion:
oracle exactness ≠ generator correctness; both are needed.

**A3 → Statistics F4 (cross-DGP scale). Classification: experiment-resolvable — CONCEDE and sharpen.**
I supply the magnitude: kangschafer operates on a Y≈210 scale (naive bias −19.7)
vs baseline Y≈1 (`lane2_pathology_v2` §A,F). This makes the statistics
cross-DGP-aggregation caution concrete: raw `mean_abs_error` pooled across these
two DGPs is dominated by kangschafer's scale. Agreed and reinforced.

No further attacks (3 raised, cap respected).
