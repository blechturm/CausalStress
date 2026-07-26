# Phase 3 — Lane 3 (Statistics) adversarial rebuttal

Orchestrator-run adversarial pass. Target: the strongest Estimands and DGP
conclusions.

**A1 → Estimands "no-cross-scoring ⇒ no misleading comparisons (Tier 2 sound)". Classification: experiment-resolvable — CONTEST the scope of "no misleading".**
Per-target scoring is exact (I accept `lane1_check1`). But "no misleading
comparisons" is a claim about what a *user reads*, and the user reads
`cs_summarise_runs` aggregates, which carry **no MC uncertainty** (`cs-summary.R:
58-66`) and can invert under reseeding on heavy-tail DGPs (`lane3_statistics` §C,
ordering flip). So the *system* can still mislead at the layer users consume, even
though the atomic scoring is honest. Resolution: the estimands "sound" verdict
holds only for the atomic row; the packet must state the aggregate caveat. Check
to close: reseeded ranking study with reported SE (cost ≈15 min).

**A2 → DGP "difficulty separates ⇒ meaningful benchmark". Classification: experiment-resolvable — CONCEDE, add a caveat.**
The naive-bias ladder (`lane2_pathology_v2` §A) does separate difficulty — agreed.
But "separates on a single seed" ≠ "separates reproducibly": on heavy-tail DGPs
the *estimator* error is so seed-variable (0.6→2.2) that difficulty *ordering
between estimators* is not stable (my F1). So DGP difficulty is real; estimator
*ranking* on the hardest DGPs is not yet reproducible. Both stand.

**A3 → DGP F4 (10 experimental DGPs). Classification: value + open-factual — AGREE and escalate.**
I add a statistics angle: even the two *stable* DGPs (`baseline`, `heavytail`) are
the ones on which my ranking-instability finding bites hardest (heavytail). So
"stable" certifies reproducible *data/truth*, not reproducible *estimator
comparison*. Whether to gate "stable" on comparison-stability is a maintainer
value decision. Routed.

No further attacks (3 raised, cap respected).
