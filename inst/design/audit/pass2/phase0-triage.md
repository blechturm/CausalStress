# Phase 0 — Triage: Scientific Design Review, PASS 2 (sidecar-aware)

**Git HEAD:** `9c16cd2d6a8915aef808bb3c865b8224778fa227` · **Date:** 2026-07-26
**Orchestrator:** Claude. Findings routed via action classes, never applied.
Pass-1 deliverables (`inst/design/audit/*.md`, `checks/`) are **preserved** as a
record — including their correction trail; pass 2 writes only under
`inst/design/audit/pass2/`.

## Why a second pass

Pass 1 made two consequential errors, both from the **same root cause: reviewers
did not read the DGP sidecars or per-DGP scientific narratives**, so they
misclassified *intended demonstrations* as defects:

1. **heavytail.** Pass 1 flagged "ATT estimator rankings flip across seeds on
   `synth_heavytail`" as an aggregation defect (its headline "materially
   misaligned" finding). It is the **opposite**: the noise is
   `0.8·N(0,0.5)+0.2·Cauchy(0,1)`, which has **no finite mean**, so mean-based ATT
   is *deliberately* ill-posed and the sample mean cannot converge — the DGP
   exists to demonstrate exactly this ("L2 break"; sidecar `noise: heavy`,
   `tags:[robustness,heavytail]`; Const. §1.3 prohibits sample-mean truth in
   heavy tails and pairs the DGP with QST). The instability is the correct result.
2. **hd_sparse_plm.** Pass 1 found shared within-unit noise (sd(y1−y0−τ)=0) vs
   Registry §4 line 48 "ε₀,ε₁ independent." This one may be a genuine finding OR
   an intended constant-shift design — pass 1 never checked the sidecar/narrative.

Both must be **re-adjudicated from scratch** with sidecar + narrative context.

## MANDATORY rule for every lane (pass 2)

**Read, for every DGP, before classifying any of its behavior:**
`inst/dgp_meta/<id>.yml` (sidecar: `stress_profile`, `difficulty.stars`, `tags`,
`meta.description`) **and** `inst/dgp_meta/<id>.Rmd` (the per-DGP scientific
narrative), plus `inst/design/CAUSAL_STRESS_DGP_REGISTRY_1.4.0.md` and
`CAUSAL_STRESS_FAMILIES_SPEC_v3_2_final.md`.

**Intent-first classification.** A behavior that *fulfils a DGP's declared
scientific intent* is **SOUND**, not a finding — even when it looks pathological
(e.g. non-converging ATT under Cauchy noise is success, not failure). Only
behavior that (a) contradicts the DGP's own declared intent, (b) contradicts a
governing clause, or (c) would mislead a user *who read the DGP's documentation*
is a finding. Every DGP-touching finding must quote the sidecar/narrative line it
relies on and state why the behavior is *not* the intended demonstration.

## New cross-cutting question (all lanes): estimand appropriateness per DGP

For each (DGP, estimand) pair: is the estimand **well-posed** on that DGP (does the
population target exist), is it the **intended** comparison target, and does the
benchmark **signal** which estimand a user should compare on that DGP? The
legitimate, intent-aware version of pass-1's botched aggregation finding is:
*does the tool prevent a user from naively ranking a mean-based ATT on a
no-mean DGP, or at least tell them to use QST there?*

## Scientific surface (unchanged from pass 1)

12 DGP IDs / 24 immutable versions, all synthetic; 8 estimators; stable =
`synth_baseline`, `synth_heavytail`. Estimands: ATT/ATE finite-sample structural;
QST distributional (treated, oracle N=10⁶); CATE unit-level held-out,
`target_not_implemented`. Typed no-cross-scoring. Gatekeeper: sharp-null placebo
suite; ATT ≥90% coverage; QST 10/10 rule.

## Claim under review (unchanged)

CausalStress identifies where estimators succeed, degrade, and fail under
controlled synthetic stress, without misleading comparisons across estimands,
populations, failures, or DGPs. Tiers: reproducible ≠ scientifically valid ≠
externally representative. Contract compliance out of scope.

## Lane assignments (unchanged) + salvage

- **Lane 1 estimands**, **Lane 2 dgp**, **Lane 3 statistics** — as pass 1.
- **Salvaged, corrected checks** available to build on (do not re-derive from
  zero): `inst/design/audit/checks/lane1_check1_typed_scoring.R` (oracle
  exactness / no-cross-scoring / airlock — all PASS),
  `lane2_pathology_v2.R` (12-DGP pathology; note the `include_truth=FALSE`
  generator quirk — call with `include_truth=TRUE`), `lane3_statistics.R`
  (ranking / gatekeeper; its survivorship probe was DEFECTIVE — rebuild via
  `cs_run_grid`, not result-lists). New pass-2 checks go under
  `inst/design/audit/pass2/checks/` (prefix `p2_lane{1,2,3}_`).

## Environment & process

- `"C:\Program Files\R\R-4.5.2\bin\x64\Rscript.exe"` + `pkgload::load_all(".")`;
  deps in user library 4.5; do NOT use R 4.6. Set `NOT_CRAN=true`; explicit seeds.
- Isolation restored this pass: Phase 1 = docs-only (now **including** sidecars +
  `.Rmd` narratives + registry + families), 3 fresh isolated agents; Phase 2 =
  3 fresh isolated agents, full repo; Phase 3 = each lane attacks the others;
  Phase 5 = fresh meta-review that MUST check whether any "defect" contradicts a
  DGP's declared intent (the pass-1 error class).
