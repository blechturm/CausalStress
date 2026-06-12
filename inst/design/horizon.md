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

## Deferred Tooling Work

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
