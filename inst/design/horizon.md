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
