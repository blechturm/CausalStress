# CausalStress Roadmap

**Status:** Active roadmap
**Authority:** Planning document (authority level 5 per `README.md`); below the
Constitution, `contracts.md`, active packets, and accepted RFC syntheses.
**Last updated:** 2026-06-16

## Completed: v0.1.10

Governance bootstrap (authority README, `contracts.md`, release gate, RFC cycle,
templates) plus the v0.1.9 audit Rev 2 repair cycle. Packet
`causalstress_v0_1_10_spec_packet/` closed 2026-06-14.

------------------------------------------------------------------------

## Current Program: v0.2.0 — "Converge & Freeze"

**Theme.** v0.2.0 lands the public-API break — the typed **estimand registry**
(ratified Constitution v2.0.0 / accepted RFC-1) — **once**, in Wave 1, behind a
legacy compatibility shim, then **converges and freezes** the presentation surface
on top of it. Everything after the freeze is **additive**: CATE, new DGP families,
documentation.

**Guiding principle — the break is owned by RFC-1; the freeze comes after, not
before.** The single largest API change (the estimator output contract
`list(outputs, meta)`) is decided by RFC-1 and implemented in **Wave 1** with a
legacy `list(att, qst, meta)` shim, so existing estimators keep running. The
UX-convergence work (**RFC-2a**) is the *presentation / collector* surface that
displays typed scores — it cannot converge or freeze until that machinery exists,
so it **follows** Wave 1 (RFC-1 §1.13, §4). That is what "break once" means here:
RFC-2a stabilizes what Wave 1 produced; it is not a second break. (Earlier drafts
of this roadmap put RFC-2 before Wave 1 — that contradicted the accepted RFC-1 and
is corrected below.)

### Phase order

0. **[DONE] Constitution v2.0.0 ratified (2026-06-16).** Typed estimand registry
   (ATT, ATE, QST, CATE), typed estimator output contract, per-estimand
   gatekeeper, fit-artifact/score-record persistence grain. The legal foundation;
   RFC-1 consumed.

1. **CI / continuous-enforcement infrastructure.** GitHub Actions guarding every
   subsequent phase. Depends on nothing — the existing test suite and `R CMD check`
   run today — so it starts **now, concurrent with Wave 1 design**, before the
   breaking implementation lands. Constitutionally motivated: Article VII §7.8
   ("compliance MUST be enforced continuously by automated tests") and Article II
   §2.2 (the CI OS/R matrix is the declared substrate for cross-substrate
   reproducibility evidence). Scope:
   - `R CMD check` via `r-lib/actions` on a matrix (ubuntu release + devel,
     windows, macOS), `NOT_CRAN=true` for the full test suite.
   - Test coverage (covr) and a lint job (lintr).
   - A reproducibility-substrate job that records the substrate per Art. II §2.2.
   - The pkgdown build/deploy job is added later with the documentation cycle
     (Phase 6). **Green CI becomes a v0.2.0 release-gate requirement.**

2. **Wave 1 implementation packet — the API break.** The one coordinated breaking
   change, behind the legacy shim: typed scoring for **ATT/ATE**, the
   `outputs`/`meta` estimator output shape (the RFC-1 contract), the fit/score
   split, **schema-4** migration (`fit_fingerprint` / `score_fingerprint`, with the
   score-record layer reserving the Wave 2 eval-identity fields so Wave 2 stays
   additive), the typed **collection/tidy/science** output reshaping, and the
   scorer-only truth channel for ATE (`mean(structural_te)` read off the truth
   side, never exposed to estimators). Consumes RFC-1; updates `contracts.md`.
   CATE staged out via `target_not_implemented`. Lands under green CI from Phase 1.
   Spec drafted in `causalstress_v0_2_0_spec_packet/`; activates after its review
   amendments are folded in and the ticketing-blocking open decisions are closed.

3. **RFC-2a — scalar/typed-scoring UX freeze + API-stability policy.** Converges
   and freezes the presentation / collector / runner UX for the now-shipped typed
   scalar surface (RFC-1 §1.13). May freeze **after** Wave 1; the CATE UX portion
   stays experimental until Wave 2. Carries the stability contract — deprecation,
   semver commitment, and the pre-CRAN change budget — that makes "keep it
   constant" enforceable rather than aspirational.

4. **Wave 2 implementation packet — CATE.** Additive on the frozen scalar surface:
   held-out evaluation sample (eval seed/size/`unit_id` per Art. II §2.2), the
   covariates+`unit_id` predict airlock, PEHE plus a robust companion metric, and
   the heterogeneity-**detection** gatekeeper component (not per-unit ≈0). No API
   break.

5. **RFC-2b (CATE UX freeze) + RFC-3 (families).** **RFC-2b** freezes the
   per-unit / CATE presentation surface once Wave 2 has fixed its shape. **RFC-3**
   reconciles `CAUSAL_STRESS_FAMILIES_SPEC_v3_2_final.md` into the governed
   registry: stress-dial-indexed families with frozen oracle truth under Article
   VII immutability — the infrastructure behind continuous breakdown / "kill-plot"
   studies. Families are additive DGPs on the stable contract; their *design* may
   begin in parallel earlier since it does not touch the estimator/runner API, but
   they land after the surface freezes.

6. **Documentation cycle.** Vignettes, per-DGP scientific documentation, DGP
   registry docs, and the pkgdown site (deployed via a CI job extending Phase 1).
   Sequenced last because docs rot if written before the API freezes — but staged:
   the API-reference portion may start the moment the scalar surface freezes
   (post-Wave 1 / RFC-2a) and run concurrently with Wave 2 and RFC-3; the final
   pkgdown/vignette polish covers the families and is a **release-gate requirement**
   for v0.2.0 so it is not skipped.

### Why this order (dependencies)

- **CI first**: it depends on nothing and protects every later phase, so it lands
  before the breaking implementation rather than after the damage.
- **Wave 1 before RFC-2a**, not the reverse: the API break is owned by RFC-1 and
  lands in Wave 1 behind a legacy shim; RFC-2a *freezes the presentation surface*,
  which can only converge after typed scoring exists (RFC-1 §1.13, §4). Putting the
  UX RFC first would freeze a UI for machinery that does not yet exist.
- **Wave 1 → Wave 2; RFC-2a → RFC-2b**: CATE and its UX freeze are additive and
  only safe to build on the proven scalar infrastructure.
- **Docs last**: documentation written against a moving API is wasted; it follows
  the freeze and the families it must describe; its pkgdown deploy extends the
  Phase 1 CI.

------------------------------------------------------------------------

## Parallel & deferred tracks

Tracked in `horizon.md`; these do not gate the main v0.2.0 arc and may proceed
independently:

- **GenGC bootstrap** stabilization (estimator-side, no contract change).
- **Gatekeeper recalibration RFC** — pass/fail calibration, difficulty tiers, and
  the `Non-Robust` label for the new ATE/CATE gate components (Constitution
  §4.2.5 defers these here).
- **mirai/mori parallelism spike** — evaluate against the Wide & Shallow rule and
  Article VI atomic persistence before any parallel-execution RFC.
- **Registry-spec version bump** for stale scientific registry entries
  (`CAUSAL_STRESS_DGP_REGISTRY_1.4.0.md`), corrected by version bump, never by
  editing scientific history.
- **Real-data external-truth RFC** — a generalized external-truth tier for
  ATE/CATE on real DGPs (Constitution §1.7 defers this).
- **Python spoke** — only after the R package contract is stable (post-freeze).

------------------------------------------------------------------------

## Non-scope for v0.2.0

- A generalized external-truth tier for real-data ATE/CATE (deferred RFC).
- Gatekeeper pass/fail recalibration (deferred RFC).
- Parallel execution beyond experimental mode under Article VI.
- Python interoperability.
- CRAN submission beyond the v0.2.0 release gate.

------------------------------------------------------------------------

## Later horizons

- Continuous breakdown-curve / "kill-plot" studies built on the RFC-3 families
  (the methodological payoff the stable foundation enables).
- Sensitivity-analysis design as a DGP stress-axis RFC, if still desired.
- Public article / vignette polish once the runner and artifact contracts have
  shipped and settled.
