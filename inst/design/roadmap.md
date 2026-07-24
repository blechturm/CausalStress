# CausalStress Roadmap

**Status:** Active roadmap
**Authority:** Planning document (authority level 5 per `README.md`); below the
Constitution, `contracts.md`, active packets, and accepted RFC syntheses.
**Last updated:** 2026-07-24

## Completed: v0.1.10

Governance bootstrap (authority README, `contracts.md`, release gate, RFC cycle,
templates) plus the v0.1.9 audit Rev 2 repair cycle. Packet
`causalstress_v0_1_10_spec_packet/` closed 2026-06-14.

------------------------------------------------------------------------

## Current Program: close v0.2.0

v0.2.0 is a narrow public-release closure, not the complete multi-wave program
described by earlier roadmap revisions.

**Implemented release surface:** Constitution v2.0.1; typed ATT, finite-sample
ATE, and QST scoring; canonical long-form score collection; schema-4 fit,
score-record, and score-row identities; legacy ATT/QST projections; and CI/release
enforcement. CATE remains a registered descriptor with deterministic
`target_not_implemented` behavior. It is not an implemented v0.2.0 capability.

**Remaining release work:** independently review the completed emergency RDS
persistence correction in CS-1228, rerun the final-tree local gate, publish the
immutable v0.1.10 archive, and obtain green branch/main/tag CI evidence. The
correction and CI packets named in `README.md` own this work.

**Explicitly deferred from v0.2.0:** CATE execution, parameterized families,
real-data DGP support, a public DGP-extension API, an authoritative feature
roster, legacy `.qs` conversion, generalized/configurable persistence, the
pkgdown site, and comprehensive vignettes. No item in that list may be inferred
from the v0.2.0 target descriptors or historical Wave 2 language.

The closed `causalstress_v0_2_0_spec_packet/` remains the historical Wave 1
record. Its internal status headers are not reopened or rewritten; current
packet lifecycle is declared by the governance index.

------------------------------------------------------------------------

## Emergency v0.2.0 correction — "Retire `qs`"

**Trigger and boundary.** The work previously planned as v0.2.1 became a v0.2.0
release blocker when clean branch CI on R 4.6 proved that archived `qs` 0.27.3
itself no longer compiles against current R headers. The first failed commit
`13bd7a2` exposed archived `stringfish` 0.17.0; the narrow `56376a6` trial made
`stringfish` compile and exposed the underlying `qs` failure. Transitive pinning
cannot make the current runtime dependency support the required matrix.

CS-1228 in the active correction packet now owns only the minimum safe removal:

1. Make base-R RDS the sole runtime format for individual-result staging,
   campaign-batch staging, and oracle caches; keep pin artifacts on RDS.
2. Remove `qs` from runtime imports, tests, and CI bootstrap. New package
   operations produce no `.qs` files.
3. Use one small internal, non-pluggable persistence boundary for repeated
   atomic write/read behavior. Storage bytes do not define scientific or logical
   identity.
4. Fail closed when legacy `.qs` staging appears in an active staging directory.
   Preserve it and instruct the user to rerun in a clean directory. Ignore
   immutable legacy oracle caches and compute a separate `.rds` entry.
5. Test atomicity, corrupt/partial/invalid artifacts, resume, consolidation,
   duplicates, and legacy/mixed staging; then rerun fresh local and R
   release/devel remote gates.

**Explicit non-scope.** v0.2.0 does not add `qs2`, a codec plug-in layer,
dual-writing, a general storage platform, or a legacy converter. Existing
pre-release `.qs` artifacts are not read, mutated, deleted, or migrated by the
package. The maintainer is the current package consumer and has accepted a clean
rerun while that cost is still small. Any future recovery utility runs outside
CausalStress under separate authority. Parquet/JSON evidence-lake work, an OCI
campaign image, CATE, and parameterized families remain separately planned.

------------------------------------------------------------------------

## Planning gate for v0.3.0 and later science

Do not assume that CATE is automatically the next scientific release. Before a
v0.3.0 packet is drafted, hold a focused planning session that compares:

1. **Parameterized DGP families**, which are currently the more immediate
   scientific need because they support continuous breakdown curves and
   kill-plot studies; and
2. **CATE execution**, which would exercise held-out unit-level outputs and
   resolve remaining target/persistence ambiguities alongside ATT/ATE/QST.

The planning session may choose families first or a deliberately bounded
parallel program. It must name consuming studies, dependencies, identity and
truth requirements, and independent review gates. This roadmap does not decide
that sequence in advance.

A separate documentation release, with its version assigned during planning,
owns the pkgdown site, a report for every DGP, a canonical-workflow vignette,
and contract vignettes for user-defined estimators and DGPs. Until a public DGP
extension contract is designed, documentation must not advertise one.

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
