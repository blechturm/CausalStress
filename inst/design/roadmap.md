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

**Remaining release work:** correct only release-blocking README/roxygen and
governance drift, rerun the local release gate, publish the immutable v0.1.10
archive, and obtain green branch/main/tag CI evidence. The correction and CI
packets named in `README.md` own this work.

**Explicitly deferred from v0.2.0:** CATE execution, parameterized families,
real-data DGP support, a public DGP-extension API, an authoritative feature
roster, persistence retirement, the pkgdown site, and comprehensive vignettes.
No item in that list may be inferred from the v0.2.0 target descriptors or
historical Wave 2 language.

The closed `causalstress_v0_2_0_spec_packet/` remains the historical Wave 1
record. Its internal status headers are not reopened or rewritten; current
packet lifecycle is declared by the governance index.

------------------------------------------------------------------------

## Mandatory post-A2 maintenance release: v0.2.1 — "Retire `qs`"

**Trigger and boundary.** The upstream `qs` package was removed from CRAN on
2026-01-17 and its maintainers now direct users to `qs2`; `.qs2` is not
compatible with the existing `.qs` format. CausalStress still uses `.qs` for raw
batch staging and installs archived `qs` 0.27.3 in CI, although consolidated pin
artifacts already use RDS. This is therefore a package-level persistence and
recoverability risk, not a campaign-wrapper defect.

The `qcb-2026-07-a2` commissioning campaign was stopped after its GenGC leg and
before CFM. Its retained CausalStress 0.1.10 `.qs` artifacts remain immutable
partial commissioning evidence; they are not migrated in place and the campaign
is not resumed or sealed as complete. After the current v0.2.0 CI/release gate
closes, v0.2.1 becomes the next bounded maintenance release and a prerequisite
for the clean task-zero A2 rerun and the two WP-02 calibration campaigns. This
narrowly supersedes the earlier decision to park all CausalStress development
behind WP-02; unrelated feature development remains parked.

**Required v0.2.1 scope (specification and tickets still required):**

1. Make base-R RDS the canonical staging and retained R-object format; keep pins
   on RDS. Benchmark compression and I/O on representative artifacts, but use
   `qs2` only as an optional backend if a material measured need justifies the
   additional format and atomicity surface.
2. Remove `qs` from runtime imports and remove archived-`qs` CI installation.
   New campaigns must produce no `.qs` artifacts.
3. Introduce one internal persistence boundary for atomic write, read, format
   detection, validation, and checksum handling. Scientific/logical identity
   must be distinguished from storage encoding and file-byte identity.
4. Specify fail-closed resume behavior for partial, corrupt, duplicate, legacy,
   and mixed-format batch artifacts.
5. Provide a governed, idempotent legacy converter in a separately frozen
   environment containing archived `qs` 0.27.3. Conversion is read-old/write-new:
   preserve every source `.qs` byte, validate source and target objects, and emit
   per-artifact receipts binding source/target hashes, schemas, R/package
   versions, converter identity, and lineage.
6. Test a representative legacy corpus, corruption and partial-write cases,
   cross-version RDS reads, resume/consolidation behavior, and removal of the
   archived dependency. Document both ordinary use and historical recovery.
7. Build the complete base-registry OCI smoke image under R 4.6.0 and run all
   eight shipped estimators offline, including numeric-sanity and representative
   native/bootstrap CI checks. The 2026-07-21 spike showed that R 4.5.2 passes,
   while R 4.6.0 is blocked specifically by the archived `stringfish`/`qs`
   chain; this gate verifies that the migration removes the blocker rather than
   hiding it in the image. Campaign-local dynamic arms are a separate campaign-
   image acceptance obligation, not v0.2.1 package scope.

**Explicit non-scope.** v0.2.1 does not implement CATE, parameterized DGP
families, runner-integrity migration, Parquet/DuckDB evidence-lake machinery,
Python spokes, or a general storage platform. RDS is the stable R-native bridge;
the later evidence-lake RFC owns normalized language-neutral Parquet/JSON
evidence. The legacy converter creates derivative artifacts and never rewrites,
deletes, or upgrades the evidential status of original campaign evidence.

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
