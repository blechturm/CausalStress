# CausalStress v0.1.10 Release Closeout

**Status:** CLOSED
**Date closed:** 2026-06-14

This file is opened with the packet so release-gate evidence has a canonical
destination. It must be completed before v0.1.10 closes.

## Shipped

| Ticket | Evidence |
| --- | --- |
| CS-1100 | Governance scaffold committed in `a05679b`; Batch 0 Claude review reported no blocking findings and declared Batch 0 ready to close after review. |
| CS-1101--CS-1104 | Batch 1 implementation committed in `068c0ff`; Claude review found one blocking dependency issue and non-blocking routing notes, all resolved and re-reviewed before commit. |
| CS-1105 | RFC synthesis `inst/design/rfc/20260612_schema3_rng_oracle_synthesis.md` reviewed with no blocking findings; maintainer accepted CS-1105 as amended on 2026-06-12. |
| CS-1106--CS-1107, CS-1117 | Batch 2 implementation reviewed with no blocking findings; review verified schema-3 fingerprints, versioned pins/resume guards, RNG isolation, oracle truth-cache identity/atomicity, and planner/batch version execution. |
| CS-1108--CS-1109 | Batch 3 RFC accepted as amended on 2026-06-13; Claude review reported no blocking findings and verified column-scoped oracle access, Article II/VII substrate-scoped reproducibility, and release-gate substrate evidence. |
| CS-1110--CS-1115 | Batch 4 implementation committed in `5c96068`; Claude review reported no blocking findings. Review non-blockers were resolved or routed before the Batch 4 commit: QST gatekeeper denominator fixed, dead parallel-backend assignment removed, low-bootstrap contract wording routed to CS-1116, and full-suite future-plan cleanup kept in CS-1116. |
| CS-1116 | Batch 5 cleanup committed in `3cacf97`; Claude review reported no blocking findings and verified duplicate alias removal, QST tau warning behavior, robust-bootstrap future-plan restoration, low-bootstrap contract wording, line-ending policy, and audit-routing cleanup. |
| CS-1118 | Batch 6 implementation updated v0.1.10 release surfaces, resolved package-check warnings/notes, ran the release gate, recorded evidence below, and passed final release-gate review with no blocking findings. |

## Migration Notes

- Legacy v0.1.9 result boards with schema-2 pins that lack `dgp_version`
  metadata cannot be proven to match the resolved DGP version under the schema-3
  resume contract. Recompute those runs with `force = TRUE` or use a fresh
  board rather than resuming them.
- v0.1.10 does not claim cross-platform/R-version/BLAS/libm bitwise identity
  unless a version-specific regression corpus proves it. Same-substrate bitwise
  identity remains mandatory, and release-gate evidence must record the
  validation substrate.

## Deferred

| Ticket/Finding | Rationale | Destination |
| --- | --- | --- |
| D1--D3 stale v0.3.0 design examples | `CAUSALSTRESS_DESIGN_v0.3.0.md` is archived verbatim and explicitly non-current under `inst/design/README.md`; editing historical examples would obscure the audit trail. | No active implementation instruction remains; current contracts and Constitution govern. |
| D4 registry-spec stale entries | Active scientific registry specs stay at the design root and must be corrected by a registry-spec version bump, not silent edits to historical scientific claims. | `roadmap.md` Later Horizons: registry-spec version bump |

## Rejected

| Ticket/Finding | Rationale |
| --- | --- |
| M14 as originally stated | Rejected by audit Rev 2; estimators may request non-canonical tau. Residual tau/truth mismatch is handled as a reporting warning in CS-1116. |
| D5 no authority model | Resolved by the governance index, authority order, active packet structure, RFC process, release gate, and archive model installed under CS-1100. |

## Release Gate Evidence

| Gate | Evidence | Result |
| --- | --- | --- |
| Release gate playbook read | Read `inst/design/release_gate.md` on 2026-06-14 and applied all nine required gates. | Pass |
| R CMD check | `& "C:\Program Files\R\R-4.5.2\bin\x64\Rscript.exe" -e "devtools::check(document = FALSE, build_args = '--no-build-vignettes', args = c('--no-manual', '--ignore-vignettes'), error_on = 'never')"` on 2026-06-14 after the v0.1.10 metadata bump. | Pass: `CausalStress_0.1.10`, `Status: OK`, 0 errors, 0 warnings, 0 notes; tests ran inside check and passed. |
| Validation suite | `cs_validate_dgp_registry(strict = TRUE)`, `cs_validate_registry()`, `test-validate-dgp-registry.R`, `test-v018-dgp-sidecar-consistency.R`, `test-validate-all.R`, and `test-validate-dgp.R` under R 4.5.2 on 2026-06-14. | Pass: `registry_strict_rows=24`; `validate_registry_rows=24; all_valid=TRUE`; focused validation tests all passed. |
| Full test suite | `devtools::test(reporter = "summary")` under R 4.5.2 on 2026-06-14, plus the final full `R CMD check` testthat run. | Pass: no failures or errors; expected test warnings were emitted for experimental-DGP fallback and optional estimator internals. |
| Acceptance criteria | Checked `v0_1_10_spec.md`, `v0_1_10_tickets.md`, `tickets.yml`, and this closeout against the shipped rows above. | Pass: all packet tickets CS-1100--CS-1118 are shipped and complete after review. |
| Audit routing | Rechecked the v0.1.9 audit routing through packet tickets, RFCs, closeout deferred/rejected tables, and roadmap/horizon parking. | Pass: every C/M/D finding is ticketed, fixed, rejected with rationale, or deferred to a named non-v0.1.10 destination. |
| README planning state | Checked `inst/design/README.md` on 2026-06-14. | Pass: it still names `causalstress_v0_1_10_spec_packet/` as the active packet pending final review/tag; no premature archive/completion state was written. |
| Known constitutional violations | Checked release gate, Constitution v1.8.2, packet decisions, and open closeout rows. | Pass: no known constitutional violation remains open except by explicit Article II/VII scope amendment already recorded in Constitution v1.8.2. |
| Reproducibility substrate | R 4.5.2 x64 at `C:\Program Files\R\R-4.5.2\bin\x64\Rscript.exe`; platform `x86_64-w64-mingw32`; Windows 10 x64 build 26200 as reported by R; `RNGkind=Mersenne-Twister / Inversion / Rejection`; `sessionInfo()` matrix products `default`; `extSoftVersion()` reports `BLAS` as blank and no `LAPACK` entry; thread env vars `OMP_NUM_THREADS`, `OPENBLAS_NUM_THREADS`, `MKL_NUM_THREADS`, `BLAS_NUM_THREADS`, `VECLIB_MAXIMUM_THREADS`, and `RCPP_PARALLEL_NUM_THREADS` all unset; `.libPaths()` = `C:/Users/maxth/Documents/R/win-library/4.5 | C:/Users/maxth/AppData/Local/R/win-library/4.5 | C:/Program Files/R/R-4.5.2/library`. Same-substrate truth-path probe: two `dgp_synth_baseline_v160(n=250, seed=20260614L, include_truth=TRUE)` calls compared `df`, `true_att`, `true_qst`, and `meta`. | Pass: `include_truth_bitwise=TRUE`; substrate recorded with unavailable BLAS/LAPACK fields explicitly noted as unreported by this R build. |
| Final release-gate review | Claude review accepted Batch 6 / CS-1118 on 2026-06-14 with no blocking findings, independently reproducing the full check, validation suite, and include-truth same-substrate bitwise evidence. | Pass |
