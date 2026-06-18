# CausalStress v0.2.0 Release Closeout

**Status:** CLOSED - PUBLIC TAG BLOCKED BY CI PACKET
**Date closed:** 2026-06-18

This closeout records evidence for the v0.2.0 Wave 1 typed-scoring packet. It is
not a public tag authorization. The Wave 1 packet is closed after final review,
but the public v0.2.0 tag remains blocked until
`causalstress_v0_2_0_ci_packet` satisfies the roadmap CI release-gate
requirement.

## Shipped

| Ticket | Evidence |
| --- | --- |
| CS-1200 | Contract baseline committed in `ecfffec`; Claude review found no blocking findings and verified `contracts.md` records the typed `outputs`/`meta` contract, legacy shim, target support, non-comparable reasons, and schema-4 identity. |
| CS-1201--CS-1203 | Typed scoring core committed in `ab5d970`; Claude re-review found MAJOR-1 closed, verified target descriptors, output normalization, three-way scoring, scorer-only ATE truth, and airlock coverage, and declared Batch 1 ready to commit. |
| CS-1204--CS-1205 | Schema-4 and output surfaces committed in `9c4ae5c`; Claude review verified fit/score identity, populated `scoring_population_id`, nullable Wave 2 fields, schema 1-3 resume fail-closed behavior, and typed collection/science/audit surfaces. |
| CS-1206--CS-1208 | Batch 3 committed in `c45aaae`; Claude re-review verified CATE-only hard rejection, mixed-target `target_not_implemented` rows, structure-only ATE gate slot, and the added golden ATT/QST compatibility regression. |
| CS-1209 | Release-gate implementation updated release metadata and NEWS, fixed a GenGC formula-parser regression found by the full suite, routed the Batch 3 golden-value CI note into `horizon.md`, ran the local release gates, and recorded evidence below. |

## Migration Notes

- New runs write schema-4 score records with distinct `fit_fingerprint` and
  `score_fingerprint` identities. Schema 1-3 artifacts remain historical inputs
  for explicit compatibility collectors, but they are not valid resume targets
  for schema-4 runs; resume attempts fail closed with a classed migration error.
- The canonical result surface is the long-form typed score table. ATT/QST
  helpers remain compatibility projections and are not independent scoring
  authorities.
- CATE is a governed target in v0.2.0 but remains staged out in Wave 1:
  CATE-only tasks hard-reject before estimator execution, while mixed-target
  tasks emit a `target_not_implemented` row for CATE and continue scoring
  implemented targets.
- GenGC QST bootstrap intervals remain experimental/unvalidated inference. The
  heavy-tail campaign should use `ci_method = "none"` unless a later RFC changes
  that posture.

## Deferred / Routed

| Item | Rationale | Destination |
| --- | --- | --- |
| RFC-2a scalar UX freeze timing | The active spec names this as a maintainer decision required before final closeout. The roadmap default is that RFC-2a follows Wave 1. | Final release-gate review / maintainer acceptance |
| Roadmap Phase 1 CI infrastructure | `roadmap.md` says green GitHub Actions CI becomes a v0.2.0 release-gate requirement, but the active Wave 1 spec explicitly lists CI/coverage infrastructure as non-scope unless promoted by a later packet. No `.github/` directory existed at Wave 1 closeout. | `causalstress_v0_2_0_ci_packet`; public v0.2.0 tag remains blocked until this packet closes green. |
| Gatekeeper recalibration | Wave 1 adds only a structure-only ATE slot and intentionally does not decide thresholds, difficulty tiers, or registry consequences. | `horizon.md` Gatekeeper recalibration RFC |
| Real-DGP generalized external truth | Wave 1 preserves `truth_unavailable` for ATE/CATE without external truth and does not add a generalized external-truth tier. | Horizon / future RFC |
| CATE implementation and CATE UX | Wave 1 registers and stages CATE only. Held-out evaluation, PEHE, prediction APIs, and CATE UX remain out of scope. | Roadmap Wave 2 and RFC-2b |
| Cross-substrate golden-value tolerance policy | Batch 3 review noted that absolute DGP/model golden values must not be 1e-12 locks on CI substrates. The note was routed into the CI/coverage horizon entry. | `horizon.md` CI/coverage work |

## Release Gate Evidence

| Gate | Evidence | Result |
| --- | --- | --- |
| Release gate playbook read | Read `inst/design/release_gate.md` on 2026-06-17 and applied all nine required gates for the active packet. | Pass |
| R CMD check | `& "C:\Program Files\R\R-4.5.2\bin\x64\Rscript.exe" -e "devtools::check(document = FALSE, build_args = '--no-build-vignettes', args = c('--no-manual','--ignore-vignettes'), error_on = 'never')"` on 2026-06-17 after the v0.2.0 metadata bump. | Pass under the release-gate rule: `CausalStress_0.2.0`, 0 errors, 0 warnings, 1 NOTE (`unable to verify current time`, environmental). Tests ran inside check and passed. |
| Validation suite | `devtools::load_all(quiet=TRUE); cs_validate_dgp_registry(strict=TRUE); cs_validate_registry(); testthat::test_file(...)` for `test-validate-dgp-registry.R`, `test-v018-dgp-sidecar-consistency.R`, `test-validate-all.R`, and `test-validate-dgp.R` under R 4.5.2 on 2026-06-17. | Pass: `registry_strict_rows=24`; `validate_registry_rows=24`; `all_valid=TRUE`; focused validation tests passed. |
| Full test suite | `devtools::test(reporter = "summary")` under R 4.5.2 on 2026-06-17 after fixing the GenGC additive-formula regression found by the first full-suite run. | Pass: no failures or errors. Expected warnings remain for experimental DGP fallback, small GenGC groups, unsupported CI fallback, and RNGkind notices. |
| GenGC gate fix | The first full-suite run failed because `est_gengc()` and `est_gengc_dr()` passed `y ~ . - w` to the v0.2.0 additive formula parser. Both wrappers now build explicit covariate formulas with `stats::reformulate()` after airlock filtering. | Pass: targeted rerun of `test-estimator-gengc.R`, `test-estimator-gengc-dr.R`, `test-integrity-capabilities.R`, `test-qst-ci-gengc.R`, `test-v019-e2e.R`, and `test-viz-proof.R` passed; the subsequent full suite passed. |
| Acceptance criteria | Checked `v0_2_0_spec.md`, `v0_2_0_tickets.md`, `tickets.yml`, and tests covering typed targets, no-cross-scoring, non-comparable reasons, ATE truth, real-DGP truth unavailability, CATE staging, schema-4 identity, typed collection/audit output, scorer-only airlock, unchanged gatekeeper policy, and legacy ATT/QST numerical compatibility. | Pass locally, subject to final review of the CI-scope issue above. |
| Audit/review routing | Batch 0--3 Claude reviews were routed before the next batch. Batch 3's cross-substrate golden-value note was routed to `horizon.md`. Gate-discovered GenGC formula-parser failures were fixed in CS-1209. | Pass locally, subject to final review of the CI-scope issue above. |
| README planning state | Updated `inst/design/README.md` on 2026-06-17 to state that the v0.2.0 packet is still active and Batch 4 is implemented awaiting final review/tag. | Pass: no premature archive/completed-packet state was written. |
| Packet closeout exists | This file records commands, evidence, routed items, and final-review blockers/questions. | Pass |
| Known constitutional violations | Checked Constitution v2.0.0, the active spec, ticket dispositions, and release-gate findings. | No known Wave 1 implementation violation remains open. The roadmap CI tension is a process/release-scope issue for final review, not a silent constitutional deferral. |
| Reproducibility substrate | R 4.5.2 x64 at `C:\Program Files\R\R-4.5.2\bin\x64\Rscript.exe`; platform `x86_64-w64-mingw32`; Windows 11 x64 build 26200 as reported by R CMD check; governed generation runs under `RNGkind=Mersenne-Twister / Inversion / Rounding` via the scoped `cs_set_rng()` contract, which restores the ambient session default afterward (`Mersenne-Twister / Inversion / Rejection` in the release-gate shell); `extSoftVersion()` reports `BLAS` as blank and no `LAPACK` entry; thread env vars `OMP_NUM_THREADS`, `OPENBLAS_NUM_THREADS`, `MKL_NUM_THREADS`, `BLAS_NUM_THREADS`, `VECLIB_MAXIMUM_THREADS`, and `RCPP_PARALLEL_NUM_THREADS` all unset; `.libPaths()` = `C:/Users/maxth/Documents/R/win-library/4.5`; `C:/Users/maxth/AppData/Local/R/win-library/4.5`; `C:/Program Files/R/R-4.5.2/library`. Same-substrate truth-path probe: two `dgp_synth_baseline_v160(n=250, seed=20260617L, include_truth=TRUE)` calls compared `df`, `true_att`, `true_qst`, and `meta`. | Pass: `include_truth_bitwise=TRUE`; all four compared fields identical. |
| Final release-gate review | Claude reviewed Batch 4 / CS-1209 and found no code-correctness blocker. Review required correcting the RNG substrate evidence and splitting unrelated `horizon.md` changes out of the CS-1209 commit; both were done before commit `808c6f5`. Review adjudicated CI as tag-blocking but not Wave-1-commit-blocking. | Pass for Wave 1 packet closeout; public tag blocked by CI packet. |

## Final Review Disposition

1. RFC-2a may begin after Wave 1; no additional UX cleanup packet is mandated by
   governance before RFC-2a, though the maintainer may choose to insert one.
2. CI absence does not invalidate the Wave 1 packet because CI was out of scope
   for that active spec, but it does block the public v0.2.0 tag under
   `roadmap.md`. The CI requirement is promoted into
   `causalstress_v0_2_0_ci_packet`.
