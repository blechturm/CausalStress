# CausalStress v0.2.0 Release Corrections Closeout

**Status:** ACTIVE
**Closeout state:** CS-1225 complete after independent review; emergency
CS-1228 implementation is complete after independent review; fresh final gates
remain pending; CS-1226 and CS-1227 remain incomplete
**Date opened:** 2026-07-24
**Date closed:** TBD
**Packet:** `causalstress_v0_2_0_correction_packet`

## Shipped

| Ticket | Evidence |
| --- | --- |
| CS-1220 | Constitution v2.0.1 was ratified and applied with the accepted uppercase `X1...Xk` clause and three `real-data DGP` prose corrections. Independent Claude review on 2026-07-24 returned **APPROVE** with no blocker, major, or minor findings and confirmed the v2.0.0 history, `type="real"`, historical RFCs, closed Wave 1 spec, and implementation artifacts remain unchanged. Local follow-up confirmed the authoritative R YAML parse and Git's declared LF normalization. |
| CS-1221 | Added one shared canonical synthetic-covariate predicate across internal contract validation, all-version registry execution, and public certification. All 24 registered versions pass; malformed and absent covariate names fail closed; validation restores RNG state. Independent Claude final review returned **APPROVE** with no actionable findings. |
| CS-1222 | Corrected schema-4 identity so QST curve rows share one `score_fingerprint` and carry unique deterministic `score_row_fingerprint` values. Runner metadata, pins, tidy/collector projections, science payloads, and result/batch audit surfaces preserve both levels. Independent Claude final review returned **APPROVE** with no actionable findings. |
| CS-1223 | Replaced the stale v0.1.x README with the current v0.2.0 scientific boundary, 12-DGP/24-version and 8-estimator inventories, structured runner result, canonical score collection, honest Airlock and experimental-parallel claims, installation, and version-derived citation workflow. Corrected affected roxygen and regenerated README/help/namespace artifacts. Claude returned **APPROVE WITH NON-BLOCKING NOTES**, explicitly adjudicating the nine DGP-version exports as the correct synchronization of authoritative `@export` declarations. |
| CS-1224 | Defined the packet lifecycle vocabulary, normalized both active packets, reclassified the closed Wave 1 packet only in the authority index, narrowed the v0.2.0 roadmap boundary, and routed persistence, real-data/feature-roster/extension risks, families/CATE planning, and the full documentation release. Claude returned **APPROVE WITH NON-BLOCKING NOTES** and confirmed the closed Wave 1 packet remains byte-identical. |
| CS-1225 | Fresh post-correction Windows and WSL/Ubuntu gates passed on 2026-07-24: focused/full tests, strict validation, lint, coverage, substrate, and platform-local `R CMD check` evidence are recorded below. Claude's independent re-review returned **APPROVE WITH NON-BLOCKING NOTES**, confirmed the corrected host-context WSL evidence, closed the sole prior blocker, and found no executable rerun necessary. Ubuntu 20.04 remains useful early-warning evidence while remote Ubuntu CI remains mandatory. |

## Open Release Blocker

| Ticket | Evidence | Required resolution |
| --- | --- | --- |
| CS-1228 | Branch CI at `13bd7a2` failed before package-owned checks because archived `stringfish` 0.17.0 does not compile on R 4.6. The narrow `56376a6` trial made `stringfish` compile but all three workflows then failed because archived `qs` 0.27.3 itself calls R internals removed from the R 4.6 headers. Claude's independent specification review returned **APPROVE WITH NON-BLOCKING NOTES** and authorized implementation. The completed implementation removes `qs`, uses one atomic RDS boundary, validates staged identities, refuses legacy staging, and never opens legacy oracle caches. The first implementation review returned **REQUEST CHANGES** for one stale test-only `skip_if_not_installed("qs")` guard; that guard was removed and the focused plus affected persistence suites passed without `qs`. Claude's independent re-review returned **APPROVE**, closed the finding, and found no remaining executable `qs` path. | Record a fresh final-tree Windows/WSL gate plus green R release/devel remote CI. |

## Deferred

| Ticket/Finding | Rationale | Destination |
| --- | --- | --- |
| CATE execution and parameterized families | Both need a deeper scientific planning session; families are the more immediate scientific need, while a bounded parallel CATE track may clarify unit-level contracts. | v0.3.0 planning gate; version/scope not yet authorized |
| Real-data DGPs, feature roster, and synthetic-extension naming | No real-data rows or public DGP-registration API exist. A runner-supplied feature roster must precede real-data support; synthetic naming relaxation may require a constitutional amendment. | `horizon.md`; future real-data/extension RFC |
| Legacy `.qs` conversion, optional codecs, and generalized persistence | The current pre-release corpus can be rerun cheaply; adding recovery machinery or another dependency would expand a release-blocking encoding correction into a storage platform. | `horizon.md`; any recovery utility remains outside CausalStress under separate authority |
| Full documentation program | Per-DGP reports, pkgdown, canonical workflow, and user-defined estimator/DGP contract vignettes deserve a dedicated release after their target surfaces are settled. | Separately versioned documentation release |

## Rejected

| Ticket/Finding | Rationale |
| --- | --- |
| Itemize the nine synchronized DGP-version exports in NEWS | The source already declared all versioned generators public and the generated sync changes no scientific behavior. Itemizing it as a feature would overstate a correction to stale generated artifacts; the existing release-hygiene note covers the roxygen correction at the appropriate level. |
| Treat the README installation organization as unverified | Local Git configuration resolves `origin` to `https://github.com/blechturm/CausalStress.git`, exactly matching `pak::pak("blechturm/CausalStress")`. |

## Release Gate Evidence

| Gate | Evidence | Result |
| --- | --- | --- |
| Constitution v2.0.1 ratification/application | Accepted synthesis applied verbatim; independent Claude review returned **APPROVE** on 2026-07-24. Local checks confirmed `git diff --check`, `*.md`/`*.yml` `eol=lf` attributes, exact amendment/live-clause text, unchanged v2.0.0 history and `type="real"` count, no implementation-path changes, and successful `yaml::read_yaml()` parsing of the then-current eight correction tickets. | Pass |
| Synthetic covariate validation | Focused contract/RNG tests, strict executable validation of all 24 registered versions, malformed-name rejection matrix, full package suite, and independent Claude review. | Pass |
| QST record/row identity | Focused schema-4, pin, projection, science, audit, and historical-resume tests; full package suite; and independent Claude review. | Pass |
| Emergency RDS implementation | Focused result/batch/oracle persistence tests; valid/invalid destination, corrupt/partial, duplicate, resume-plan identity, legacy-only/mixed staging, and immutable legacy-oracle-cache coverage; final-tree full package suite in 156 seconds; strict 24/24 registry validation with 153 focused expectations; lint count zero; dependency/code/CI searches found no archived `qs` execution path. A clean Windows `R CMD check` on the immediate pre-review implementation completed in 6m 21.7s with 0 errors, 0 warnings, and 0 notes; the subsequent executable edit changed the validated batch index from all-object retention to one-at-a-time reads, and the affected plus full suites were rerun. Independent implementation review then found one stale `qs` skip guard in `test-parallel-protocol.R`; removing it made the test run on clean installations. The focused test and the four-file affected RDS suite passed, and a fresh executable search found no remaining `qs` bootstrap, API call, or skip guard. Independent re-review returned **APPROVE** with no remaining findings. | Pass after independent review. Fresh final-tree Windows/WSL checks and remote R release/devel CI remain required. |
| README and roxygen truthfulness | `README.Rmd` rendered with all evaluated examples; `devtools::document()` regenerated help/namespace artifacts; focused stale-claim searches passed; final-tree Windows `R CMD check` passed examples, documentation, and the full tests with 0 errors, 0 warnings, and one environment-only time-verification NOTE. Claude independently returned **APPROVE WITH NON-BLOCKING NOTES** and accepted the namespace sync. | Pass |
| Focused tests | Fresh seven-file release selection on 2026-07-24; exact command and file list below. | Pass: 123 expectations, 0 failures. |
| Full test suite | `testthat::test_local('.', reporter='summary', stop_on_failure=TRUE)` with `NOT_CRAN=true` on 2026-07-24. | Pass in 187.4 seconds with no failures; 50 governed experimental/optional-path warnings. |
| Registry validation | `tools/ci-validation.R` on 2026-07-24. | Pass: `registry_strict_rows=24`, `validate_registry_rows=24`, `all_valid=TRUE`; 153 focused expectations passed. |
| Lint | `tools/ci-lint.R` on 2026-07-24. | Pass/blocking: `lint_count=0`; 21 reviewed internal-helper false positives filtered. |
| Coverage | `tools/ci-coverage.R` on 2026-07-24. | Pass/evidence-only: 81.64% over 3,135 measured entries. |
| Reproducibility substrate | `tools/ci-substrate.R` on 2026-07-24; exact substrate below and in ignored artifact `ci-substrate.txt`. | Pass: governed RNG and all bitwise truth payload components confirmed. |
| R CMD check - Windows | Fresh isolated build/check on 2026-07-24; exact command below. | Pass: 0 errors, 0 warnings, 1 environment-only NOTE (`unable to verify current time`). |
| R CMD check - WSL/Ubuntu | Fresh full tests, strict validation, substrate probe, and `rcmdcheck::rcmdcheck()` on the configured default `Ubuntu` WSL 2 distribution on 2026-07-24; exact commands and substrate below. | Pass: full suite and 24/24 validation passed; `R CMD check` reported 0 errors, 0 warnings, and 0 notes. Remote Ubuntu branch/tag CI remains mandatory. |
| v0.1.10 archival publication | TBD | TBD |
| Branch CI | All three workflows failed at `13bd7a2` in archived `stringfish` bootstrap and again at `56376a6` in archived `qs` compilation on R 4.6. | Fail: routed to CS-1228; package-owned gates did not begin. |
| Main/tag CI | Coordinated through CS-1214 and CS-1227 after CS-1228 closes. | TBD |
| Audit and ticket routing | Authority index lifecycle definitions, active packet headers/YAML, roadmap, horizon, and both closeouts were reconciled. All CS-1224 deferrals have named destinations. The final v0.1.10 closeout records every finding in the still-open v0.1.9 audit as ticketed, fixed, rejected with rationale, or deferred to a named destination. | Pass |
| Known constitutional violations | Audited Constitution v2.0.1, all active ticket dispositions, accepted RFC routing, release metadata, and both active closeouts on 2026-07-24. | Pass: none known open. Publication tickets remain procedural blockers, not constitutional violations. |

## CS-1225 Fresh Local Gate Detail

**Date:** 2026-07-24

**Tested baseline:** `c05be176950603aa374e0202ca70f1e5d7443b1e`

**Worktree scope during executable gates:** the Windows gates ran against the
reviewed Batch 2 baseline plus the release-scoped `DESCRIPTION` date correction
from 2026-06-17 to 2026-07-24. The WSL gates ran after the evidence-only closeout
documents were added. Subsequent edits only corrected those evidence records.
`git diff --check` passed. The final reviewed commit SHA and clean-tree check
remain publication prerequisites; no implementation file changed during
CS-1225.

### Commands and Results

| Gate | Exact command | Result |
| --- | --- | --- |
| Focused tests | `$env:NOT_CRAN='true'; & "C:\Program Files\R\R-4.5.2\bin\x64\Rscript.exe" -e "devtools::load_all('.', quiet=TRUE); files <- file.path('tests/testthat', c('test-dgp-contract.R','test-validate-dgp-registry.R','test-validate-dgp.R','test-v020-schema4-surfaces.R','test-consolidate-schema.R','test-pins-integration.R','test-v019-e2e.R')); for (f in files) { cat('\n===', f, '===\n'); testthat::test_file(f, reporter='summary', stop_on_failure=TRUE) }; cat('\nfocused_release_tests=PASS\n')"` | Pass: 123 expectations. |
| Full tests | `$env:NOT_CRAN='true'; & "C:\Program Files\R\R-4.5.2\bin\x64\Rscript.exe" -e "res <- testthat::test_local('.', reporter='summary', stop_on_failure=TRUE); cat('\nfull_test_suite=PASS\n')"` | Pass in 187.4 seconds. |
| Strict validation | `& "C:\Program Files\R\R-4.5.2\bin\x64\Rscript.exe" tools/ci-validation.R` | Pass: 24/24 registry rows valid; 153 focused expectations. |
| Lint | `& "C:\Program Files\R\R-4.5.2\bin\x64\Rscript.exe" tools/ci-lint.R` | Pass: zero remaining lint. |
| Coverage | `& "C:\Program Files\R\R-4.5.2\bin\x64\Rscript.exe" tools/ci-coverage.R` | Evidence: 81.64%, 3,135 entries. |
| Substrate | `& "C:\Program Files\R\R-4.5.2\bin\x64\Rscript.exe" tools/ci-substrate.R` | Pass; substrate recorded below. |
| Windows check | `$env:NOT_CRAN='true'; & "C:\Program Files\R\R-4.5.2\bin\x64\Rscript.exe" -e "res <- devtools::check(document = FALSE, build_args = '--no-build-vignettes', args = c('--no-manual', '--ignore-vignettes'), error_on = 'never'); print(res)"` | Pass in 338.6 seconds: 0 errors, 0 warnings, 1 environment-only clock NOTE. |
| WSL discovery | `wsl.exe --status`; `wsl.exe --list --verbose`; `wsl.exe --list --quiet` in the host context | Default distribution `Ubuntu`, default version 2; installed distributions `Ubuntu`, `Ubuntu-24.04`, and `docker-desktop`. The configured default `Ubuntu` was used for the gate. |
| WSL full tests | `wsl.exe --% -d Ubuntu --cd /mnt/c/Users/maxth/Documents/GitHub/CausalStress --exec env NOT_CRAN=true Rscript -e "testthat::test_local('.', reporter='summary', stop_on_failure=TRUE); cat('\nwsl_full_test_suite=PASS\n')"` | Pass in 199.8 seconds: no failures, 12 declared optional-package skips, and 44 governed experimental/optional-path warnings. |
| WSL strict validation | `wsl.exe -d Ubuntu --cd /mnt/c/Users/maxth/Documents/GitHub/CausalStress --exec Rscript tools/ci-validation.R` | Pass: `registry_strict_rows=24`, `validate_registry_rows=24`, `all_valid=TRUE`; 153 focused expectations passed. |
| WSL substrate | `wsl.exe -d Ubuntu --cd /mnt/c/Users/maxth/Documents/GitHub/CausalStress --exec Rscript tools/ci-substrate.R` | Pass; Linux substrate recorded below. |
| WSL check | `wsl.exe --% -d Ubuntu --cd /mnt/c/Users/maxth/Documents/GitHub/CausalStress --exec env NOT_CRAN=true _R_CHECK_FORCE_SUGGESTS_=false Rscript -e "result <- rcmdcheck::rcmdcheck(path='.', args=c('--no-manual','--ignore-vignettes'), build_args='--no-build-vignettes', error_on='never'); print(result)"` | Pass in 105.3 seconds wall time: 0 errors, 0 warnings, 0 notes. `rcmdcheck` is the installed Linux wrapper for the same underlying `R CMD check`; the `devtools` convenience package is not installed. |
| Worktree review | `git rev-parse HEAD`; `git status --short`; `git diff --check` | Baseline SHA recorded; diff was release-gate scoped and whitespace-clean. Independent re-review accepted the evidence-only commit boundary; the resulting commit SHA and clean-tree result are required before external action. |

### Windows Reproducibility Substrate

- R: `R version 4.5.2 (2025-10-31 ucrt)`.
- Platform: `x86_64-w64-mingw32` on Windows 11 x64.
- Library paths: `C:/Users/maxth/Documents/R/win-library/4.5`;
  `C:/Users/maxth/AppData/Local/R/win-library/4.5`;
  `C:/Program Files/R/R-4.5.2/library`.
- Ambient RNG: `Mersenne-Twister / Inversion / Rejection`.
- Governed generation RNG: `Mersenne-Twister / Inversion / Rounding`.
- Thread-cap variables `OMP_NUM_THREADS`, `OPENBLAS_NUM_THREADS`,
  `MKL_NUM_THREADS`, `BLAS_NUM_THREADS`, `VECLIB_MAXIMUM_THREADS`, and
  `RCPP_PARALLEL_NUM_THREADS`: unset.
- Observable numerical substrate: zlib 1.3.1, bzlib 1.0.8, xz 5.8.1,
  libdeflate 1.24, PCRE 10.46, ICU 77.1, TRE 0.8.0; the R substrate probe did
  not report a named external BLAS path on Windows.
- Bitwise probe: `include_truth_bitwise=TRUE`; `df`, `true_att`, `true_qst`,
  and `meta` all `TRUE`.

### WSL/Ubuntu Reproducibility Substrate

- Distribution: configured default `Ubuntu` under WSL 2, reporting Ubuntu
  20.04 LTS. The separately installed `Ubuntu-24.04` distribution did not have
  the package's hard R dependencies installed, so it was not used.
- R: `R version 4.5.2 (2025-10-31)`.
- Platform: `x86_64-pc-linux-gnu`.
- Library paths: `/home/max/R/x86_64-pc-linux-gnu-library/4.5`;
  `/usr/local/lib/R/site-library`; `/usr/lib/R/site-library`;
  `/usr/lib/R/library`.
- Ambient RNG: `Mersenne-Twister / Inversion / Rejection`.
- Governed generation RNG: `Mersenne-Twister / Inversion / Rounding`.
- BLAS: `/usr/lib/x86_64-linux-gnu/blas/libblas.so.3.9.0`.
- Thread-cap variables `OMP_NUM_THREADS`, `OPENBLAS_NUM_THREADS`,
  `MKL_NUM_THREADS`, `BLAS_NUM_THREADS`, `VECLIB_MAXIMUM_THREADS`, and
  `RCPP_PARALLEL_NUM_THREADS`: unset.
- Observable numerical substrate: zlib 1.2.11, bzlib 1.0.8, xz 5.2.4,
  PCRE 10.34, ICU 66.1, TRE 0.8.0, glibc 2.31, readline 8.0.
- Bitwise probe: `include_truth_bitwise=TRUE`; `df`, `true_att`, `true_qst`,
  and `meta` all `TRUE`.

### Acceptance Audit

- `DESCRIPTION` reports package version 0.2.0 and release date 2026-07-24.
- The ratified Constitution is v2.0.1; all correction implementation tickets
  CS-1220--CS-1224 are `complete_after_review`.
- CS-1228 is `complete_after_review`. Its accepted executable change invalidates
  CS-1225 as final-tree tag evidence until fresh gates pass.
- `inst/design/README.md` correctly keeps the correction and CI packets active
  until publication, and identifies the Wave 1 packet as final.
- Every finding in the open v0.1.9 deep audit is routed by the final v0.1.10
  closeout; all correction-packet deferrals have named destinations.
- The immutable annotated `refs/tags/v0.1.10` object is
  `3496e8f90ddddff1a86da4376113d82b5d7e7943` and peels to the governed commit
  `d05164a856b3e19101b989021f20dabe0b2a00a8`. It was inspected only; CS-1226
  still requires maintainer approval immediately before publication.
- No known constitutional violation is open. Remaining blockers are fresh
  final-tree gates; archival
  publication approval; remote branch/main/tag CI; CS-1214 final closeout; and
  the final v0.2.0 release decision.

## Final Release Decision

The public v0.2.0 tag remains blocked until every ticket in this packet and the
existing CI packet is complete, or explicitly deferred/rejected with a
maintainer-approved rationale that does not violate the Constitution.
