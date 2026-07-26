# CausalStress v0.2.0 CI Packet Closeout

**Status:** ACTIVE
**Gate state:** CS-1228 and CS-1229 are complete after independent review;
the CS-1229-inclusive committed tree at `badeac1` passes the fresh Windows and
WSL/Ubuntu local gate; remote branch/main/tag CI remains pending
**Date opened:** 2026-06-18
**Packet:** `causalstress_v0_2_0_ci_packet`

This closeout records CI-packet evidence for the public v0.2.0 tag. The results
under **Prior Local Gate Evidence** are the 2026-06-18 pre-correction rehearsal.
They remain useful implementation evidence, but the CS-1225 Windows and
WSL/Ubuntu results cover only the pre-CS-1228 tree. Two later branch-CI attempts
exposed an R 4.6 runtime-installation blocker before package-owned checks began.
CS-1228 and the later CS-1229 documentation correction now have fresh
committed-tree local evidence below. Remote branch, main/default-branch, and
tag-triggered GitHub Actions evidence remains pending.

## Shipped

| Ticket | Evidence |
| --- | --- |
| CS-1210--CS-1211 | CI foundation committed in `b5e0d71`; Batch 0 review verified the release CI playbook, check matrix, test/validation/substrate jobs, README indexing, WSL rehearsal obligation, and packet activation. |
| CS-1212--CS-1213 | Coverage/lint and golden-tolerance hardening committed in `da21dcc`; Batch 1 review found no blocking defects after local lint execution and verified coverage evidence, strict lint gating, and cross-substrate-safe golden tolerances. |

## Implemented Awaiting Review

| Ticket | Evidence |
| --- | --- |
| CS-1214 | The June local release-gate implementation added dependency bootstrap, workflow hardening, pin compatibility, timeout portability, and Windows/WSL rehearsal evidence. CS-1225 supplies accepted historical local evidence for baseline `c05be176950603aa374e0202ca70f1e5d7443b1e`. Later branch CI proved the archived dependency bootstrap is not R 4.6-compatible. CS-1228 replaced that runtime; CS-1229 corrected the release-facing heavytail interpretation. Both are independently accepted, and the CS-1229-inclusive commit `badeac1` passes the fresh Windows/WSL gate. Mandatory remote branch/main/tag CI remains open. |

## CS-1229-Inclusive Committed-Tree Local Gate (2026-07-26)

The exact commands and full evidence are recorded in the correction packet's
`release_closeout.md` under **CS-1229 Fresh Committed-Tree Local Gate**. The
source was an isolated `git archive` of
`badeac1f905f4755648c154d8cdea3fd0e8705b4`, excluding unrelated untracked audit
records and testing the exact committed package.

| Gate | Fresh result |
| --- | --- |
| Full tests - Windows | Pass in 183.1 seconds with no failures and 56 governed warnings; the parallel RDS protocol test executed. |
| Strict validation - Windows | Pass: 24/24 registry rows valid and 153 focused expectations passed. |
| Lint - Windows | Pass: `lint_count=0`; 33 reviewed internal-helper false positives filtered. |
| Coverage - Windows | Pass/evidence-only: 81.97% over 3,212 measured entries. |
| Substrate - Windows | Pass: R 4.5.2 ucrt on `x86_64-w64-mingw32`; governed RNG locked; all truth payload components bitwise-identical; thread-cap variables unset. |
| R CMD check - Windows | Pass: 0 errors, 0 warnings, 0 notes; check duration 5m 30.7s. |
| Full tests - WSL/Ubuntu | Pass on default `Ubuntu` WSL 2 in 197.2 seconds with no failures, 12 declared optional-package skips, and 50 governed warnings; the parallel RDS protocol test executed. |
| Strict validation - WSL/Ubuntu | Pass: 24/24 registry rows valid and 153 focused expectations passed. |
| Substrate - WSL/Ubuntu | Pass: Ubuntu 20.04 LTS, R 4.5.2, `x86_64-pc-linux-gnu`, governed RNG locked, all truth payload components bitwise-identical, and thread-cap variables unset. |
| R CMD check - WSL/Ubuntu | Pass through installed `rcmdcheck`: 0 errors, 0 warnings, 0 notes; check duration 1m 32.1s. |
| WSL lint/coverage | Not rerun because CS-1229 changed documentation and an R comment only. Windows scripts passed; remote coverage/lint CI remains mandatory. |

## CS-1228 Committed-Tree Local Gate (2026-07-24)

The exact commands and full Windows/WSL substrate are recorded in the correction
packet's `release_closeout.md` under **CS-1228 Fresh Final-Tree Local Gate
Detail**. `git status --short` was empty before and after the executable gate,
so every result below covers commit
`9c16cd2d6a8915aef808bb3c865b8224778fa227` exactly.

| Gate | Fresh result |
| --- | --- |
| Full tests - Windows | Pass in 155.1 seconds with no failures and 56 governed warnings; the parallel RDS protocol test executed. |
| Strict validation - Windows | Pass: 24/24 registry rows valid and 153 focused expectations passed. |
| Lint - Windows | Pass: `lint_count=0`; 30 reviewed internal-helper false positives filtered. |
| Coverage - Windows | Pass/evidence-only: 81.97% over 3,212 measured entries. |
| Substrate - Windows | Pass: R 4.5.2 ucrt on `x86_64-w64-mingw32`; governed RNG `Mersenne-Twister / Inversion / Rounding`; all truth payload components bitwise-identical; thread-cap variables unset. |
| R CMD check - Windows | Pass: 0 errors, 0 warnings, 0 notes; check duration 4m 16.5s. |
| Full tests - WSL/Ubuntu | Pass on default `Ubuntu` WSL 2 in 251 seconds with no failures, 12 declared optional-package skips, and 50 governed warnings; the parallel RDS protocol test executed. |
| Strict validation - WSL/Ubuntu | Pass: 24/24 registry rows valid and 153 focused expectations passed. |
| Substrate - WSL/Ubuntu | Pass: Ubuntu 20.04 LTS, R 4.5.2, `x86_64-pc-linux-gnu`, BLAS `/usr/lib/x86_64-linux-gnu/blas/libblas.so.3.9.0`, governed RNG locked, all truth payload components bitwise-identical, and thread-cap variables unset. |
| R CMD check - WSL/Ubuntu | Pass through installed `rcmdcheck`: 0 errors, 0 warnings, 0 notes; check duration 1m 31.2s. |
| WSL lint/coverage | Not rerun because CS-1228 did not change lint or coverage behavior. Windows scripts passed; remote coverage/lint CI remains mandatory. |

## Pre-CS-1228 Fresh Post-Correction Local Gate (2026-07-24)

The Windows executable gate ran against reviewed Batch 2 baseline
`c05be176950603aa374e0202ca70f1e5d7443b1e` plus the release-scoped
`DESCRIPTION` date correction to 2026-07-24. The WSL gate ran after the
evidence-only closeout documents were added. No implementation file changed.
Exact focused-test file selection, full command strings, substrate details,
acceptance audit, and worktree scope are recorded in the correction packet's
`release_closeout.md` under **CS-1225 Fresh Local Gate Detail**.

| Gate | Fresh result |
| --- | --- |
| Focused tests | Pass: seven release-critical files, 123 expectations, 0 failures. |
| Full tests | Pass: `testthat::test_local()` with `NOT_CRAN=true` in 187.4 seconds; no failures and 50 governed experimental/optional-path warnings. |
| Strict validation | Pass: `registry_strict_rows=24`, `validate_registry_rows=24`, `all_valid=TRUE`; 153 focused expectations passed. |
| Lint | Pass/blocking: `lint_count=0`; 21 reviewed internal-helper false positives filtered. |
| Coverage | Pass/evidence-only: 81.64% over 3,135 measured entries. |
| Substrate | Pass: R 4.5.2 ucrt on `x86_64-w64-mingw32`; governed RNG `Mersenne-Twister / Inversion / Rounding`; `include_truth_bitwise=TRUE` and every truth payload component TRUE; thread-cap variables unset. |
| R CMD check - Windows | Pass: 0 errors, 0 warnings, 1 environment-only NOTE (`unable to verify current time`) in 338.6 seconds. |
| Full tests - WSL/Ubuntu | Pass on the configured default `Ubuntu` WSL 2 distribution: `testthat::test_local()` completed in 199.8 seconds with no failures, 12 declared optional-package skips, and 44 governed experimental/optional-path warnings. |
| Validation - WSL/Ubuntu | Pass: `registry_strict_rows=24`, `validate_registry_rows=24`, `all_valid=TRUE`; 153 focused expectations passed. |
| Substrate - WSL/Ubuntu | Pass: Ubuntu 20.04 LTS, R 4.5.2, `x86_64-pc-linux-gnu`, BLAS `/usr/lib/x86_64-linux-gnu/blas/libblas.so.3.9.0`, governed RNG `Mersenne-Twister / Inversion / Rounding`, all truth payload components bitwise-identical, and thread-cap variables unset. |
| R CMD check - WSL/Ubuntu | Pass through installed `rcmdcheck`: 0 errors, 0 warnings, 0 notes in 105.3 seconds wall time. The exact self-contained commands and full substrate are recorded in the correction closeout. Remote Ubuntu CI remains release-blocking. |
| Worktree/acceptance audit | `git diff --check` passed; only release-gate metadata/evidence changed. Package version 0.2.0, Constitution v2.0.1, ticket/audit routing, README planning state, and absence of known constitutional violations were confirmed. Final commit SHA/clean-tree evidence remains required before tagging. |

## Branch CI Attempts (2026-07-24)

| Commit | Result | Adjudication |
| --- | --- | --- |
| `13bd7a2652b415878d8de77602398ef600e173f0` | Coverage/lint, R CMD check, and test/validation/substrate workflows all failed before package-owned checks. Archived `stringfish` 0.17.0 used R API removed in R 4.6. | Valid dependency-bootstrap failure, not flaky test evidence. A narrow transitive-pin trial was permitted to isolate the blocker. |
| `56376a6ee8c45d14c4d3303d8bcaf45ced6d6290` | `stringfish` 0.18.0 compiled, then all three workflows failed because archived `qs` 0.27.3 itself uses removed R internals. | Decisive runtime-installation blocker. Further pinning cannot repair `qs`; routed to correction ticket CS-1228 for minimal RDS retirement. |

Run evidence: first-attempt
[coverage/lint](https://github.com/blechturm/CausalStress/actions/runs/30101823789),
[R CMD check](https://github.com/blechturm/CausalStress/actions/runs/30101823795), and
[test/validation/substrate](https://github.com/blechturm/CausalStress/actions/runs/30101823807);
pin-trial
[coverage/lint](https://github.com/blechturm/CausalStress/actions/runs/30102494120),
[R CMD check](https://github.com/blechturm/CausalStress/actions/runs/30102494130), and
[test/validation/substrate](https://github.com/blechturm/CausalStress/actions/runs/30102494153).

Neither attempt supplies package test/check evidence. The accepted CS-1225
run remains historical evidence for its tested tree. The later CS-1228 and
CS-1229 committed-tree local gates are green; a fresh remote gate remains
mandatory.

## Prior Local Gate Evidence (2026-06-18)

This table is historical rehearsal evidence and must not be read as a current
tag authorization.

| Gate | Evidence | Result |
| --- | --- | --- |
| CI workflows | `.github/workflows/R-CMD-check.yaml`, `.github/workflows/test-validation-substrate.yaml`, and `.github/workflows/coverage-lint.yaml`. The workflows install archived `qs`, run hard-dependency checks, full tests, validation, substrate evidence, coverage, and lint. | Pass locally; remote CI pending. |
| Archived dependency bootstrap | `tools/ci-install-archived-qs.R` installs archived `qs 0.27.3` when absent and pins `stringfish 0.17.0` only on clean installs. Smoke checks on 2026-06-18: Windows reported `qs=0.27.3`, `stringfish=0.19.0` with no install attempt; WSL reported `qs=0.27.3`, `stringfish=0.17.0`. | Pass. |
| Pin-board compatibility | `pins 1.4.2` makes `type = "qs"` defunct and Windows `pins 1.4.1` does not support `type = "qs2"`. Pin-board writes were migrated to `type = "rds"` while raw staging/oracle `.qs` files remain unchanged. Focused pin/schema tests and full Windows/WSL tests passed after the change. | Pass. |
| R CMD check - Windows | `$env:NOT_CRAN='true'; & "C:\Program Files\R\R-4.5.2\bin\x64\Rscript.exe" -e "devtools::check(document = FALSE, build_args = '--no-build-vignettes', args = c('--no-manual', '--ignore-vignettes'), error_on = 'never')"` on 2026-06-18. | Pass: 0 errors, 0 warnings, 1 environment NOTE: `unable to verify current time`. |
| R CMD check - WSL/Ubuntu | `wsl.exe --cd /mnt/c/Users/maxth/Documents/GitHub/CausalStress --exec env NOT_CRAN=true _R_CHECK_FORCE_SUGGESTS_=false Rscript -e "rcmdcheck::rcmdcheck(args = c('--no-manual', '--ignore-vignettes'), build_args = '--no-build-vignettes', error_on = 'never', check_dir = 'check-wsl-final')"` on 2026-06-18. | Pass: 0 errors, 0 warnings, 0 notes. |
| Full tests - Windows | Covered by the Windows `R CMD check` testthat run on 2026-06-18 after the final pin/timeout changes. | Pass. |
| Full tests - WSL/Ubuntu | `wsl.exe --cd /mnt/c/Users/maxth/Documents/GitHub/CausalStress --exec R CMD INSTALL .`, then `wsl.exe --cd /mnt/c/Users/maxth/Documents/GitHub/CausalStress --exec env NOT_CRAN=true Rscript -e "testthat::test_local('.', reporter = 'summary', stop_on_failure = TRUE)"` on 2026-06-18. | Pass: expected optional-package skips and experimental-DGP warnings only. |
| Validation - Windows | `& "C:\Program Files\R\R-4.5.2\bin\x64\Rscript.exe" tools/ci-validation.R` on 2026-06-18. | Pass: `registry_strict_rows=24`, `validate_registry_rows=24`, `all_valid=TRUE`; focused validation tests passed. |
| Validation - WSL/Ubuntu | `wsl.exe --cd /mnt/c/Users/maxth/Documents/GitHub/CausalStress --exec Rscript tools/ci-validation.R` on 2026-06-18. | Pass: `registry_strict_rows=24`, `validate_registry_rows=24`, `all_valid=TRUE`; focused validation tests passed. |
| Coverage - Windows | `& "C:\Program Files\R\R-4.5.2\bin\x64\Rscript.exe" tools/ci-coverage.R` on 2026-06-18. | Pass/evidence-only: `coverage_percent=80.53`, `coverage_entries=3127`. |
| Coverage - WSL/Ubuntu | `wsl.exe --cd /mnt/c/Users/maxth/Documents/GitHub/CausalStress --exec Rscript tools/ci-coverage.R` on 2026-06-18. | Pass/evidence-only: `coverage_percent=71.42`, `coverage_entries=3127`. Lower than Windows because hard-dependency CI skips optional scientific estimators. |
| Lint - Windows | `& "C:\Program Files\R\R-4.5.2\bin\x64\Rscript.exe" tools/ci-lint.R` on 2026-06-18. | Pass/blocking: `lint_count=0`; `ignored_internal_helper_false_positives=19`. |
| Lint - WSL/Ubuntu | `wsl.exe --cd /mnt/c/Users/maxth/Documents/GitHub/CausalStress --exec Rscript tools/ci-lint.R` on 2026-06-18. | Pass/blocking: `lint_count=0`; `ignored_internal_helper_false_positives=0`. |
| Substrate - Windows | `& "C:\Program Files\R\R-4.5.2\bin\x64\Rscript.exe" tools/ci-substrate.R` on 2026-06-18. | Pass: R 4.5.2 ucrt, platform `x86_64-w64-mingw32`, ambient RNG `Mersenne-Twister / Inversion / Rejection`, governed RNG `Mersenne-Twister / Inversion / Rounding`, `include_truth_bitwise=TRUE`, `df=TRUE`, `true_att=TRUE`, `true_qst=TRUE`, `meta=TRUE`. |
| Substrate - WSL/Ubuntu | `wsl.exe --cd /mnt/c/Users/maxth/Documents/GitHub/CausalStress --exec Rscript tools/ci-substrate.R` on 2026-06-18. | Pass: R 4.5.2, platform `x86_64-pc-linux-gnu`, BLAS `/usr/lib/x86_64-linux-gnu/blas/libblas.so.3.9.0`, ambient RNG `Mersenne-Twister / Inversion / Rejection`, governed RNG `Mersenne-Twister / Inversion / Rounding`, `include_truth_bitwise=TRUE`, all bitwise components TRUE. |
| Branch CI | Not yet available on 2026-06-18. Superseded by the failed 2026-07-24 attempts recorded above. | Historical pending state. |
| Main/default-branch CI | Not yet available. Must be recorded after merge to the default branch, if the release process merges before tagging. | Pending. |
| Tag-triggered CI | Not yet available. The public `v0.2.0` tag must not be treated as released until the tag-triggered CI run is green. | Pending. |

## CI Semantics

- Lint is blocking: `tools/ci-lint.R` exits non-zero on any remaining lint after
  reviewed exclusions and internal-helper false-positive filtering.
- Coverage is evidence-only in this packet. No threshold is enforced. The WSL/CI
  coverage value is expected to be lower than the Windows/local value because
  hard-dependency CI honestly skips optional scientific estimator tests.
- `inst/dgp_meta` is excluded from lint because it contains optional R Markdown
  evidence documents that may require packages outside the hard CI dependency
  set. Package and test code remain linted.
- The June WSL rehearsal installed the package before `testthat::test_local()`
  so future worker processes could attach `CausalStress`. The fresh CS-1225
  direct suite passed, and its subsequent `R CMD check` separately built,
  installed, and tested the package under the GitHub-Actions-like model.
- Runner timeout handling now performs a post-estimator elapsed-time check in
  addition to `setTimeLimit()`, because Linux/WSL may not interrupt blocking
  calls such as `Sys.sleep()` at exactly the elapsed-time boundary.

## Tag Decision

The public v0.2.0 tag remains blocked. Unblock only after:

1. The immutable v0.1.10 archival/pre-CI release is publicly recorded without
   moving its tag. **Done:** `https://github.com/blechturm/CausalStress/releases/tag/v0.1.10`.
2. Correction tickets CS-1223 and CS-1224 are independently accepted. **Done.**
3. CS-1225 records a fresh post-correction local gate and CS-1214 receives its
   final review. **The latest CS-1229-inclusive Windows and WSL/Ubuntu local gate
   passes; CS-1214 remains open for remote evidence and final review.**
4. CS-1228's amended specification is independently accepted. **Done.** Its
   minimal RDS migration is implemented, independently accepted, committed, and
   passes fresh final-tree Windows/WSL gates. **Done.**
5. CS-1229 is independently accepted, committed, and passes a fresh
   CS-1229-inclusive Windows/WSL gate. **Done at `badeac1`.**
6. The reviewed correction and CI changes are committed and pushed. **Committed
   locally; push pending.**
7. Branch CI is green on `v0.2.0`, including R release and R-devel installation.
8. Main/default-branch CI is green if the release is merged before tagging.
9. The tag-triggered `v0.2.0` CI run is green.

Until then, the June local results are rehearsal evidence and v0.2.0 is not
publicly released.
