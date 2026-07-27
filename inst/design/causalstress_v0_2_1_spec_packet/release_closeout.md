# CausalStress v0.2.1 Release Closeout

**Status:** FINAL
**Gate state:** All local, branch, pull-request, main, and tag gates passed in
playbook order; the GitHub Release and exact-tag GitHub Pages site are public.
**Date closed:** 2026-07-27
**Release candidate commit:** `9031c0d3353be7d9b3462695ff7749f04ca541b4`
**Released commit:** `ea4885e7ebc2ce3a226d0d8dc6be068b307c946b`
**Annotated tag object:** `638d45239e86cc1fb66e12176a225f6d9ad2ca94`

This closeout is the final evidence record for CausalStress v0.2.1. Earlier
preflight and failed attempts are retained below because they explain the
release-gate corrections; only the explicitly named final branch, main, tag,
release, and Pages evidence authorized publication.

## Shipped

| Ticket | Evidence |
| --- | --- |
| CS-1230--CS-1244 | Complete after their recorded independent reviews and included in the released tree. |
| CS-1245 | Complete after final maintainer review and release decision. All governed gates and publication steps passed in order; the maintainer explicitly waived another Claude review. |

## Initial Branch-CI Preflight — 2026-07-27

Branch `v0.2.1` was first pushed at
`e42d216e4ce769d4593a74843434e192f4f26ab3`. This was an early implementation
probe, not the final release-branch CI gate.

| Signal | Evidence | Result |
| --- | --- | --- |
| Coverage/lint | [run 30292862046](https://github.com/blechturm/CausalStress/actions/runs/30292862046) | Pass |
| Pkgdown preview and runtime-without-documentation-tooling | [run 30292862146](https://github.com/blechturm/CausalStress/actions/runs/30292862146) | Pass; no Pages deployment requested or permitted by the push event |
| R CMD check — Ubuntu R release | [job 90066578717](https://github.com/blechturm/CausalStress/actions/runs/30292862144/job/90066578717) | Pass |
| R CMD check — Ubuntu R-devel | [job 90066578546](https://github.com/blechturm/CausalStress/actions/runs/30292862144/job/90066578546) | Pass |
| R CMD check — Windows R release | [job 90066578503](https://github.com/blechturm/CausalStress/actions/runs/30292862144/job/90066578503) | Fail before package check: automatic Pandoc detection tried to resolve optional non-CRAN Suggests package `GenGC` |
| R CMD check — macOS R release | [job 90066578451](https://github.com/blechturm/CausalStress/actions/runs/30292862144/job/90066578451) | Fail before package check: same automatic Pandoc-detection dependency solve |
| Test/validation/substrate | No run exists for this SHA because the workflow trigger omitted `v0.2.1` | Missing required branch signal |

### Correction hypothesis and scope

1. `setup-r-dependencies` had already installed the declared hard dependencies
   and explicitly requested check/documentation packages. On Windows and macOS,
   its later automatic Pandoc check called `pak::pkg_deps()` with all dependency
   types and failed on optional `GenGC`. Set `install-pandoc: false` while
   retaining the pinned Quarto installation and `tools/ci-docs.R` verification.
2. Add `v0.2.1` to the existing `test-validation-substrate` push branches so
   the active release branch receives its required test, validation, and
   reproducibility-substrate matrix.

The executable correction changes two workflow configuration lines only. It does not change
`DESCRIPTION`, package code, tests, estimators, DGPs, truth, RNG, fingerprints,
or documentation contents. Independent review and a fresh push are required to
test the hypothesis on Windows, macOS, Ubuntu release, and Ubuntu R-devel.

## Targeted Correction Validation — 2026-07-27

| Check | Evidence | Result |
| --- | --- | --- |
| Upstream action-input contract | Read the official `r-lib/actions` v2 `setup-r-dependencies/action.yaml`: `install-pandoc` controls the automatic Pandoc detection/install path independently; `install-quarto: true` still always installs Quarto, and `quarto-version` supplies its version. | Pass; the proposed input combination is supported |
| Workflow and ticket YAML | Parsed both changed workflow files and `tickets.yml` with R `yaml`; asserted `install-pandoc == FALSE`, `install-quarto == TRUE`, Quarto CLI `1.9.38`, and the presence of the `v0.2.1` validation trigger. | Pass |
| Documentation substrate | PowerShell set `$env:QUARTO_PATH = "C:\\tmp\\CausalStress-quarto-1.9.38\\bin\\quarto.exe"`, then ran `Rscript tools/ci-docs.R` with Windows R 4.5.2, Quarto R 1.5.1, and pkgdown 2.2.1. The isolated CLI reported 1.9.38; its source ZIP SHA-256 `3DD3B22616DCAE65F710B1D6C019B818027312C8CBF54A0A08FDD9842346375E` matches the official release asset. | Pass |
| Dossier source validation | Same pinned environment; `Rscript dev/render_dossiers.R --validate-only`. | Pass: 12 YAML/QMD pairs, 2 stable and 10 experimental |
| Diff hygiene | `git diff --check`; diff scan under `R`, `tests/testthat`, `DESCRIPTION`, `NAMESPACE`, and `man`. | Pass; no whitespace error and no package/runtime/test/documentation-content diff |

The first documentation-substrate attempt correctly failed because the local
user library held pkgdown 2.2.0 rather than the governed 2.2.1; pkgdown 2.2.1
was installed into the Windows R user library, outside the repository. The next
attempt correctly rejected the undisclosed Positron 1.7.32 and RStudio 1.6.42
CLIs. The final pass explicitly selected the pre-existing, checksum-verified
isolated Quarto 1.9.38 executable. `actionlint` is not installed locally; YAML
semantics and remote execution remain review and fresh-CI obligations. Full
package gates were deliberately not run on this unreviewed, uncommitted
workflow-only correction.

## Independent Correction Review — 2026-07-27

The independent review returned **APPROVE WITH NON-BLOCKING NOTES**. It
confirmed the failure diagnosis, supported action-input combination, unchanged
check/vignette/Quarto/GenGC/hard-dependency boundaries, restored
test-validation-substrate trigger, CS-1245 authority, and honest closeout. It
found no additional local check necessary before commit and push because fresh
GitHub Actions execution is the definitive test of the workflow change.

The review authorizes commit and push solely for fresh branch CI. Required
signals are R-CMD-check on Ubuntu release, Ubuntu R-devel, Windows release, and
macOS release; test-validation-substrate; coverage-lint; and the non-deploying
pkgdown-site preview. It does not authorize the final release gate, merge, tag,
GitHub Release, or Pages deployment. Horizon-only commit `9586841` is part of
the new lineage but does not change the two-line executable correction.

## Fresh Correction CI and Windows Vignette Follow-up — 2026-07-27

The reviewed correction was committed as `991185f` and pushed to `v0.2.1`.
Coverage/lint, test/validation/substrate, and the non-deploying pkgdown preview
passed. R-CMD-check passed on Ubuntu R release and macOS R release. The original
automatic-Pandoc dependency-resolution failure is therefore closed.

Windows R release reached `R CMD check` but failed while `R CMD build` rebuilt
the Quarto vignettes: each Quarto child R process failed at
`library(CausalStress)` because it could not see the package that the parent
build process had staged in its temporary library. This is a Windows process-
library propagation boundary, not a vignette-content or package-runtime
failure. The Ubuntu R-devel job was still running when the Windows blocker was
diagnosed; no result is recorded here prematurely.

The narrow follow-up installs the exact checked-out revision into a temporary
Windows-only check library and prepends that library to `R_LIBS` before
`rcmdcheck`. It does not skip vignette execution, weaken check policy, alter
package sources, or change non-Windows jobs. Targeted local validation,
workflow validation, a fresh commit, and complete fresh branch CI are required
before the final CS-1245 release gate may begin. The maintainer explicitly
waived another independent Claude review for this release-gate correction.

Targeted local validation parsed the workflow YAML and executed the exact
PowerShell install/load mechanism with Windows R 4.5.2. The step resolves
`R.exe` explicitly because `R` is a PowerShell history alias, installs the
working tree into a fresh temporary library, prepends it to `R_LIBS`, and then
successfully loads CausalStress from that exact path in a `--vanilla` child R
session with the ordinary user library disabled. The child also confirmed that
the current `cs_collect_scores` export is present.

Two diagnostic full-build attempts are not release evidence. The first showed
that `R_LIBS_USER` is not a reliable boundary for the vignette child and was
discarded. The second reached and executed the current vignette code through
the package staged by `R CMD build`, but this managed sandbox denied the
vignettes' temporary `pins` paths and Quarto cache database under the user
profile. Those filesystem denials are local harness limitations, not passes;
the fresh remote Windows full-vignette check is therefore mandatory before any
release-state work resumes.

### Fresh correction result

Commit `fd3e4fcc2a234746a690eb89388ced5b42658acc` passed every required branch
preflight signal. The Windows check installed the exact revision through the
new temporary-library step and then completed the full Quarto-vignette package
build/check successfully.

| Signal | Evidence | Result |
| --- | --- | --- |
| R CMD check — Ubuntu R-devel | [job 90103627106](https://github.com/blechturm/CausalStress/actions/runs/30303998160/job/90103627106) | Pass |
| R CMD check — Ubuntu R release | [job 90103627223](https://github.com/blechturm/CausalStress/actions/runs/30303998160/job/90103627223) | Pass |
| R CMD check — Windows R release | [job 90103627187](https://github.com/blechturm/CausalStress/actions/runs/30303998160/job/90103627187) | Pass, including exact-revision install and full vignette build/check |
| R CMD check — macOS R release | [job 90103627260](https://github.com/blechturm/CausalStress/actions/runs/30303998160/job/90103627260) | Pass |
| Test/validation/substrate | [run 30303998024](https://github.com/blechturm/CausalStress/actions/runs/30303998024) | Pass |
| Coverage/lint | [run 30303997892](https://github.com/blechturm/CausalStress/actions/runs/30303997892) | Pass |
| Pkgdown preview/runtime smoke | [run 30303997952](https://github.com/blechturm/CausalStress/actions/runs/30303997952) | Pass; no deployment requested or permitted |

These results close the branch-CI correction loop but remain preflight evidence:
they precede the v0.2.1 version/date/NEWS update. The final-tree local Windows
and WSL gates, final release-candidate branch CI, main CI, tag CI, GitHub
Release, and Pages deployment remain required in playbook order.

## Final-tree Windows Gate — 2026-07-27

The candidate reports version `0.2.1` and date `2026-07-27`. Windows used R
4.5.2 (`x86_64-w64-mingw32`) and the isolated installation
`C:/Users/maxth/AppData/Local/Temp/CausalStress-v021-gate-lib-c3f94da0293c4db88b09eba5d2e98236/CausalStress`.
Documentation used Quarto CLI 1.9.38, `quarto` R 1.5.1, and pkgdown 2.2.1.

| Gate | Command/evidence | Result |
| --- | --- | --- |
| Exact candidate install | `R CMD INSTALL --library=<gate-lib> .` | Pass; installed CausalStress 0.2.1 for worker and documentation child processes |
| Full tests | `testthat::test_local('.', reporter='summary', stop_on_failure=TRUE)` | Pass in 238.2-second combined core gate; 0 failures, with expected optional skips and governed warnings |
| Validation | `Rscript tools/ci-validation.R` | Pass; strict registry and negative certification tests completed with 0 failures |
| Reproducibility substrate | `Rscript tools/ci-substrate.R` | Pass; ambient RNG `Mersenne-Twister/Inversion/Rejection`, governed generation `Mersenne-Twister/Inversion/Rounding`, `include_truth` and `df`/`true_att`/`true_qst`/`meta` bitwise checks all `TRUE`; thread-cap variables unset |
| Source build and check | Pinned `Rscript tools/ci-docs.R`; `R CMD build <source>`; `R CMD check --no-manual CausalStress_0.2.1.tar.gz` | Pass in 335 seconds at `C:/Users/maxth/AppData/Local/Temp/CausalStress-v021-build-4e45c54c5ec44167b0f8e9eafe5cefaf`; all seven Quarto vignettes built and rebuilt; `Status: OK` |
| Documentation/site | Pinned README render, `pkgdown::build_site(new_process=FALSE, install=FALSE)`, all-12 `dev/render_dossiers.R --installed`, and `tools/ci-site.R` | Pass in 163.7 seconds; 7 articles, 12 registry-keyed reports, 121 HTML pages, no broken internal links, and no source drift after the accepted generated README version update |
| Coverage | `Rscript tools/ci-coverage.R` | Pass; 82.53%, 3,172 entries |
| Correctness lint | `Rscript tools/ci-lint.R` | Pass; 0 findings and 5 documented internal-helper false positives ignored |

Three local documentation attempts are explicitly not counted as passes. Two
full-build attempts showed that the Windows Quarto child selected the older
user-library CausalStress despite the outer isolated library variables; their
missing `cs_collect_scores` and old estimator-shape failures matched that stale
installation. A temporary R startup profile outside the repository prepended
the validated candidate library inside the Quarto child; the passing build
above used it, and the temporary profile was then deleted. The first standalone
site attempt selected a separately installed R 4.6.0 and failed on an R 4.5
binary-library mismatch. Pinning `QUARTO_R`, `R_HOME`, and `PATH` to R 4.5.2
fixed that harness issue. The next README render correctly changed six generated
estimator-version cells from 0.2.0 to 0.2.1; that mechanical release artifact
is retained, and the complete rerun then passed without further source drift.

## Final-tree WSL/Ubuntu Gate — 2026-07-27

The configured default distribution was Ubuntu 20.04 LTS on x86_64 under WSL2,
using R 4.5.2 (`x86_64-pc-linux-gnu`). Because the distribution had no local
documentation tooling, the gate installed `quarto` R 1.5.1 and pkgdown 2.2.1
into disposable library `/tmp/tmp.ZFFdds4ort` and unpacked Quarto CLI 1.9.38
into a separate temporary directory. The CLI archive SHA-256 was
`ea8c897368791ad9f200010c087ea3111b2e556b12a960487dd4e216902aa102`.
No system package or repository file was used to persist this tooling.

| Gate | Command/evidence | Result |
| --- | --- | --- |
| Exact candidate install and tool pins | Temporary `pak::pkg_install(c('quarto@1.5.1', 'pkgdown@2.2.1'))`, Quarto 1.9.38 archive, and `R CMD INSTALL --library=/tmp/tmp.ZFFdds4ort .` | Pass; the child library resolved `/tmp/tmp.ZFFdds4ort/CausalStress` and all three documentation versions exactly |
| Full tests | `testthat::test_local('.', reporter='summary', stop_on_failure=TRUE)` | Pass; 12 optional-estimator skips, 50 governed warnings, and 0 failures |
| Validation | `Rscript tools/ci-validation.R` | Pass; 24 strict registry rows matched 24 executable validation rows, all valid, and the certification/negative tests passed |
| Reproducibility substrate | `Rscript tools/ci-substrate.R` | Pass; BLAS `/usr/lib/x86_64-linux-gnu/blas/libblas.so.3.9.0`, ambient RNG `Mersenne-Twister/Inversion/Rejection`, governed generation `Mersenne-Twister/Inversion/Rounding`, all bitwise checks `TRUE`, and thread-cap variables unset |
| Source build and check | `R CMD build <repo>` then `R CMD check --no-manual CausalStress_0.2.1.tar.gz` | Pass at `/tmp/tmp.wgkb35ci5W/CausalStress.Rcheck/00check.log`; all vignette checks and rebuilds passed, `Status: OK` |
| Documentation/site | Pinned README render, pkgdown build, all-12 dossier render, and `tools/ci-site.R` | Pass; 7 articles, 12 indexed reports, 121 HTML pages, no broken internal links, and identical source diff/untracked set before and after |

The complete cold-substrate command took 810.6 seconds. The default Ubuntu
rehearsal is local early-warning evidence only; remote current-Ubuntu branch,
main, and tag CI remain mandatory.

## Acceptance, Audit Routing, and Constitutional Gate — 2026-07-27

- Machine-readable ticket inspection confirmed the exact sequence CS-1230
  through CS-1245: CS-1230--CS-1244 are `complete_after_review`, and CS-1245 is
  the sole `open` ticket while publication evidence remains incomplete.
- The v0.1.9 deep-code-review audit's historical `OPEN` header does not denote
  unrouted current work: the v0.1.10 specification's disposition table maps
  every C1--C5 and M1--M19 finding to CS-1101--CS-1117 or records the reviewed
  rejection/downgrade for M14/M17; that packet is `FINAL` in the governance
  index.
- The code-simplicity audit is fully routed by the v0.2.1 specification: F1
  characterization shipped while production unification remains in the v0.3.0
  foundation/v0.2.2 defect escape hatch; F2 and F4--F7 are complete; high-risk
  F3 remains a pre-families governance decision in the roadmap and horizon.
- Scientific pass 2 is controlled by `audit/pass2/maintainer-adjudication.md`:
  CS-1229 shipped the accepted release-facing heavytail/oracle wording in
  v0.2.0; aggregation-regime, survivorship/SEM, successor-registry, and future
  family work are explicitly routed to the families planning gate/horizon; the
  rejected atomic suppression remains rejected. The v0.2.1 maintainer decision
  preserves Registry 1.4.0 as historical/superseded rather than silently
  editing it.
- All other horizon entries are non-binding and retain explicit activation
  gates; none is implied as v0.2.1 implementation. No audit finding was found
  silently omitted from current routing.
- Acceptance assertions found exactly 12 current dossier `.qmd` sources, no
  current authored `.Rmd` in the README/vignette/dossier locations, no remaining
  definitions of the three retired helpers, exact descriptor-name/embedded-ID
  agreement for `att`, `ate`, `qst`, and `cate`, valid ticket YAML, and a clean
  `git diff --check`.
- Constitution v2.0.1, the active contract/spec boundary, test/validation
  results, audit dispositions, and release scope were inspected. No known
  constitutional violation remains open or deferred.

## Final Branch-CI Attempt and Correction

The first final-candidate branch run was bound to evidence-only head
`61c9015a30742d22a9360e7fcf5cf532960d0660`. R-CMD-check
[run 30307956641](https://github.com/blechturm/CausalStress/actions/runs/30307956641),
test/validation/substrate
[run 30307956621](https://github.com/blechturm/CausalStress/actions/runs/30307956621),
and coverage/lint
[run 30307956827](https://github.com/blechturm/CausalStress/actions/runs/30307956827)
passed. The pkgdown workflow
[run 30307956634](https://github.com/blechturm/CausalStress/actions/runs/30307956634)
failed only in `runtime-without-documentation-tooling`; the complete site-build
job passed.

The failing job requested only `"hard"` dependencies and disabled automatic
Pandoc and Quarto installation, but `setup-r-dependencies` restored a broad
fallback package-library cache that already contained the `quarto` and
`pkgdown` R packages. Package installation and its runtime smoke call
succeeded; the deliberate absence assertion correctly failed. The dependency
selection was not the defect: pak defines `"hard"` as `Depends`, `Imports`,
and `LinkingTo`, while the log showed the unrelated packages arriving through
cache restoration.

The CS-1245 correction sets `cache: false` only for this negative-control job.
The positive site-build job retains caching, and the runtime job still installs
the declared hard dependencies before installing, loading, and exercising the
package. Disabling restoration is required for the assertion to test an
actually clean documentation-tooling boundary. This changes no package code,
dependency declaration, scientific behavior, or release authority. Fresh
complete branch CI is required before merge.

## Final Remote Release Evidence

| Boundary | Exact ref | Workflow evidence | Result |
| --- | --- | --- | --- |
| Corrected release branch | `61bdf0332dd56ce737dc1c1b6bebbf7b2b71ad4f` | [R CMD check](https://github.com/blechturm/CausalStress/actions/runs/30309108768), [test/validation/substrate](https://github.com/blechturm/CausalStress/actions/runs/30309108753), [coverage/lint](https://github.com/blechturm/CausalStress/actions/runs/30309108802), [pkgdown/runtime control](https://github.com/blechturm/CausalStress/actions/runs/30309108769) | All success; the corrected documentation-free runtime control and complete 121-page site build passed. |
| Pull request | [PR #8](https://github.com/blechturm/CausalStress/pull/8), head `61bdf03` | [R CMD check](https://github.com/blechturm/CausalStress/actions/runs/30309818277), [test/validation/substrate](https://github.com/blechturm/CausalStress/actions/runs/30309818269), [coverage/lint](https://github.com/blechturm/CausalStress/actions/runs/30309818303), [pkgdown/runtime control](https://github.com/blechturm/CausalStress/actions/runs/30309818289) | All success; PR was clean and mergeable before merge. |
| Main/default branch | `ea4885e7ebc2ce3a226d0d8dc6be068b307c946b` | [R CMD check](https://github.com/blechturm/CausalStress/actions/runs/30310688480), [test/validation/substrate](https://github.com/blechturm/CausalStress/actions/runs/30310688494), [coverage/lint](https://github.com/blechturm/CausalStress/actions/runs/30310688466), [pkgdown/runtime control](https://github.com/blechturm/CausalStress/actions/runs/30310688486) | All success before tag creation. |
| Annotated tag | Tag object `638d45239e86cc1fb66e12176a225f6d9ad2ca94`, peeling to `ea4885e7ebc2ce3a226d0d8dc6be068b307c946b` | [R CMD check](https://github.com/blechturm/CausalStress/actions/runs/30311589419), [test/validation/substrate](https://github.com/blechturm/CausalStress/actions/runs/30311589416), [coverage/lint](https://github.com/blechturm/CausalStress/actions/runs/30311589386), [pkgdown/runtime control](https://github.com/blechturm/CausalStress/actions/runs/30311589376) | Four distinct tag-triggered workflows succeeded before publication; the tag was not moved. |
| GitHub Release | `v0.2.1` | [CausalStress v0.2.1](https://github.com/blechturm/CausalStress/releases/tag/v0.2.1) | Public, non-draft, and non-prerelease; published 2026-07-27 at 22:53:50 UTC. |
| Rejected Pages dispatch | Short ref `v0.2.1` resolved to release-branch head `61bdf03` | [run 30312379765](https://github.com/blechturm/CausalStress/actions/runs/30312379765) | Tag-ref validation failed before dependency installation, artifact packaging, or deployment; the run was cancelled. This is expected fail-closed evidence, not a package failure. |
| Exact-tag Pages dispatch | Fully qualified `refs/tags/v0.2.1`, head `ea4885e` | [run 30312434833](https://github.com/blechturm/CausalStress/actions/runs/30312434833), deployment `5631282311` | Tag validation, complete site build, runtime-without-documentation-tooling, Pages artifact, and `github-pages` deployment all succeeded. |
| Public site | `https://blechturm.github.io/CausalStress/` | GitHub deployment state `success`; direct HTTPS request after deployment | HTTP 200; page contains the CausalStress title, v0.2.1 content, and DGP-report links. |

The ambiguous-dispatch evidence produced one process correction: the playbook
now requires `--ref refs/tags/vX.Y.Z` and verification that the run `headSha`
equals the validated tag commit. The existing workflow guard was preserved
because it prevented publication from the wrong ref exactly as designed.

## Deferred

No v0.2.1 ticket or release-gate finding remains deferred. Future capabilities
and maintenance already routed to `roadmap.md` and `horizon.md` remain outside
this final packet rather than open work within it.

## Rejected

| Ticket/Finding | Rationale |
| --- | --- |
| Treat the initial green signals as final release evidence | Rejected because the tested tree still reports version 0.2.0 and predates the final CS-1245 gate. |

## Release Gate Evidence

| Gate | Evidence | Result |
| --- | --- | --- |
| R CMD check | Final Windows and WSL source build/check plus branch, PR, main, and tag matrices passed with all Quarto vignettes | Pass |
| Validation suite | Final Windows/WSL validation plus branch, PR, main, and tag validation workflows passed | Pass |
| Full test suite | Final Windows/WSL suites plus branch, PR, main, and tag test workflows passed | Pass |
| Documentation and site | Pinned Windows/WSL rehearsals, branch/PR/main/tag previews, exact-tag Pages deployment, and public HTTP verification passed | Pass |
| Coverage and lint | Final Windows coverage 82.53% and lint 0; branch, PR, main, and tag coverage/lint workflows passed | Pass |
| Acceptance criteria | Packet-wide assertions and reviewed batch dispositions checked as recorded above | Pass |
| Audit routing | Historical deep audit, simplicity audit, scientific pass 2, and horizon destinations reconciled as recorded above | Pass |
| README planning state | Governance README and roadmap classify v0.2.1 as final/completed; generated README contains v0.2.1 example output | Pass |
| Known constitutional violations | Constitution v2.0.1, contracts, tests, and routed findings checked | Pass: none known open or deferred |
| Reproducibility substrate | Final Windows/Ubuntu evidence plus branch, PR, main, and tag substrate artifacts passed | Pass |
