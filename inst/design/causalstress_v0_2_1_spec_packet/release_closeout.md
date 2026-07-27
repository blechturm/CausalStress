# CausalStress v0.2.1 Release Closeout

**Status:** OPEN — second branch-CI correction under validation
**Date closed:** TBD
**Release candidate commit:** TBD

This closeout is an active evidence record, not release authorization. The
initial branch push below predates the v0.2.1 version/date/NEWS update and final
local gates; none of its results are represented as final-tree release evidence.

## Shipped

| Ticket | Evidence |
| --- | --- |
| CS-1230--CS-1244 | Complete after their recorded independent reviews; final release-gate acceptance audit remains pending. |
| CS-1245 | Open. The Pages mechanism passed independent review, and remote setup is verified in `v0_2_1_tickets.md` under "Remote Pages-configuration evidence." Two initial branch-CI corrections passed independent review; fresh CI is pending. |

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

## Deferred

| Ticket/Finding | Rationale | Destination |
| --- | --- | --- |
| Final CS-1245 release gates | Version/date/NEWS and final-tree local/remote gates have not started. | CS-1245 after correction review and fresh branch CI |

## Rejected

| Ticket/Finding | Rationale |
| --- | --- |
| Treat the initial green signals as final release evidence | Rejected because the tested tree still reports version 0.2.0 and predates the final CS-1245 gate. |

## Release Gate Evidence

| Gate | Evidence | Result |
| --- | --- | --- |
| R CMD check | Initial preflight recorded above; final Windows/WSL and branch/main/tag evidence pending | Pending |
| Validation suite | Initial branch workflow did not trigger; correction awaiting review | Pending |
| Full test suite | Final local and remote evidence pending | Pending |
| Documentation and site | Initial pkgdown preview passed; final pinned Windows/WSL and remote evidence pending | Pending |
| Coverage and lint | Initial branch signal passed; final-tree signal pending | Pending |
| Acceptance criteria | Final packet-wide audit pending | Pending |
| Audit routing | Final open-audit routing check pending | Pending |
| README planning state | Active packet state is current; final closeout update pending | Pending |
| Known constitutional violations | Final check pending; none introduced by this workflow-only correction | Pending |
| Reproducibility substrate | Required branch signal missing on initial push; final local/remote evidence pending | Pending |
