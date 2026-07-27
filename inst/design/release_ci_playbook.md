# CausalStress Release CI Playbook

**Status:** Active CI/release-gate playbook
**Origin:** Adapted from the ledgr v0.1.7.2 release CI lessons for the
CausalStress v0.2.0 CI packet.

This playbook records operational rules for CI-backed releases. It complements
`release_gate.md`: the release gate defines what must be true; this playbook
defines how to collect trustworthy CI evidence without confusing local,
branch, main, coverage, and tag signals.

## Core Lessons

- **Local Windows, local WSL/Ubuntu, branch CI, main CI, and tag CI are separate
  evidence.** A pass in one place is useful but does not prove another place is
  green.
- **Coverage is its own gate.** Coverage reruns tests under instrumentation and
  can fail differently from `R CMD check` or a normal `testthat` run.
- **Local WSL parity is useful but incomplete.** WSL catches many Ubuntu/Linux
  issues before push, but it does not perfectly match GitHub runner timing,
  filesystem behavior, package binaries, or scheduling.
- **Timeouts are not failures when progress is normal.** Release-gate commands
  need release-gate timeout budgets. If a command times out while making normal
  passing progress, rerun once with an appropriate budget and record both facts.
- **Tag pushes are release candidates, not proof.** A public release tag is not
  release-valid until the tag-triggered CI run itself is green.
- **Green base CI does not prove optional scientific estimators.** The default
  CI matrix may skip GenGC, grf, bartCause, tmle, SuperLearner, or other
  optional estimator stacks when those packages are unavailable. Treat those
  paths as separately validated unless a workflow explicitly installs and runs
  them.

## Required Release Order

1. Finish the packet or release ticket on the release branch.
2. Run local Windows gates:
   - full package tests;
   - validation suite;
   - build the source package with its declared vignette builder and run
     `R CMD check --no-manual` on that tarball, without
     `--no-build-vignettes` or `--ignore-vignettes`;
   - coverage when coverage behavior changed or coverage is release evidence;
   - the complete pkgdown and DGP-report build when documentation work is in
     scope.
3. Run the local WSL/Ubuntu gate for any change touching executable R code,
   CI, file paths, time-sensitive behavior, test infrastructure, coverage, or
   release-gate logic.
4. Push the branch and wait for branch CI.
5. Merge only after branch CI is green.
6. Wait for main/default-branch CI to be green.
7. Create or move the release tag only after main/default-branch CI is green.
8. Wait for the tag-triggered CI. The tag is not release-valid until its own CI
   run is green.
9. Create or update the GitHub Release entry only after tag CI is green.
10. If Pages publication is authorized, manually dispatch the site workflow at
    the exact validated release tag with `deploy_pages=true`, wait for the
    `github-pages` environment deployment to succeed, and verify the public
    URL. A branch, main, pull-request, or automatic tag build remains a preview
    and must never deploy.

## Local WSL/Ubuntu Gate

When WSL is available on the development machine, run a Linux rehearsal before
pushing CI-sensitive changes. For CausalStress, the minimum WSL gate is:

```sh
Rscript -e "devtools::test(reporter = 'summary')"
Rscript -e "devtools::load_all(quiet=TRUE); strict <- cs_validate_dgp_registry(strict=TRUE); val <- cs_validate_registry(); stopifnot(nrow(strict) == nrow(val), all(val$valid))"
R CMD build .
R CMD check --no-manual CausalStress_*.tar.gz
```

For a documentation release, the build/check commands above require the
versions pinned in `DESCRIPTION` and must build the vignettes. A command using
`--no-build-vignettes` and `--ignore-vignettes` is permitted only as an optional
fast pre-check; it is never release-gate evidence.

## Governed Documentation Toolchain

Documentation-aware v0.2.1 checks use the exact versions declared in
`DESCRIPTION`: Quarto CLI 1.9.38, the `quarto` R package 1.5.1, and `pkgdown`
2.2.1. Run `Rscript tools/ci-docs.R` before local Windows and WSL documentation
gates. The check must find the declared CLI directly through `PATH` or
`QUARTO_PATH`; an undisclosed IDE fallback is not acceptable release evidence.

The complete Windows PowerShell documentation rehearsal installs the package
under test once and places that isolated library before any user library:

```powershell
$docLib = Join-Path ([System.IO.Path]::GetTempPath()) ("CausalStress-doc-lib-" + [guid]::NewGuid())
New-Item -ItemType Directory -Path $docLib | Out-Null
R CMD INSTALL --library="$docLib" .
$env:CAUSALSTRESS_DOC_LIB = $docLib
$env:R_LIBS_USER = if ($env:R_LIBS_USER) { "$docLib;$env:R_LIBS_USER" } else { $docLib }
Rscript tools/ci-docs.R
Rscript dev/render_dossiers.R --installed --validate-only
quarto render README.qmd --to gfm
git diff --exit-code -- README.md
Rscript -e "Sys.setenv(RSTUDIO_PANDOC = file.path(dirname(quarto::quarto_path()), 'tools')); pkgdown::build_site(new_process = FALSE, install = FALSE)"
Rscript dev/render_dossiers.R --installed --output-dir=docs/dgp
Rscript tools/ci-site.R
git diff --exit-code
if (git ls-files --others --exclude-standard) { throw "Documentation rehearsal created untracked source files." }
```

The WSL/Ubuntu equivalent uses the same installed-package boundary:

```sh
doc_lib="$(mktemp -d)"
R CMD INSTALL --library="$doc_lib" .
export CAUSALSTRESS_DOC_LIB="$doc_lib"
export R_LIBS_USER="$doc_lib${R_LIBS_USER:+:$R_LIBS_USER}"
Rscript tools/ci-docs.R
Rscript dev/render_dossiers.R --installed --validate-only
quarto render README.qmd --to gfm
git diff --exit-code -- README.md
Rscript -e "Sys.setenv(RSTUDIO_PANDOC = file.path(dirname(quarto::quarto_path()), 'tools')); pkgdown::build_site(new_process = FALSE, install = FALSE)"
Rscript dev/render_dossiers.R --installed --output-dir=docs/dgp
Rscript tools/ci-site.R
git diff --exit-code
test -z "$(git ls-files --others --exclude-standard)"
```

The final source diff must remain unchanged after the rehearsal. Generated
`docs/`, Quarto working files, and dossier caches are ignored preview artifacts,
not publication authority.

## GitHub Pages Publication

The repository Pages source must be **GitHub Actions**, and the
`github-pages` environment should allow selected tags matching `v*` rather than
ordinary branches. The
`pkgdown-site.yaml` workflow builds and uploads ordinary preview artifacts on
branch, pull-request, main, tag, and non-deploying manual runs. It packages and
deploys a Pages artifact only when a maintainer manually dispatches the
workflow with `deploy_pages=true` against a `vX.Y.Z` tag that exactly matches
the package version in `DESCRIPTION`.

Run the deployment only after the ordinary tag-triggered CI matrix is green and
the maintainer has accepted the final release evidence:

```sh
gh workflow run pkgdown-site.yaml --ref refs/tags/vX.Y.Z -f deploy_pages=true
gh run list --workflow pkgdown-site.yaml --limit 5
```

Use the fully qualified tag ref. A merged release branch may have the same
short name as its tag, and GitHub can resolve a short workflow-dispatch ref to
the branch. Before trusting the run, verify its `headSha` equals the validated
tag commit and that the explicit tag-validation step passed. The workflow must
remain fail-closed when `github.ref_type` is not `tag`.

The deployment job uses the `github-pages` environment, serializes deployments,
and holds only the documented `pages: write` and `id-token: write` permissions.
Its successful `page_url` must be recorded in the release closeout and opened
after deployment. A successful preview artifact, manual build with
`deploy_pages=false`, or skipped deployment job is not publication evidence.

If coverage or lint behavior changed, also run the closest local equivalent:

```sh
Rscript -e "covr::package_coverage()"
Rscript -e "lintr::lint_package()"
```

The WSL gate is early warning only. It does not replace branch CI, main CI, or
tag-triggered CI. A WSL pass is evidence; it is not a release certificate.

## Release-Gate Ticket Requirements

Every final CI/release-gate ticket must name this playbook in its source
references and require:

- local Windows gate evidence;
- local WSL/Ubuntu gate evidence or an explicit reason it was skipped;
- branch CI status;
- main/default-branch CI status;
- tag-triggered CI status before the release is marked valid;
- coverage evidence;
- lint evidence;
- reproducibility-substrate evidence;
- exact commands, run URLs, commit SHA, skipped gates, and reruns.

Do not leave this as implicit convention. If a release-gate ticket does not name
this playbook and the exact gates, update the ticket before starting the gate.

## CI Failure Triage

Remote CI logs define the first scope. Before editing:

1. Fetch or open the failed job log.
2. Record the first package stack frame and the smallest owning file.
3. Write a one-sentence hypothesis: "The failure is caused by X because Y."
4. Reproduce narrowly first:
   - the exact failing test file;
   - the exact failing test under coverage if coverage failed;
   - related subsystem tests;
   - then broader gates.
5. Keep release-gate fixes small. If a fix expands beyond the initially failing
   subsystem or more than three production files, stop and ask for review.

Do not weaken tests just because Ubuntu, macOS, Windows, or coverage exposed a
real issue. If the assertion is about a CausalStress contract, fix the contract
path or deliberately route the design issue.

## Coverage Triage

Coverage failures are not interchangeable with package-check failures. If a
coverage-only failure happens after normal tests and package check pass:

1. Rerun the failed coverage job once and record the evidence.
2. If the rerun fails too, treat it as a release blocker.
3. Do not lower coverage thresholds or hide instrumentation failures to pass a
   release.

## What Counts as Green

A public CausalStress release tag is ready only when all of the following are
true:

- local Windows gates passed;
- local WSL/Ubuntu gate passed when applicable;
- `inst/design/README.md` points to the correct active/completed packet state;
- branch CI is green;
- main/default-branch CI is green;
- tag-triggered CI is green;
- coverage and lint evidence are present;
- no failed CI run remains unexplained as a real package failure.
