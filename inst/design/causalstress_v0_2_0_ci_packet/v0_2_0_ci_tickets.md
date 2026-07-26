# CausalStress v0.2.0 CI Packet Tickets

**Status:** FINAL
**Packet:** `causalstress_v0_2_0_ci_packet`

## Batch 0 - CI Foundation

### CS-1210 - Add release CI playbook and GitHub Actions R CMD check matrix

- **Batch:** 0
- **Source:** `roadmap.md` Phase 1; v0.2.0 Wave 1 release-gate review M2;
  ledgr `release_ci_playbook.md`
- **Motivation:** The public v0.2.0 tag is blocked until green CI exists.
- **Files:** `inst/design/release_ci_playbook.md`, `.github/workflows/`,
  `.Rbuildignore`, CI support files as needed
- **Constitutional check:** Article VII continuous enforcement; Article II
  cross-substrate evidence.
- **Test obligation:** `release_ci_playbook.md` is indexed and cited by the
  release-gate ticket; CI runs `R CMD check` with no errors or warnings on
  Ubuntu release, Ubuntu devel, Windows release, and macOS release; any notes
  are classified in closeout.
- **Review gate:** Batch 0 CI matrix review.
- **Disposition:** complete_after_review

### CS-1211 - Add full test, validation, and substrate evidence jobs

- **Batch:** 0
- **Source:** `inst/design/release_gate.md`; `roadmap.md` Phase 1
- **Motivation:** CI must enforce the same tests and validation evidence as the
  local release gate, not merely install the package.
- **Files:** `.github/workflows/`, `tools/ci-validation.R`,
  `tools/ci-substrate.R`, test/validation scripts as needed
- **Constitutional check:** Articles II, III, V, VI, and VII.
- **Test obligation:** CI runs full testthat with `NOT_CRAN=true`, runs the
  registry/DGP validation suite, records registry counts and `all_valid=TRUE`,
  records substrate evidence, proves `include_truth_bitwise=TRUE`, and records a
  local WSL/Ubuntu rehearsal or an explicit maintainer-approved skip.
- **Review gate:** Batch 0 test/validation/substrate review.
- **Disposition:** complete_after_review

## Batch 1 - Coverage, Lint, and CI Robustness

### CS-1212 - Add coverage and lint jobs

- **Batch:** 1
- **Source:** `roadmap.md` Phase 1
- **Motivation:** The roadmap requires coverage tests and lint as part of the
  continuous-enforcement layer.
- **Files:** `.github/workflows/`, `tools/ci-coverage.R`, `tools/ci-lint.R`,
  lint configuration as needed
- **Constitutional check:** Article VII continuous enforcement.
- **Test obligation:** CI produces coverage evidence and lint evidence. Any lint
  baseline, exclusions, or non-blocking mode must be explicit in the closeout and
  must not hide new lint regressions.
- **Review gate:** Batch 1 coverage/lint review.
- **Disposition:** complete_after_review

### CS-1213 - Harden golden-value tests for CI substrates

- **Batch:** 1
- **Source:** v0.2.0 Batch 3/4 review note routed to `horizon.md`
- **Motivation:** Absolute DGP/model golden values captured on one substrate can
  flake on Ubuntu/macOS/Windows CI even when the code is constitutionally
  compliant.
- **Files:** `tests/testthat/`, CI workflows as needed
- **Constitutional check:** Article II same-substrate bitwise identity and
  cross-substrate tolerance-level reproducibility.
- **Test obligation:** Exact assertions remain only for in-process
  schema-preservation identities; generated DGP/model absolute references either
  use documented tolerances or are explicitly gated to a declared reference
  substrate. CI confirms the tests pass on the declared matrix.
- **Review gate:** Batch 1 CI tolerance review.
- **Disposition:** complete_after_review

## Batch 2 - CI Release Gate

### CS-1214 - Run CI release gate and unblock v0.2.0 tag

- **Batch:** 2
- **Source:** `inst/design/release_gate.md`; `inst/design/release_ci_playbook.md`;
  v0.2.0 Wave 1 release closeout
- **Motivation:** The public v0.2.0 tag remains blocked until CI evidence is
  recorded and accepted.
- **Files:** `inst/design/README.md`,
  `inst/design/causalstress_v0_2_0_ci_packet/release_closeout.md`,
  release surfaces as needed
- **Constitutional check:** No known constitutional violation remains open except
  by constitutional amendment or explicit deferral outside the packet.
- **Test obligation:** Record local Windows and WSL evidence, branch/main/tag CI
  run URLs/status, matrix results, R CMD check, full tests, validation, coverage,
  lint, substrate evidence, review routing, and final tag decision in
  `release_closeout.md`.
- **Review gate:** Final CI release-gate review before public v0.2.0 tag.
- **Disposition:** complete_after_review
