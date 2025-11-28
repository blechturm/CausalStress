# CausalStress MVP Code Audit

Generated: 2025-11-28 19:38:43

## 1. Exported functions

| Function | File | Title |
|---------|------|-------|
| cs_set_rng | cs-rng.R | @export |
| cs_true_att | cs-truth.R | @export |
| cs_true_qst | cs-truth.R | @export |

## 2. Internal helpers

| Function | File | Title |
|---------|------|-------|
| cs_chk_scalar_numeric | cs-contracts.R | @noRd |
| cs_check_dgp_synthetic | cs-contracts.R | @noRd |
| cs_check_estimator_output | cs-contracts.R | @noRd |
| dgp_synth_baseline | dgp-synth-baseline.R | @export |

## 3. Testthat files

- test-dgp-baseline.R
- test-dgp-contract.R
- test-estimator-contract.R
- test-reproducibility.R
- test-runner-placebo.R
- test-truth-helpers.R
