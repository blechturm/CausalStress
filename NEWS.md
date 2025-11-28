# CausalStress 0.1.0

- First MVP:
  - Synthetic DGPs: `synth_baseline`, `synth_heavytail`
  - Estimators: `oracle_att`, `lm_att`, `ipw_att`, `grf_dr_att` (optional)
  - Registry system for DGPs and estimators
  - Runners: `cs_run_single()`, `cs_run_seeds()`
  - Summary: `cs_summarise_runs()`
  - Contracts + tests for DGPs, estimators, runners, truth helpers
